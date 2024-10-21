import mne
import os
import pickle
import numpy as np
from ..signal.process import filter_gsr, filter_ppg, filter_respiration
from ..feature.hrv import extract_hrv
from ..feature.gsr import extract_gsr
from ..feature.resp import extract_resp
import neurokit2 as nk
import matplotlib.pyplot as plt
import pandas as pd


def read_file(file_path):
    """
    This function is meant to take in a single file from the DEAP dataset and output necessary information
    :param file_path: The absolute path to a file including extension
    :returns:
        - ppg - The ppg data from the file
        - resp - The respiration data from the file
        - gsr - The galvanic skin response data from the file
        - fs - The sample rate of the data collection
        - status - The status vector from the file
        - times - The times vector from the file
    """

    # Read data file and extract the ppg, resp, gsr, and status
    print(f"\nFile to read: {file_path}")
    raw = mne.io.read_raw_bdf(file_path)
    fs = raw.info["sfreq"]

    try:
        partial_raw = raw.copy().pick(["Plet", "Resp", "GSR1", "Status"])
    except ValueError:
        try:
            partial_raw = raw.copy().pick(["Plet", "Resp", "GSR1", ""])
        except ValueError:
            partial_raw = raw.copy().pick(["Plet", "Resp", "GSR1", "-0"])

    data, times = partial_raw.get_data(return_times=True)

    # Save the data appropriately in variables for ease of use
    ppg = data[0]
    resp = data[1]
    gsr = data[2]
    status = data[3]

    return ppg, resp, gsr, fs, status, times

    # # status = clean_deap_status(data[1])
    # status = data[1]
    # status -= np.min(status)

    # return ppg, fs, clean_deap_status(status), times


def read_directory(directory, save_location=None):
    """
    Access the deap physiological data and saves all the raw data to a .pkl file inside of "./data/deap"
    :param directory: The absolute path to the deap data directory
    :param save_location: If set, the absolute path and file name used to save the data. The function uses Python's Pickle
        module, so the file extension should be .pkl. Defaults to None.
    :return: The data in a python dictionary
    """
    # Get all data files
    file_names = []
    for file in os.listdir(directory):
        file_names.append(file)

    # Create dictionary for data
    deap_physio_data = {}

    # Sort and iterate through all files
    file_names.sort()
    for file_name in file_names:
        abs_path = os.path.join(directory, file_name)
        ppg, resp, gsr, fs, status, times = read_file(abs_path)

        # NK2 experiments
        signals, info = nk.eda_process(gsr, sampling_rate=fs)
        cleaned = signals["EDA_Clean"]
        features = [info["SCR_Onsets"], info["SCR_Peaks"], info["SCR_Recovery"]]

        data = nk.eda_phasic(nk.standardize(gsr), sampling_rate=fs)
        data["EDA_Raw"] = nk.standardize(gsr)
        data.plot()

        deap_physio_data[file_name] = {
            "ppg": ppg,
            "resp": resp,
            "gsr": data["EDA_Phasic"],
            "fs": fs,
            "status": status,
            "times": times
        }

    # Return if None
    if save_location is None:
        return deap_physio_data

    # Save and return
    with open(save_location, "wb") as f:
        pickle.dump(deap_physio_data, f)

    return deap_physio_data


def determine_baseline_endpoints(status, times):
    # Find start and end indeces
    start = np.where(status == 1)[0][1]  # start index
    start_time = times[start]
    end_time = start_time + 120  # Baseline is 120 seconds
    end = np.where(times >= end_time)[0][0]
    return start, end


def determine_stimuli_endpoints(status, times):
    # Find start and end indeces
    starts = np.where(status == 4)[0]  # start index
    start_times = times[starts]
    end_times = start_times + 60  # stimulus is 60 seconds
    ends = np.array([]).astype(np.int32)

    # Create the array of end indeces
    for (i, end_time) in enumerate(end_times):
        if end_time <= times[-1]:  # If the end time is larger than the largest time, skip
            ends = np.append(ends, (np.where(times >= end_time)[0][0]))

    if starts.shape[0] > ends.shape[0]:  # Ensure that the starts array and the ends array are the same length
        starts = starts[:ends.shape[0]]

    # Assertion to make sure we did this right
    assert len(starts) == len(ends), "Something got messed up with the stimuli starts and ends"

    return starts, ends


def clean_status(status):
    """
    The raw DEAP status vector is messy. This cleans the vector to have accurate information
    :param status:
    :return: clean_status
    """
    # Subtract minimum value from all values (some status vectors are increased by a constant for some reason)
    status = status - np.min(status)

    # Set any number greater than 7 equal to 0
    status = np.array([0 if (val > 7) or (val == 6) else val for val in status])

    # find duplicates
    last_value = 0
    for (i, val) in enumerate(status):
        if val == 0:
            last_value = 0
        elif val == last_value:
            status[i] = 0
        else:
            last_value = val

    return status


def crop_data(physio_data, start, stop):
    for (i, val) in enumerate(physio_data):
        if val == "fs":
            continue
        physio_data[val] = physio_data[val][start:stop]

    return physio_data


def preprocess_data(raw_data=None, saved_data=None, save_location=None):
    """
    Preprocess data from DEAP. One and only one of the arguments should be passed.
    :param raw_data: A dictionary of raw data to preprocess. This dictionary must be created using the function
        "read_directory".  Defaults to None. If this is not set, the argument "save_location" has to be set.
    :param saved_data: The location on disk of a binary file created using python's pickle module. This must be an
            absolute path including the extension ".pkl". Defaults to None. If this is not set, the argument "raw_data"
            must be set.
    :param save_location: If set, the absolute path and file name used to save the data. The function uses Python's
        pickle module, so the file extension should be .pkl. Defaults to None.
    """
    # Error checking
    if raw_data is None and saved_data is None:
        raise ValueError("One argument must be set.")

    elif raw_data is not None and saved_data is not None:
        raise ValueError("Only one argument can be set.")

    if raw_data is None:
        # Get saved pickled data
        with open(saved_data, 'rb') as f:
            raw_data = pickle.load(f)

    # Iterate over all participants
    preprocessed_data = {}
    for key in raw_data.keys():
        print(f"\nProcessing {key}...")
        part_data = raw_data[key]  # Get participant
        part_data["status"] = clean_status(part_data["status"])  # clean status

        # Crop data
        start = np.where(part_data["status"] == 1)[0][0]
        stop = np.where(part_data["status"] == 7)[0][0]
        cropped_data = crop_data(part_data, start, stop)

        # Set data to individual variables
        ppg = cropped_data["ppg"]
        resp = cropped_data["resp"]
        gsr = cropped_data["gsr"]
        fs = cropped_data["fs"]
        status = cropped_data["status"]
        times = cropped_data["times"]

        # Filter the data
        filtered_ppg = filter_ppg(ppg, fs)
        filtered_resp = filter_respiration(resp, fs)  # TODO
        filtered_gsr = gsr  # since we are now using neurokit2
        # filtered_gsr = filter_gsr(gsr, fs)  # TODO

        # Find import indeces
        baseline_start, baseline_end = determine_baseline_endpoints(status, times)
        music_stimuli_starts, music_stimuli_ends = determine_stimuli_endpoints(status, times)

        # Write the dictionary
        preprocessed_data[key] = {
            "ppg": filtered_ppg,
            "resp": filtered_resp,
            "gsr": filtered_gsr,
            "fs": fs,
            "baseline_start_idx": baseline_start,
            "baseline_end_idx": baseline_end,
            "stimuli_start_idxs": music_stimuli_starts,
            "stimuli_end_idxs": music_stimuli_ends
        }

        # plt.plot(np.arange(len(filtered_gsr)), filtered_gsr, color="red")
        # plt.show()

    if save_location is not None:
        # Pickle the dictionary
        with open(save_location, "wb") as f:
            pickle.dump(preprocessed_data, f)

    return preprocessed_data


def extract_physiological_features(preprocessed_data=None, saved_data=None, save_location=None, num_participants=None):
    """
        Extract Physiological feature preprocessed DEAP data. One and only one of the arguments should be passed.
        :param preprocessed_data: A dictionary of raw data to preprocess. This dictionary must be created using the function
            "read_directory".  Defaults to None. If this is not set, the argument "save_location" has to be set.
        :param saved_data: The location on disk of a binary file created using python's pickle module. This must be an
            absolute path including the extension ".pkl". Defaults to None. If this is not set, the argument "raw_data"
            must be set.
        :param save_location: If set, the absolute path and file name used to save the data. The function uses Python's
            pickle module, so the file extension should be .pkl. Defaults to None.
        :param num_participants: If set, the number of participants to read. Maximum is 30. Defaults to None in which
            case all participants are included.
        """
    # Error checking
    if preprocessed_data is None and saved_data is None:
        raise ValueError("One argument must be set.")

    elif preprocessed_data is not None and saved_data is not None:
        raise ValueError("Only one argument can be set.")

    # Read data if necessary
    if preprocessed_data is None:
        with open(saved_data, "rb") as f:
            preprocessed_physio_data = pickle.load(f)

    # How many participants to actually read from
    part_break = ""
    num_participants += 1  # Without this, we will analyze num_participants - 1.
    if num_participants is not None:
        if num_participants < 10:
            part_break = f"s0{num_participants}.bdf"
        else:
            part_break = f"s{num_participants}.bdf"

    # Start iterating over participants
    feature_data = {}

    # Build the dataframe
    df = pd.DataFrame(columns=["participant", "stim_id",  # Housekeeping
                               "mean_ibi", "hr", "rmssd", "sdnn",  # Cardiac
                               "gsr_mean", "gsr_std", "gsr_rms", "deriv_mean", "deriv_std", "deriv_rms", "deriv2_mean",
                               "deriv2_std", "deriv2_rms",  # GSR
                               "resp_mean", "resp_std", "resp_rms", "resp_skew", "resp_kurtosis"]  # Respiration
                      )

    for key in preprocessed_physio_data.keys():
        if key == part_break:
            break

        if key == "s23.bdf" or key == "s29.bdf":
            continue

        print(f"\nExtracting from {key}...")
        part_data = preprocessed_physio_data[key]  # Get participant data

        # Get sample rate and physio data
        fs = 512
        ppg = part_data["ppg"]
        resp = part_data["resp"]
        gsr = part_data["gsr"]

        # plt.plot(np.arange(len(ppg)), ppg)
        # plt.title(f"PPG - Part {key}")
        # plt.show()
        #
        # plt.plot(np.arange(len(gsr)), gsr)
        # plt.title(f"GSR - Part {key}")
        # plt.show()

        # Get baseline indeces and truncate data
        bl_start = part_data["baseline_start_idx"]
        bl_end = part_data["baseline_end_idx"]
        bl_ppg = ppg[bl_start:bl_end]
        bl_gsr = gsr[bl_start:bl_end]
        bl_resp = resp[bl_start:bl_end]

        # Extract baseline metrics
        bl_hrv = extract_hrv(bl_ppg, fs)
        bl_resp_metrics = extract_resp(bl_resp)
        bl_gsr_metrics = extract_gsr(bl_gsr)
        baseline = {
            "hrv": bl_hrv,
            "gsr": bl_gsr_metrics,
            "resp": bl_resp_metrics
        }

        # Save to dictionary
        feature_data[key] = {
            "baseline": baseline,
        }

        housekeeping_list = [key, "baseline"]
        hrv_list = list(bl_hrv.values())
        gsr_list = list(bl_gsr_metrics.values())
        resp_list = list(bl_resp_metrics.values())
        record = housekeeping_list + hrv_list + gsr_list + resp_list
        df.loc[len(df)] = record

        # Iterate over all start times
        starts = part_data["stimuli_start_idxs"]
        ends = part_data["stimuli_end_idxs"]
        for i, start in enumerate(starts):
            # Get stimulus indeces and truncate data
            stim_start = start
            stim_end = int(ends[i])
            stim_ppg = ppg[stim_start:stim_end]
            stim_gsr = gsr[stim_start:stim_end]
            stim_resp = gsr[stim_start:stim_end]

            # Extract stimulus metrics
            stim_hrv = extract_hrv(stim_ppg, fs)
            stim_gsr_metrics = extract_gsr(stim_gsr)
            stim_resp_metrics = extract_resp(stim_resp)

            # Create the simulus dictionary
            stimuli_dictionary = {
                f"stim_{i+1}": {
                    "hrv": stim_hrv,
                    "gsr": stim_gsr_metrics,
                    "resp": stim_resp_metrics
                }
            }

            # Add to the final dictionary
            feature_data[key].update(stimuli_dictionary)

            # Add record to dataframe
            housekeeping_list = [key, f"stim_{i + 1}"]
            hrv_list = list(stim_hrv.values())
            gsr_list = list(stim_gsr_metrics.values())
            resp_list = list(stim_resp_metrics.values())
            record = housekeeping_list + hrv_list + gsr_list + resp_list
            df.loc[len(df)] = record

    if save_location is not None:
        df.to_csv(save_location)

    return feature_data
