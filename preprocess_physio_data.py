import numpy as np
import pickle
import heartpy as hp


def determine_deap_baseline_endpoints(status, times):
    # Find start and end indeces
    start = np.where(status == 1)[0][1]  # start index
    start_time = times[start]
    end_time = start_time + 120  # Baseline is 120 seconds
    end = np.where(times >= end_time)[0][0]
    return start, end


def determine_deap_stimuli_endpoints(status, times):
    # Find start and end indeces
    starts = np.where(status == 4)[0]  # start index
    start_times = times[starts]
    end_times = start_times + 60  # stimulus is 60 seconds
    ends = np.array([]).astype(np.int32)

    # Create the array of end indeces
    for (i, end_time) in enumerate(end_times):
        if end_time <= times[-1]:  # If the end time is larger than the largest time, skip
            ends = int(np.append(ends, np.where(times >= end_time)[0][0]))

    if starts.shape[0] > ends.shape[0]:  # Ensure that the starts array and the ends array are the same length
        starts = starts[:ends.shape[0]]

    # Assertion to make sure we did this right
    assert len(starts) == len(ends), "Something got messed up with the stimuli starts and ends"

    return starts, ends


def clean_deap_status(status):
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


def crop_deap_physio_data(physio_data, start, stop):
    for (i, val) in enumerate(physio_data):
        if val == "fs":
            continue
        physio_data[val] = physio_data[val][start:stop]

    return physio_data

# def extract_ppg_peaks(ppg, times):
# def extract_rr_intervals_from_ppg(ppg, fs):
#     cleaned_data = hp.filter_signal(ppg, (0.5, 10), filtertype="bandpass", sample_rate=fs)
#     working_data, measures = hp.process(cleaned_data, fs, clean_rr=True, bpmmin=60, bpmmax=80)
#
#     rr_intervals = working_data["RR_list_cor"]
#     peak_list = working_data["peaklist"]
#     removed_beats = working_data["removed_beats"]
#
#     removed_beat_locations = np.zeros(removed_beats.shape[0], dtype=int)
#     for (i, val) in enumerate(removed_beats):
#         removed_beat_locations[i] = np.where(peak_list == val)[0][0]
#
#     cleaned_peaks = np.delete(peak_list, removed_beat_locations)
#     return rr_intervals, cleaned_peaks


def filter_deap_ppg_data(ppg, fs, order=4, cutoff_freqs=(0.5, 10)):
    """
    Filter ppg data from DEAP
    :param ppg: The ppg data to filter
    :param fs: sample
    :param order: the order of the filter
    :param cutoff_freqs: The upper and lower bounds of a bandpass filter. Must be a 2 element tuple
    :return: The filtered data
    """
    filtered_data = hp.filter_signal(ppg, cutoff_freqs, fs, order=order, filtertype="bandpass")
    return filtered_data


def filter_deap_resp_data(resp):
    """
    TODO:
        - Find methods for filtering
        - implement

    Filter respiration data from DEAP
    :param resp: The resp data to filter
    :return: The filtered data
    """
    return resp


def filter_deap_gsr_data(gsr):
    """
    TODO:
        - Find methods for filtering
        - implement

    Filter GSR data from DEAP
    :param gsr: The GSR data to filter
    :return: The filtered data
    """
    return gsr


def preprocess_deap_physio_data():
    # Get saved pickled data
    with open("./data/deap/raw-physio-data.pkl", "rb") as f:
        raw_physio_data = pickle.load(f)

    # Iterate over all participants
    preprocessed_data = {}
    for key in raw_physio_data.keys():
        part_data = raw_physio_data[key]  # Get participant
        part_data["status"] = clean_deap_status(part_data["status"])  # clean status

        # Crop data
        start = np.where(part_data["status"] == 1)[0][0]
        stop = np.where(part_data["status"] == 7)[0][0]
        cropped_data = crop_deap_physio_data(part_data, start, stop)

        # Set data to individual variables
        ppg = cropped_data["ppg"]
        resp = cropped_data["resp"]
        gsr = cropped_data["gsr"]
        fs = cropped_data["fs"]
        status = cropped_data["status"]
        times = cropped_data["times"]

        # Filter the data
        filtered_ppg = filter_deap_ppg_data(ppg, fs)
        filtered_resp = filter_deap_resp_data(resp)  # TODO
        filtered_gsr = filter_deap_gsr_data(gsr)  # TODO

        # Find import indeces
        baseline_start, baseline_end = determine_deap_baseline_endpoints(status, times)
        music_stimuli_starts, music_stimuli_ends = determine_deap_stimuli_endpoints(status, times)

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

        # Pickle the dictionary
        with open("data/deap/preprocessed-physio-data.pkl", "wb") as f:
            pickle.dump(preprocessed_data, f)


if __name__ == "__main__":
    preprocess_deap_physio_data()
