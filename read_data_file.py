import mne
import os
import pickle


def read_deap_data(file_path):
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
    print(f"File to read: {file_path}")
    raw = mne.io.read_raw_bdf(file_path)
    fs = raw.info["sfreq"]
    print(raw.info["ch_names"])

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


def read_all_deap_physio_data():
    """
    Access the deap physiological data and saves all the raw data to a .pkl file inside of "./data/deap"
    """
    # Get all data files
    directory = "./data/deap/physiological-data/"
    file_names = []
    for file in os.listdir(directory):
        file_names.append(file)

    # Create dictionary for data
    deap_physio_data = {}

    # Sort and iterate through all files
    file_names.sort()
    for file_name in file_names:
        abs_path = os.path.join(directory, file_name)
        ppg, resp, gsr, fs, status, times = read_deap_data(abs_path)
        deap_physio_data[file_name] = {
            "ppg": ppg,
            "resp": resp,
            "gsr": gsr,
            "fs": fs,
            "status": status,
            "times": times
        }

    with open("data/deap/raw-physio-data.pkl", "wb") as f:
        pickle.dump(deap_physio_data, f)


if __name__ == "__main__":
    read_all_deap_physio_data()
