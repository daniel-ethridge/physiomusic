import heartpy as hp
import numpy as np
from .statistical import rms
import matplotlib.pyplot as plt


def extract_rr_intervals(ppg, fs):
    """
    Extract the RR intervals from a PPG reading
    :param ppg:
    :param fs:
    :return:
    """

    # Use heartpy to get RR intervals
    working_data, measures = hp.process(ppg, fs, clean_rr=True)
    # hp.plotter(working_data, measures)
    # plt.show()

    # Get certain arrays from the working data
    rr_intervals = working_data["RR_list_cor"]

    # ### DEPRECATED ###

    # peak_list = working_data["peaklist"]
    # removed_beats = working_data["removed_beats"]
    #
    # # Removed beat locations
    # removed_beat_locations = np.zeros(removed_beats.shape[0], dtype=int)
    # for (i, val) in enumerate(removed_beats):
    #     removed_beat_locations[i] = np.where(peak_list == val)[0][0]
    #
    # # Get cleaned peak indeces. Use those to create an associated time vector
    # cleaned_peaks = np.delete(peak_list, removed_beat_locations)
    # time_stamps = times[cleaned_peaks]

    # Return the rr_intervals and an associated time stamp
    return rr_intervals


def rmssd(rr_ints):
    # Calculate successive differences array
    if len(rr_ints) == 0:
        return -999

    succ_diffs_arr = np.zeros(len(rr_ints) - 1)
    for i in range(len(succ_diffs_arr)):
        succ_diffs_arr[i] = rr_ints[i + 1] - rr_ints[i]

    # calculate RMS
    return rms(succ_diffs_arr)


def extract_hrv(ppg, fs):
    """
    Return a selection of HRV metrics
    :param ppg: The array of RR intervals to use
    :param fs: Sample rate
    :return: A dictionary of HRV metrics
    """

    rr_intervals = extract_rr_intervals(ppg, fs)

    hrv_metrics = {
        "mean_ibi": np.mean(rr_intervals),
        "hr": 60000 / np.mean(rr_intervals),
        "rmssd": rmssd(rr_intervals),
        "sdnn": np.std(rr_intervals),
    }

    return hrv_metrics
