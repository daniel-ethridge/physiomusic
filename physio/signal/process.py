import heartpy as hp
from scipy.signal import butter, filtfilt


def filter_ppg(ppg, fs, order=4, cutoff_freqs=(0.5, 10)):
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


def filter_respiration(resp, fs, cutoff=19, order=4):
    """
    TODO:
        - Find methods for filtering
        - implement
    Filter GSR data from DEAP
    :param gsr: The GSR data to filter
    :param fs: The sample rate
    :param cutoff: The cutoff frequency in hertz. Defaults to 19Hz
    :param order: The order of the filter
    :return: The filtered data
    """
    if order % 2 != 0:
        order += 1

    order /= 2
    coeff = butter(order, cutoff, fs=fs)
    filtered = filtfilt(coeff[0], coeff[1], resp)

    return filtered


def filter_gsr(gsr, fs, cutoff=19, order=4):
    """
    TODO:
        - Find methods for filtering
        - implement
    Filter GSR data from DEAP
    :param gsr: The GSR data to filter
    :param fs: The sample rate
    :param cutoff: The cutoff frequency in hertz. Defaults to 19Hz
    :param order: The order of the filter
    :return: The filtered data
    """
    if order % 2 != 0:
        order += 1

    order /= 2
    coeff = butter(order, cutoff, fs=fs)
    filtered = filtfilt(coeff[0], coeff[1], gsr)

    return filtered
