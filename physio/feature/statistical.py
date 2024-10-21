import numpy as np


def rms(arr):
    sq_arr = arr ** 2
    avg = np.mean(sq_arr)
    return np.sqrt(avg)


def znorm(arr):
    avg = np.mean(arr)
    std = np.std(arr)
    return (arr - avg) / std
