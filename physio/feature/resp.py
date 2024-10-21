import numpy as np
from .statistical import rms
import scipy.stats as sp


def extract_resp(resp):

    resp_metrics = {
        "mean": np.mean(resp),
        "std": np.std(resp),
        "rms": rms(resp),
        "skew": sp.skew(resp),
        "kurtosis": sp.kurtosis(resp)
    }

    return resp_metrics
