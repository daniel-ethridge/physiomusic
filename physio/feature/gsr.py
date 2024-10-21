import numpy as np
from .statistical import rms
import scipy.stats as sp
import neurokit2 as nk


def extract_phasic_component(gsr, fs):
    """
    Extract the phasic component from the GSR signal
    :param gsr: Raw GSR signal
    :param fs: sample rate
    :return: 
    """
    signals, info = nk.eda_process(gsr, fs)
    print()


def extract_gsr(gsr):

    gsr = np.array(gsr)
    first_deriv = np.zeros(gsr.shape[0] - 1)
    for (i, _) in enumerate(first_deriv):
        first_deriv[i] = gsr[i+1] - gsr[i]

    sec_deriv = np.zeros(first_deriv.shape[0] - 1)
    for (i, _) in enumerate(sec_deriv):
        sec_deriv[i] = first_deriv[i+1] - first_deriv[i]

    gsr_metrics = {
        "mean": np.mean(gsr),
        "std": np.std(gsr),
        "rms": rms(gsr),
        "deriv_mean": np.mean(first_deriv),
        "deriv_std": np.std(first_deriv),
        "deriv_rms": rms(first_deriv),
        "deriv2_mean": np.mean(sec_deriv),
        "deriv2_std": np.std(sec_deriv),
        "deriv2_rms": rms(sec_deriv),
    }
    return gsr_metrics
