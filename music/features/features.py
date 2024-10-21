import librosa.feature as mir
import librosa
from librosa.onset import onset_strength
import pandas as pd
import numpy as np
from scipy.signal import butter, sosfilt
import librosa.feature


def extract_features(file, audio, fs):

    # S = librosa.feature.melspectrogram(y=audio, sr=fs, n_mels=128)
    # stds = np.zeros(128)
    # for (i, _) in enumerate(stds):
    #     row = S[i]
    #     stds[i] = np.mean(row)

    # Create empty dataframe
    features_frame = pd.DataFrame(columns=[
        # File name
        "file_name",
        # Non-MFCCs
        "spec_cent_mean",      "spec_cent_std",
        "spec_bandwidth_mean", "spec_bandwidth_std",
        "spec_contrast_mean",  "spec_contrast_std",
        "spec_flatness_mean",  "spec_flatness_std",
        "spec_rolloff_mean",   "spec_rolloff_std",
        "zero_cross_mean",     "zero_cross_std",
        "rms_mean",            "rms_std",

        # MFCCs
        "mfcc1_mean",  "mfcc1_std",
        "mfcc2_mean",  "mfcc2_std",
        "mfcc3_mean",  "mfcc3_std",
        "mfcc4_mean",  "mfcc4_std",
        "mfcc5_mean",  "mfcc5_std",
        "mfcc6_mean",  "mfcc6_std",
        "mfcc7_mean",  "mfcc7_std",
        "mfcc8_mean",  "mfcc8_std",
        "mfcc9_mean",  "mfcc9_std",
        "mfcc10_mean", "mfcc10_std",
        # "mfcc11_mean", "mfcc11_std",
        # "mfcc12_mean", "mfcc12_std",
        # "mfcc13_mean", "mfcc13_std",
        # "mfcc14_mean", "mfcc14_std",
        # "mfcc15_mean", "mfcc15_std",
        # "mfcc16_mean", "mfcc16_std",
        # "mfcc17_mean", "mfcc17_std",
        # "mfcc18_mean", "mfcc18_std",
        # "mfcc19_mean", "mfcc19_std",
        # "mfcc20_mean", "mfcc20_std",

        # "specs1_mean", "specs1_std",
        # "specs2_mean", "specs2_std",
        # "specs3_mean", "specs3_std",
        # "specs4_mean", "specs4_std",
        # "specs5_mean", "specs5_std",
        # "specs6_mean", "specs6_std",
        # "specs7_mean", "specs7_std",
        # "specs8_mean", "specs8_std",
        # "specs9_mean", "specs9_std",
        # "specs10_mean", "specs10_std",
        # "specs11_mean", "specs11_std",
        # "specs12_mean", "specs12_std",
        # "specs13_mean", "specs13_std",
        # "specs14_mean", "specs14_std",
        # "specs15_mean", "specs15_std",

        # Power
        # "power_1", "power_2", "power_3", "power_4", "power_5", "power_6", "power_7", "power_8", "power_9", "power_10",
        # "power_11", "power_12", "power_13", "power_14", "power_15", "power_16", "power_17", "power_18", "power_19", "power_20",
        # "power_21", "power_22", "power_23", "power_24", "power_25", "power_26", "power_27", "power_28", "power_29", "power_30",
        # "power_31", "power_32", "power_33", "power_34", "power_35", "power_36", "power_37", "power_38", "power_39", "power_40",
        # "power_41", "power_42", "power_43", "power_44", "power_45", "power_46", "power_47", "power_48", "power_49", "power_50",

        # Tempo
        "tempo_mean",  "tempo_std"
    ])

    # sos = butter(50, (100, 750), 'bandpass', fs=fs, output="sos")
    # audio = sosfilt(sos, audio)

    # Extract features
    spec_cent = mir.spectral_centroid(y=audio, sr=fs).flatten()
    spec_band = mir.spectral_bandwidth(y=audio, sr=fs).flatten()
    spec_cont = mir.spectral_contrast(y=audio, sr=fs).flatten()
    spec_flat = mir.spectral_flatness(y=audio).flatten()
    spec_roll = mir.spectral_rolloff(y=audio, sr=fs).flatten()
    zero_cross = mir.zero_crossing_rate(y=audio).flatten()
    rms_val = mir.rms(y=audio).flatten()
    mfccs = mir.mfcc(y=audio, sr=fs, n_mfcc=10)

    onset_env = onset_strength(y=audio, sr=fs)
    tempo = mir.tempo(onset_envelope=onset_env).flatten()

    # Build the dataframe
    features_frame.loc[0] = [
        # File name
        file,
        # Non-MFCCs
        np.mean(spec_cent), np.std(spec_cent),
        np.mean(spec_band), np.std(spec_band),
        np.mean(spec_cont), np.std(spec_cont),
        np.mean(spec_flat), np.std(spec_flat),
        np.mean(spec_roll), np.std(spec_roll),
        np.mean(zero_cross), np.std(zero_cross),
        np.mean(rms_val), np.std(rms_val),

        # MFCCs
        np.mean(mfccs[0]), np.std(mfccs[0]),
        np.mean(mfccs[1]), np.std(mfccs[1]),
        np.mean(mfccs[2]), np.std(mfccs[2]),
        np.mean(mfccs[3]), np.std(mfccs[3]),
        np.mean(mfccs[4]), np.std(mfccs[4]),
        np.mean(mfccs[5]), np.std(mfccs[5]),
        np.mean(mfccs[6]), np.std(mfccs[6]),
        np.mean(mfccs[7]), np.std(mfccs[7]),
        np.mean(mfccs[8]), np.std(mfccs[8]),
        np.mean(mfccs[9]), np.std(mfccs[9]),
        # np.mean(mfccs[10]), np.std(mfccs[10]),
        # np.mean(mfccs[11]), np.std(mfccs[11]),
        # np.mean(mfccs[12]), np.std(mfccs[12]),
        # np.mean(mfccs[13]), np.std(mfccs[13]),
        # np.mean(mfccs[14]), np.std(mfccs[14]),
        # np.mean(mfccs[15]), np.std(mfccs[15]),
        # np.mean(mfccs[16]), np.std(mfccs[16]),
        # np.mean(mfccs[17]), np.std(mfccs[17]),
        # np.mean(mfccs[18]), np.std(mfccs[18]),
        # np.mean(mfccs[19]), np.std(mfccs[19]),

        # stds[0], stds[1], stds[2], stds[3], stds[4], stds[5], stds[6], stds[7], stds[8], stds[9],
        # stds[10], stds[11], stds[12], stds[13], stds[14], stds[15], stds[16], stds[17], stds[18], stds[19],
        # stds[20], stds[21], stds[22], stds[23], stds[24], stds[25], stds[26], stds[27], stds[28], stds[29],
        # stds[30], stds[31], stds[32], stds[33], stds[34], stds[35], stds[36], stds[37], stds[38], stds[39],
        # stds[40], stds[41], stds[42], stds[43], stds[44], stds[45], stds[46], stds[47], stds[48], stds[49],

        # tempo
        np.mean(tempo), np.std(tempo)
    ]

    return features_frame
