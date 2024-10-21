import physio.read_dataset.deap as read_deap
import music.read_dataset.deap as read_deap_audio
import music.preprocessing.preprocessing as prep
import music.features.features as feat
import settings as sets
from scipy.io.wavfile import write as wavwrite
from scipy.io.wavfile import read as wavread
import pandas as pd
import os
import numpy as np
import librosa as mir
import matplotlib.pyplot as plt
from scipy.fftpack import dct


def mel_to_freq(mel):
    return 700 * (np.exp(mel/1125) - 1)


def freq_to_mel(freq):
    return 1125 * np.log(1 + freq / 700)


def main():
    if sets.CREATE_RAW_PHYSIO:
        # Extract raw data from deap
        save_location = os.path.join(sets.PATH_TO_DEAP, sets.RAW_DATA)
        _ = read_deap.read_directory(sets.PATH_TO_DEAP_PHYSIO, save_location)

    if sets.CREATE_PREPROCESSED_PHYSIO:
        # Preprocess data from DEAP
        saved_data = os.path.join(sets.PATH_TO_DEAP, sets.RAW_DATA)
        save_location = os.path.join(sets.PATH_TO_DEAP, sets.PREPROCESSED_DATA)
        _ = read_deap.preprocess_data(saved_data=saved_data, save_location=save_location)

    if sets.CREATE_FEATURES_PHYSIO:
        # extract physiological features
        saved_data = os.path.join(sets.PATH_TO_DEAP, sets.PREPROCESSED_DATA)
        save_location = os.path.join(sets.PATH_TO_DEAP, sets.PHYSIO_FEATURE_DATA)
        _ = read_deap.extract_physiological_features(saved_data=saved_data,
                                                     save_location=save_location,
                                                     num_participants=30)

    if sets.CREATE_RAW_AUDIO:
        # Extract raw audio from DEAP and save to wav files
        save_location = sets.PATH_TO_DEAP_RAW_AUDIO
        _ = read_deap_audio.read_deap_yt(sets.PATH_TO_DEAP_META,
                                         save_location,
                                         extract_from_yt=sets.EXTRACT_FROM_YT,
                                         convert_to_wav=sets.CONVERT_TO_WAV)

    if sets.CREATE_PREPROCESSED_AUDIO:
        # Preprocess DEAP AUDIO
        raw_dir = sets.PATH_TO_DEAP_RAW_AUDIO
        raw_files = os.listdir(raw_dir)

        # Delete any non audio files in the directory
        not_needed = []
        for item in raw_files:
            if ".wav" not in item:
                not_needed.append(item)
        if len(not_needed) > 0:
            for item in not_needed:
                raw_files.remove(item)

        # Read metadata to get the audio start times
        metadata = pd.read_csv(sets.PATH_TO_DEAP_META)
        reduced = metadata[["Experiment_id", "Highlight_start"]]

        # Preprocess and save each audio file to the preprocessed audio directory
        preprocessed_audio_dir = sets.PATH_TO_DEAP_PREPROCESSED_AUDIO
        for (i, file) in enumerate(raw_files):
            # Read input audio
            input_path = os.path.join(raw_dir, file)
            fs, audio_data = prep.read_and_preprocess_wav_audio(input_path)

            # Get highlight start time to crop audio
            temp_file_name = file.replace("audio-file-", "")
            experiment_id = int(temp_file_name.replace(".wav", ""))
            data_point = reduced[reduced["Experiment_id"] == experiment_id]
            highlight_start = data_point.iloc[0]["Highlight_start"]
            cropped_audio = prep.crop_audio(audio_data, fs, highlight_start, 60, experiment_id)

            """
            Shorter length files:
            33: 56.7 seconds
            1: 59.0 seconds
            24: 48.9 seconds
            26: 57.0 seconds
            17: 57.4 seconds
            25: 58.8 seconds
            """

            # Save new file
            new_file_name = file.replace("audio-file", "prep-audio")
            wavwrite(os.path.join(preprocessed_audio_dir, new_file_name), fs, cropped_audio)

    if sets.CREATE_FEATURES_AUDIO:
        # Create dataframe
        audio_features = None

        prep_audio_dir = sets.PATH_TO_DEAP_PREPROCESSED_AUDIO
        count = 1
        for file in os.listdir(prep_audio_dir):
            print(f"Starting with file {count} of 40.")
            count += 1
            fs, data = wavread(os.path.join(prep_audio_dir, file))
            features = feat.extract_features(file, data, fs)
            if audio_features is None:
                audio_features = features
            else:
                audio_features = pd.concat([audio_features, features])

        if audio_features is not None:
            save_location = os.path.join(sets.PATH_TO_DEAP, sets.AUDIO_FEATURE_DATA)
            audio_features.to_csv(save_location)


if __name__ == "__main__":
    main()

    """
    For 10 MFCCs:
    2324Hz - 4920Hz
    """
