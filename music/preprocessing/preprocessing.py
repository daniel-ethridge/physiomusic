from scipy.io.wavfile import read as wavread
import numpy as np
from scipy import signal
from pydub import AudioSegment


def convert_mp3_to_wav(input_file, output_file):
    """
    This function is run on only a single file. To convert a directory, iterate over the directory and call this
    function for every file.

    :param input_file: The input file
    :param output_file: How to save the output (no extension)
    """
    sound = AudioSegment.from_mp3(input_file)
    sound.export(output_file, format="wav")
    print(f"{output_file} created!")


def read_wav_audio(filename, mmap=False):
    """
    :param filename: Input WAV file
    :param mmap: See https://docs.scipy.org/doc/scipy/reference/generated/scipy.io.wavfile.read.html

    :returns:
        - rate: sample rate of WAV file
        - data: Return 32-bit floating-point data
    """

    rate, data = wavread(filename, mmap)

    # Set the number of bits. Return if we have float32
    nbits = 8
    if data.dtype == "int16":
        nbits = 16
    elif data.dtype == "int32":
        nbits = 32
    elif data.dtype == "float32":
        return rate, data

    # Center 8 bit audio around 0
    if nbits == 8:
        data -= 2 ** (nbits - 1)

    # Map data between 0 and 1 and set as float32
    data = data / (2 ** (nbits - 1))
    data.astype("float32")

    # return audio
    return rate, data


def mix_to_mono(audio):
    """
    :param audio: Multichannel audio data to downmix
    :return: Downmixed audio data
    """
    # Transpose audio if necessary
    if audio.shape[0] < audio.shape[1]:
        audio = np.transpose(audio)

    return np.mean(audio, axis=1)


def remove_dc_offset(audio):
    """
    :param audio: The float32 audio data stored in a numpy array
    :return: The numpy array with the dc offset removed
    """
    dc_offset = np.mean(audio)
    corrected_audio = audio - dc_offset
    return corrected_audio


def normalize_audio(audio):
    """
    :param audio: Audio data to be normalized
    :return: Normalized audio data
    """
    abs_audio = np.abs(audio)
    maximum_value = np.max(abs_audio)
    return audio / maximum_value


def downsample_audio(audio, old_fs, new_fs, order=4):
    """
    :param audio: the audio to downsample
    :param old_fs: The orginal samplerate
    :param new_fs: The new samplerate
    :param order: The order of the anti-aliasing filter. Default is 4
    :return: The downsampled audio data
    """
    # filter audio
    filter_cutoff = new_fs / 2
    sos = signal.butter(order, filter_cutoff, output="sos", fs=old_fs)
    filtered = signal.sosfilt(sos, audio)

    # Downsample
    old_x = np.arange(0, len(filtered) / old_fs, 1 / old_fs)
    new_x = np.arange(0, len(filtered) / old_fs, 1 / new_fs)
    return np.interp(new_x, old_x, filtered)


def preprocess_audio(audio, downsample=False, old_fs=0, new_fs=0, order=4):
    """
    :param audio: The float32 audio data stored in a numpy array
    :param downsample: Optional downsampling. Default is False
    :param old_fs: If downsample is True, the sampling rate of the audio passed. Must be set if downsample is True.
    :param new_fs: If downsample is True, the new sampling rate after downsampling. Must be set if downsample is True.
    :param order: If downsample is True, the order of the anti-aliasing filter used. Defaults to 4.
    :return: preprocessed version of the array
    """
    mono_audio = mix_to_mono(audio)
    no_dc_audio = remove_dc_offset(mono_audio)
    normalized_audio = normalize_audio(no_dc_audio)
    if downsample:
        return downsample_audio(normalized_audio, old_fs, new_fs, order)

    return normalized_audio


def read_and_preprocess_wav_audio(filename, mmap=False, downsample=False, old_fs=0, new_fs=0, order=4):
    """
    :param filename: Input WAV file
    :param mmap: See https://docs.scipy.org/doc/scipy/reference/generated/scipy.io.wavfile.read.html
    :param downsample: Optional downsampling. Default is False
    :param old_fs: If downsample is True, the sampling rate of the audio passed. Must be set if downsample is True.
    :param new_fs: If downsample is True, the new sampling rate after downsampling. Must be set if downsample is True.
    :param order: If downsample is True, the order of the anti-aliasing filter used. Defaults to 4.

    :returns:
        - rate: sample rate of WAV file
        - data: Return 32-bit floating-point data
    """
    fs, data = read_wav_audio(filename, mmap)
    return fs, preprocess_audio(data, downsample, old_fs, new_fs, order)


def crop_audio(data, fs, start_time, duration, id_number=None):
    """
    Crop audio data to a specified length

    :param data: The input audio data
    :param fs: the sample rate
    :param start_time: The start time in seconds
    :param duration: The duration in seconds
    :param id_number: Optional parameter for console logging / debugging purposes. Defaults to None
    :return: Audio data vector of length fs * duration
    """
    if id_number is not None:
        print(f"\nCropping file {id_number}...")

    t = np.arange(0, len(data) / fs, 1 / fs)
    try:
        start_idx = np.where(t >= start_time)[0][0]
        end_idx = np.where(t > start_time + duration)[0][0]
        cropped_audio = np.array(data[start_idx:end_idx]).astype(np.float32)
    except IndexError:
        start_idx = np.where(t >= start_time)[0][0]
        cropped_audio = np.array(data[start_idx:]).astype(np.float32)
        new_duration = cropped_audio.shape[0] / fs
        if id_number is not None:
            print(f"WARNING: File {id_number} is length {new_duration}.")

    # fade in
    fade_in = np.arange(0, 1, 1 / 100)
    for (i, val) in enumerate(fade_in):
        cropped_audio[i] = cropped_audio[i] * val

    # fade out
    fade_out = np.arange(1, 0, 1 / 100)
    for (i, val) in enumerate(fade_out):
        idx = len(cropped_audio) - (100 - i)
        cropped_audio[idx] = cropped_audio[idx] * val

    if id_number is not None:
        print(f"Audio file {id_number}")

    return cropped_audio