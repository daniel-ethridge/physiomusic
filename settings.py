# Settings for MIR to Physio Features Project
import os


# ### Booleans ### #
# Physiological signals
CREATE_RAW_PHYSIO = False
CREATE_PREPROCESSED_PHYSIO = False
CREATE_FEATURES_PHYSIO = True

# Audio
CREATE_RAW_AUDIO = False
EXTRACT_FROM_YT = False
CONVERT_TO_WAV = False
CREATE_PREPROCESSED_AUDIO = False
CREATE_FEATURES_AUDIO = False

# DEAP Paths
PATH_TO_DEAP = "./data/deap/"
PATH_TO_DEAP_PHYSIO = os.path.join(PATH_TO_DEAP, "original-data")
PATH_TO_DEAP_META = os.path.join(PATH_TO_DEAP, "Metadata/metadata_csv/video_list_sm.csv")
PATH_TO_DEAP_RAW_AUDIO = os.path.join(PATH_TO_DEAP, "raw-audio")
PATH_TO_DEAP_PREPROCESSED_AUDIO = os.path.join(PATH_TO_DEAP, "preprocessed-audio")

# DEAP data file names
RAW_DATA = "./features/raw-physio-data.pkl"
PREPROCESSED_DATA = "./features/preprocessed-physio-data.pkl"
PHYSIO_FEATURE_DATA = "./features/physio-feature-data.csv"
AUDIO_FEATURE_DATA = "./features/audio-feature-data.csv"
