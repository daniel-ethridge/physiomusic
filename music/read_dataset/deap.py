import pandas as pd
from ..yt.yt import download_audio as dl_yt
from ..preprocessing.preprocessing import convert_mp3_to_wav
import os
import numpy as np
import pickle as pkl


def read_deap_yt(metadata_file, save_location, extract_from_yt=True, convert_to_wav=True):
    if extract_from_yt:
        # Read in metadata file
        metadata = pd.read_csv(metadata_file)
        reduced = metadata[["Experiment_id", "Artist", "Title", "Youtube_link", "Highlight_start", "Manual_links"]]

        reduced = reduced.loc[reduced["Experiment_id"] >= 1].reset_index().drop("index", axis=1)

        reduced.to_csv(metadata_file.replace(".csv", "_sm.csv"))

        # extract the 'youtube_link' column and save it to a list
        youtube_links = reduced['Youtube_link'].tolist()
        backups = reduced["Manual_links"].tolist()

        # extract experiment ID and save to list
        experiment_id = reduced["Experiment_id"].tolist()

        # extract song names and save to list
        song_titles = reduced['Title'].tolist()

        # Extract start times from file
        start_times = np.array(reduced["Highlight_start"])

        with open(os.path.join(save_location, "start_times.pkl"), "wb") as f:
            pkl.dump(start_times, f)

        failed_to_download = pd.DataFrame(columns=["song_title", "youtube-link", "trial-number", "reason-for-failure"])
        # iterate over youtube links and download each one
        for index, link in enumerate(youtube_links):
            print(f"\nSTARTING: {song_titles[index]}-{experiment_id[index]}")
            # download full mp3
            # scrub strings and assemble filename
            output_filename = f"audio-file-{experiment_id[index]}"

            # download mp3 from youtube
            dl_successful, reason_for_failure = dl_yt(link, output_filename, save_location)

            # Attempt 2
            if not dl_successful:
                dl_successful, reason_for_failure = dl_yt(backups[index], output_filename, save_location)

            # if download failed, add title to failed list
            if not dl_successful:
                print(f"FAILED: {song_titles[index]}-{experiment_id[index]}")
                failed_to_download.loc[len(failed_to_download)] = [song_titles[index],
                                                                   youtube_links[index],
                                                                   experiment_id[index],
                                                                   reason_for_failure]
            else:
                print(f"SUCCESS: {song_titles[index]}-{experiment_id[index]}")

        if failed_to_download.shape[0] > 0:
            failed_to_download.to_csv(os.path.join(save_location, "failed-to-download.csv"))

    if convert_to_wav:
        dir_list = os.listdir(save_location)
        for (_, file) in enumerate(dir_list):
            if ".mp3" in file:
                full_path = os.path.join(save_location, file)
                convert_mp3_to_wav(full_path, full_path.replace("mp3", "wav"))

        dir_list = os.listdir(save_location)
        for (_, file) in enumerate(dir_list):
            if ".mp3" in file:
                full_path = os.path.join(save_location, file)
                os.remove(full_path)
