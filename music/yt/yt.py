from pytube import YouTube
from moviepy.video.io.ffmpeg_tools import ffmpeg_extract_audio
import os


def download_audio(url, filename, output_path):
    """
        Downloads audio from a YouTube video and converts it to MP3.

        Parameters:
        - url (str): The YouTube video URL.
        - filename (str): The base filename for saving the audio file (without extension).
        - output_path (str, optional): The directory where the audio file will be saved. Defaults to the current directory.

        Returns:
        - bool: True if the audio was successfully downloaded and converted to MP3, False otherwise.
        """
    try:
        # Create YouTube object
        yt = YouTube(url)

        # Get the highest resolution stream with audio
        video_stream = yt.streams.filter(only_audio=True).first()

        # Download the video
        print("Downloading audio...")
        video_stream.download(output_path, filename + ".mp4")

        # generate name for video file
        # video_title_clean = video_stream.title.replace(" ", "_")

        # Convert the video to audio (MP3)
        video_file_path = os.path.join(output_path, filename)
        audio_file_path = os.path.join(output_path, filename)

        print("Converting to MP3...")
        ffmpeg_extract_audio(video_file_path + ".mp4", audio_file_path + ".mp3")

        # Remove the original video file
        os.remove(video_file_path + ".mp4")

        print("Audio downloaded successfully as MP3.")
        print(f"Saved at: {audio_file_path}")

        # returns true if mp3 was successfully downloaded
        return True, None
    except Exception as e:
        print(f"An error occurred: {e}")

        # returns false if mp3 could not be downloaded
        return False, e
