import pickle
from process_hrv import extract_hrv
from process_gsr import extract_gsr
from process_resp import extract_resp


def extract_physiological_features():
    with open("./data/deap/preprocessed-physio-data.pkl", "rb") as f:
        preprocessed_physio_data = pickle.load(f)

    feature_data = {}
    for key in preprocessed_physio_data.keys():
        if key == "s11.bdf":
            break

        part_data = preprocessed_physio_data[key]  # Get participant data

        # Get sample rate and physio data
        fs = 512
        ppg = part_data["ppg"]
        resp = part_data["resp"]
        gsr = part_data["gsr"]

        # Get baseline indeces and truncate data
        bl_start = part_data["baseline_start_idx"]
        bl_end = part_data["baseline_end_idx"]
        bl_ppg = ppg[bl_start:bl_end]
        bl_gsr = gsr[bl_start:bl_end]
        bl_resp = resp[bl_start:bl_end]

        # Extract baseline metrics
        bl_hrv = extract_hrv(bl_ppg, fs)
        bl_resp_metrics = extract_resp(bl_resp)
        bl_gsr_metrics = extract_gsr(bl_gsr)
        baseline = {
            "hrv": bl_hrv,
            "gsr": bl_gsr_metrics,
            "resp": bl_resp_metrics
        }

        # Save to dictionary
        feature_data[key] = {
            "baseline": baseline,
        }

        # Iterate over all start times
        starts = part_data["stimuli_start_idxs"]
        ends = part_data["stimuli_end_idxs"]
        for i, start in enumerate(starts):
            # Get stimulus indeces and truncate data
            stim_start = start
            stim_end = int(ends[i])
            stim_ppg = ppg[stim_start:stim_end]
            stim_gsr = gsr[stim_start:stim_end]
            stim_resp = gsr[stim_start:stim_end]

            # Extract stimulus metrics
            stim_hrv = extract_hrv(stim_ppg, fs)
            stim_gsr_metrics = extract_gsr(stim_gsr)
            stim_resp_metrics = extract_resp(stim_resp)

            # Create the simulus dictionary
            stimuli_dictionary = {
                f"stim_{i+1}": {
                    "hrv": stim_hrv,
                    "gsr": stim_gsr_metrics,
                    "resp": stim_resp_metrics
                }
            }

            # Add to the final dictionary
            feature_data[key].update(stimuli_dictionary)

    with open("data/deap/physio-feature-data.pkl", "wb") as f:
        pickle.dump(feature_data, f)

    print()


if __name__ == "__main__":
    extract_physiological_features()
