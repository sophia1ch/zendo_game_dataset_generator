import csv
import json
import numpy as np
from pathlib import Path

def load_camera_info(json_path):
    with open(json_path) as f:
        data = json.load(f)
    return {
        "camera_location": np.array(data["camera_location"]),
        "camera_matrix": np.array(data["camera_matrix"]),
        "res_x": data["resolution_x"],
        "res_y": data["resolution_y"]
    }

def compute_2d_bbox(min_bb, max_bb, camera_matrix, res_x, res_y):
    # Generate the 8 corner points of the 3D bounding box
    corners = np.array([
        [x, y, z]
        for x in [min_bb[0], max_bb[0]]
        for y in [min_bb[1], max_bb[1]]
        for z in [min_bb[2], max_bb[2]]
    ])

    # Convert to homogeneous coordinates
    corners_homo = np.hstack([corners, np.ones((8, 1))])  # [8, 4]

    # Apply world-to-camera transformation
    camera_matrix_np = np.array(camera_matrix)
    corners_cam = (camera_matrix_np @ corners_homo.T).T[:, :3]  # [8, 3]

    # Project to 2D
    projected = corners_cam[:, :2] / corners_cam[:, 2:3]

    # Map from NDC-style to pixel coordinates
    xs = (projected[:, 0] + 1) * res_x / 2
    ys = (1 - projected[:, 1]) * res_y / 2

    # Clamp and return as integers
    x_min, x_max = np.clip(xs.min(), 0, res_x - 1), np.clip(xs.max(), 0, res_x - 1)
    y_min, y_max = np.clip(ys.min(), 0, res_y - 1), np.clip(ys.max(), 0, res_y - 1)
    return int(x_min), int(y_min), int(x_max), int(y_max)

def add_2d_boxes_to_csv(input_csv, output_csv, camera_json):
    cam_info = load_camera_info(camera_json)

    with open(input_csv, newline='') as infile:
        reader = csv.DictReader(infile)
        fieldnames = reader.fieldnames + [
            "image_x_min", "image_y_min", "image_x_max", "image_y_max"
        ]
        rows = []
        for row in reader:
            min_bb = [
                float(row["bounding_box_min_x"]),
                float(row["bounding_box_min_y"]),
                float(row["bounding_box_min_z"])
            ]
            max_bb = [
                float(row["bounding_box_max_x"]),
                float(row["bounding_box_max_y"]),
                float(row["bounding_box_max_z"])
            ]

            x_min, y_min, x_max, y_max = compute_2d_bbox(
                min_bb, max_bb,
                cam_info["camera_matrix"],
                cam_info["res_x"],
                cam_info["res_y"]
            )

            row.update({
                "image_x_min": x_min,
                "image_y_min": y_min,
                "image_x_max": x_max,
                "image_y_max": y_max
            })
            rows.append(row)

    with open(output_csv, "w", newline="") as outfile:
        writer = csv.DictWriter(outfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

if __name__ == "__main__":
    input_csv = "../Master_thesis/training_length/ground_truth.csv"  # <- change to your input file
    output_csv = "../Master_thesis/training_length/ground_truth_wb.csv"
    camera_json = "camera_params.json"

    add_2d_boxes_to_csv(input_csv, output_csv, camera_json)
    print(f"✅ Added 2D bounding boxes to {output_csv}")
