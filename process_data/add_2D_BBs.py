import bpy
import csv
import mathutils
from bpy_extras.object_utils import world_to_camera_view

def project_point_3d_to_2d(point, cam, scene):
    co_ndc = world_to_camera_view(scene, cam, point)
    render = scene.render
    x = round(co_ndc.x * render.resolution_x)
    y = round((1.0 - co_ndc.y) * render.resolution_y)
    return x, y

def compute_2d_bbox_from_csv(csv_path, output_path, base_blendfile, camera_name="Camera.001"):
    # Load the scene
    bpy.ops.wm.open_mainfile(filepath=base_blendfile)
    bpy.context.scene.render.resolution_x = 640
    bpy.context.scene.render.resolution_y = 480
    
    scene = bpy.context.scene
    cam = bpy.data.objects[camera_name]
    camera_matrix_world = cam.matrix_world.copy()
    world_to_camera = camera_matrix_world.inverted()
    camera_matrix_list = [[el for el in row] for row in world_to_camera]


    with open(csv_path, newline='') as f:
        reader = csv.DictReader(f)
        fieldnames = reader.fieldnames + ["image_x_min", "image_y_min", "image_x_max", "image_y_max"]
        rows = list(reader)

    with open(output_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()

        for row in rows:
            try:
                min_bb = mathutils.Vector((
                    float(row["bounding_box_min_x"]),
                    float(row["bounding_box_min_y"]),
                    float(row["bounding_box_min_z"])
                ))
                max_bb = mathutils.Vector((
                    float(row["bounding_box_max_x"]),
                    float(row["bounding_box_max_y"]),
                    float(row["bounding_box_max_z"])
                ))
            except Exception as e:
                print(f"❌ Skipping row due to parsing error: {e}")
                continue

            # Compute 8 bounding box corners
            corners = [
                mathutils.Vector((x, y, z))
                for x in [min_bb.x, max_bb.x]
                for y in [min_bb.y, max_bb.y]
                for z in [min_bb.z, max_bb.z]
            ]
            projected = [project_point_3d_to_2d(corner, cam, scene) for corner in corners]
            xs, ys = zip(*projected)

            x_min = max(0, min(xs))
            x_max = min(scene.render.resolution_x - 1, max(xs))
            y_min = max(0, min(ys))
            y_max = min(scene.render.resolution_y - 1, max(ys))
            row.update({
                "image_x_min": x_min,
                "image_y_min": y_min,
                "image_x_max": x_max,
                "image_y_max": y_max
            })
            writer.writerow(row)

    print(f"✅ 2D bounding boxes written to {output_path}")

# --- Edit these ---
input_csv = "../Master_thesis/data/ground_truth.csv"
output_csv = "../Master_thesis/data/ground_truth_with_bboxes.csv"
compute_2d_bbox_from_csv(input_csv, output_csv, "data/base_scene.blend")

