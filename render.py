import bpy
import mathutils, platform
from pathlib import Path
import os, sys, argparse
import json
from argparse import Namespace
import yaml
import numpy as np
import random
from blender_objects import blender_obj, check_collision, set_random_position, place_ontop
import utils


def main(args):
    # add directory to sys.path
    script_dir = os.path.dirname(bpy.data.filepath)
    if script_dir not in sys.path:
        sys.path.append(script_dir)

    #######################################################
    # Main
    #######################################################

    bpy.ops.wm.open_mainfile(filepath=args.base_scene_blendfile)
    object_shapes, object_colors, object_sizes = utils.read_properties_json(args.properties_json)
    object_colors_no_bw = object_colors.copy()
    del object_colors_no_bw["white"]
    del object_colors_no_bw["black"]
    object_colors_no_gs = object_colors_no_bw.copy()
    del object_colors_no_gs["gray"]
    #object_shape_values = list(object_shapes.values())
    object_size_values = list(object_sizes.values())
    #object_color_values = list(object_colors.values())
    object_color_values_no_bw = list(object_colors_no_bw.values())
    object_color_values_no_gs = list(object_colors_no_gs.values())

    list_of_objects = []
    def add(obj):
        list_of_objects.append(obj)
        return obj
    def create(name: str, scale: str|float, color: str|list[float]):
        scale = scale if isinstance(scale, float) else object_sizes[scale]
        color = color if isinstance(color, list) else object_colors[color]
        return add(blender_obj(args, name=object_shapes[name], scale=scale, color=color))

    disc = create("disc", "medium", "yellow")
    disc.set_pose(0.0, 0.0, 0.0, "side")

    # Pyramid on pyramid
    bottom = create("pyramid", 1.0, "yellow")
    bottom.set_pose(-4.0, -1.0, 40.0, type="upright")
    mid = create("pyramid", 0.5, "green")
    mid.set_ontop(bottom)
    top = create("pyramid", 0.3, "red")
    top.set_ontop(mid)
    big_top = create("pyramid", 1.4, "white")
    big_top.set_ontop(top)

    # Disc on disc
    prev_disc = None
    for i in range(6):
        new_disc = create("disc", (1.0 - i/10.0), random.choice(object_color_values_no_gs))
        if prev_disc is None:
            new_disc.set_pose(2.0, 4.0, 40.0, type="upright")
        else:
            new_disc.set_ontop(prev_disc)
        prev_disc = new_disc
    top_pyr = create("pyramid", 0.2, "white")
    top_pyr.set_ontop(prev_disc)

    # Pyramid on disc
    bot_disc = create("disc", "medium", "black")
    bot_disc.set_pose(2.0, 0.0, 0.0, "upright")

    top_pyr = create("pyramid", 0.5, "white")
    top_pyr.set_ontop(bot_disc)

    # Disc on pyramid
    bot_pyr = create("pyramid", "medium", "blue")
    bot_pyr.set_pose(0.0, -3.0, 0.0, "upright")

    top_disc = create("disc", 0.5, "blue")
    top_disc.set_ontop(bot_pyr)
    
    for _ in range(10):
        pyr_scale = random.choice(object_size_values)
        pyr_color = random.choice(object_color_values_no_bw)
        pyr = create("pyramid", pyr_scale, pyr_color)
        set_random_position(pyr, list_of_objects)

    #######################################################
    # Initialize render settings
    #######################################################

    # Detect system OS and configure the best rendering settings
    system = platform.system()
    preferences = bpy.context.preferences.addons["cycles"].preferences

    # Set the best compute device type based on the OS
    if system == "Darwin":
        preferences.compute_device_type = "METAL"
    elif system in ["Windows", "Linux"]:
        preferences.compute_device_type = "OPTIX"
    else:
        preferences.compute_device_type = "NONE"

    # Refresh device list after setting compute_device_type
    preferences.get_devices()

    # Set render device to GPU if available; otherwise, use CPU
    if preferences.compute_device_type in ["OPTIX", "METAL"]:
        bpy.context.scene.cycles.device = "GPU"
    else:
        bpy.context.scene.cycles.device = "CPU"

    # Explicitly activate the available devices based on compute_device_type
    for device in preferences.devices:
        # Activate only the OptiX device for NVIDIA GPU
        if preferences.compute_device_type == "OPTIX" and device.type == "OPTIX":
            device.use = True
        # If using METAL on Mac, activate both GPU and CPU devices
        elif preferences.compute_device_type == "METAL" and device.type in ["GPU", "CPU"]:
            device.use = True
        # Use CPU if no other options are available
        elif preferences.compute_device_type == "NONE" and device.type == "CPU":
            device.use = True
        else:
            # Ensure other devices are not used
            device.use = False

    # Debug render devices being used
    print(f"Using compute_device_type: {preferences.compute_device_type}")
    print(f"Render device set to: {bpy.context.scene.cycles.device}")
    for device in preferences.devices:
        print(f"Device: {device.name}, Type: {device.type}, Active: {device.use}")

    #######################################################
    # Render
    #######################################################

    # Get the directory of the executing Python script
    script_dir = os.path.dirname(os.path.realpath(__file__))

    # Set rendering properties
    bpy.context.scene.render.engine = 'CYCLES'
    bpy.context.scene.render.filepath = os.path.join(script_dir, args.output_dir, args.output_image_file)
    bpy.context.scene.render.image_settings.file_format = 'PNG'
    bpy.context.scene.cycles.samples = int(args.render_num_samples)
    bpy.context.scene.render.resolution_x = args.width
    bpy.context.scene.render.resolution_y = args.height
    bpy.context.scene.render.resolution_percentage = 100

    print("Saving output image to:", bpy.context.scene.render.filepath)

    # Render image and write to disk instead of only keeping in memory
    bpy.ops.render.render(write_still=True)

    if args.save_blendfile:
        bpy.ops.wm.save_as_mainfile(filepath=os.path.join(args.output_dir, "test.blend"))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config-file", type=str, default="configs/simple_config.yml",
                        help='config file for rendering')
    conf = parser.parse_args()

    with open(conf.config_file) as f:
        args = yaml.safe_load(f.read())  # load the config file

    args = Namespace(**args)
    main(args)
