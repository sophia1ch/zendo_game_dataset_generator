import bpy
import mathutils, platform
from pathlib import Path
import os, sys, argparse
import json
from argparse import Namespace
import yaml
import numpy as np
import random
from blender_objects import blender_obj, check_collision
import utils


def main(args):
    # add directory to sys.path
    script_dir = os.path.dirname(bpy.data.filepath)
    if script_dir not in sys.path:
        sys.path.append(script_dir)


    #######################################################
    # Initialize render settings
    #######################################################

    # Set Cycles as the render engine
    # TODO: change the blender render settings for better images
    render_args = bpy.context.scene.render
    render_args.engine = "CYCLES"
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    # TODO: make the following line work!!
    render_args.filepath = os.path.join(output_dir, args.output_image_file)
    render_args.resolution_x = args.width
    render_args.resolution_y = args.height
    render_args.resolution_percentage = 100


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

    # Some CYCLES-specific stuff
    # TODO: change the blender render settings for better images
    bpy.data.worlds['World'].cycles.sample_as_light = True
    bpy.context.scene.cycles.blur_glossy = 2.0
    bpy.context.scene.cycles.samples = args.render_num_samples
    bpy.context.scene.cycles.transparent_min_bounces = args.render_min_bounces
    bpy.context.scene.cycles.transparent_max_bounces = args.render_max_bounces
    bpy.context.scene.cycles.tile_x = args.render_tile_size
    bpy.context.scene.cycles.tile_y = args.render_tile_size


    #######################################################
    # Main
    #######################################################

    bpy.ops.wm.open_mainfile(filepath=args.base_scene_blendfile)
    object_shapes, object_colors, object_sizes = utils.read_properties_json(args.properties_json)

    pyramid = blender_obj(args, name=object_shapes["pyramid"])
    pyramid.move(0, 0, 2)
    # TODO: set color is working but not in the right way, look at blender_obj class
    pyramid.set_color(object_colors["green"])


    #######################################################
    # Render
    #######################################################

    bpy.ops.render.render(True)
    # TODO: because the render_args.file_path line isn't working, we need to save the rendered file manually...
    rendered_image = bpy.data.images["Render Result"]
    rendered_image.save_render(filepath=os.path.join(output_dir, args.output_image_file))
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