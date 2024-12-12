import bpy
import mathutils, platform
from pathlib import Path
import os, sys, argparse
import json
from argparse import Namespace
import yaml
from structure import *
import numpy as np
import random

import zendo_objects
from blender_objects import blender_obj, check_collision, set_random_position, place_ontop

import utils
from zendo_objects import *


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

    block = zendo_objects.Block(args, 1.0, object_colors["yellow"], "upright")
    block2 = zendo_objects.Block(args, 1.0, object_colors["red"], "upright")
    block3 = zendo_objects.Block(args, 1.0, object_colors["purple"], "flat")
    wedge2 = zendo_objects.Wedge(args, 1.0, object_colors["cyan"], "cheesecake")
    wedge2.rotate_z(-90)
    pyr2 = zendo_objects.Pyramid(args, 1.0, object_colors["brown"], "upright")
    pyr = zendo_objects.Pyramid(args, 1.0, object_colors["green"], "upright")
    wedge = zendo_objects.Wedge(args, 1.0, object_colors["blue"], "upright")
    wedge.set_rotation('X', -40)
    pyr2.set_rotation('X', 40)
    rel_touching(block, block2, face='left')
    rel_touching(block3, block2, face='right')
    rel_touching(pyr, block3, face='top')
    rel_touching(wedge, block, face='left')
    rel_touching(wedge2, block, face='top')
    rel_touching(pyr2, block2, face='top')





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
