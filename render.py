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
from rules.rules import generate_rule, generate_prolog_structure
import zendo_objects
import time


import utils
from zendo_objects import *
from generate import generate_structure


def render(args, output_path, name):
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
    bpy.context.scene.render.filepath = os.path.join(script_dir, args.output_dir, output_path, name)
    bpy.context.scene.render.image_settings.file_format = 'PNG'
    bpy.context.scene.cycles.samples = int(args.render_num_samples)
    bpy.context.scene.render.resolution_x = args.width
    bpy.context.scene.render.resolution_y = args.height
    bpy.context.scene.render.resolution_percentage = 100

    print("Saving output image to:", bpy.context.scene.render.filepath)

    # Render image and write to disk instead of only keeping in memory
    bpy.ops.render.render(write_still=True)

    if args.save_blendfile:
        bpy.ops.wm.save_as_mainfile(filepath=os.path.join(args.output_dir, output_path, f"{name}.blend"))


def main(args):
    #######################################################
    # Main
    #######################################################
    start_time = time.time()
    script_dir = os.path.dirname(bpy.data.filepath)
    if script_dir not in sys.path:
        sys.path.append(script_dir)

    bpy.ops.wm.open_mainfile(filepath=args.base_scene_blendfile)

    prolog_file = args.rules_prolog_file
    rules_json_file = args.rules_json_file
    num_rules = args.num_rules
    num_examples = args.num_examples

    for r in range(num_rules):
        rule, query = generate_rule(rules_json_file)
        scenes = generate_prolog_structure(num_examples, query, prolog_file)

        collection = bpy.data.collections.new("Structure")
        bpy.context.scene.collection.children.link(collection)

        i = 0
        while i < num_examples:
            # scenes = ["['item(3, red, pyramid, upright, grounded)', 'item(2, yellow, wedge, upright, grounded)', 'item(1, blue, pyramid, flat, pointing(3))', 'item(0, red, block, flat, touching(2))']"]
            structure = scenes[i]
            scene_name = f"{r}_{i}"
            try:
                generate_structure(args, structure, collection)
                render(args, str(r), scene_name)

                file_path = os.path.join(args.output_dir, str(r), f"{scene_name}.txt")
                with open(file_path, "w") as file:
                    file.write(str(rule) + "\n" + str(query) + "\n" + str(structure))
                i += 1

            except Exception as e:
                scenes[i] = generate_prolog_structure(1, query, prolog_file)[0]
                print(e)
    print(f"Time to complete: {time.time() - start_time}")


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config-file", type=str, default="configs/simple_config.yml",
                        help='config file for rendering')
    conf = parser.parse_args()

    with open(conf.config_file) as f:
        args = yaml.safe_load(f.read())  # load the config file

    args = Namespace(**args)
    main(args)
