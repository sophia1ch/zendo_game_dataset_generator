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
import csv
import multiprocessing

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
    # print(f"Using compute_device_type: {preferences.compute_device_type}")
    # print(f"Render device set to: {bpy.context.scene.cycles.device}")
    # for device in preferences.devices:
        # print(f"Device: {device.name}, Type: {device.type}, Active: {device.use}")

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

    # Redirect output to log file
    logfile = 'blender_render.log'
    open(logfile, 'a').close()
    old = os.dup(sys.stdout.fileno())
    sys.stdout.flush()
    os.close(sys.stdout.fileno())
    fd = os.open(logfile, os.O_WRONLY)

    # Do the rendering
    bpy.ops.render.render(write_still=True)

    # Disable output redirection
    os.close(fd)
    os.dup(old)
    os.close(old)

    if args.save_blendfile:
        bpy.context.preferences.filepaths.save_version = 0
        bpy.ops.wm.save_as_mainfile(filepath=os.path.join(args.output_dir, output_path, f"{name}.blend"))


def get_all_scene_objects():
    bpy.context.view_layer.update()
    object_list = []
    for obj in bpy.data.objects:
        if obj.type == 'MESH' and any(k in obj.name for k in ["Pyramid", "Wedge", "Block"]):
            object_list.append(obj)
    return object_list


def threading_prolog_query(args):
    """
    Threading the generating of the scenes with the given prolog query, as the query can be to complicated
    and then prolog will not generate an result and end in an infinity-loop. Every query that takes longer than
    5 seconds is aborted!
    """
    # Start a thread to time it
    pool = multiprocessing.Pool(processes=1)
    result_async = pool.apply_async(generate_prolog_structure,
                                    args=args)

    try:
        result = result_async.get(timeout=5)
    except multiprocessing.TimeoutError:
        print(f"Timeout: Generating the sample for '{args[1]}' took longer than 5 seconds!")
        return None
    else:
        pool.close()
        pool.join()
        return result


def generate_blender_examples(args, collection, num_examples, rule_idx, rule, query, negative=False):
    # Get the scenes from the prolog query. Need to thread it to get a timeout if it takes to long
    scenes = threading_prolog_query(args=(num_examples, query, args.rules_prolog_file))
    if scenes is None:
        return False

    i = 0
    while i < num_examples:
        structure = scenes[i]
        scene_name = f"{rule_idx}_{i}"
        if negative:
            scene_name = f"{rule_idx}_{i}_n"
        try:
            # Now generate it in blender
            generate_structure(args, structure, collection)
            render(args, str(rule_idx), scene_name)

            # Buffer scene objects for writing to CSV
            scene_objects = ZendoObject.instances

            csv_file_path = os.path.join(args.output_dir, "ground_truth.csv")

            with open(csv_file_path, "a", newline="") as csvfile:
                csv_writer = csv.writer(csvfile)

                for obj in scene_objects:
                    min_bb, max_bb = obj.get_world_bounding_box()
                    world_pos = obj.get_position()

                    csv_writer.writerow([scene_name, rule, query, obj.name,
                                         min_bb.x, min_bb.y, min_bb.z, max_bb.x, max_bb.y, max_bb.z,
                                         world_pos.x, world_pos.y, world_pos.z])

            # TODO: Check if this really is a fix for the generation of multiple scenes
            for obj in collection.objects:
                bpy.data.objects.remove(obj, do_unlink=True)
            ZendoObject.instances.clear()

            i += 1

        except Exception as e:
            # If not possible to generate in blender, generate a new structure with prolog and try again
            print(f"Error in scene generation: {e}")
            scenes[i] = generate_prolog_structure(1, query, args.rules_prolog_file)[0]
    return True


def main(args):
    #######################################################
    # Main
    #######################################################
    start_time = time.time()
    script_dir = os.path.dirname(bpy.data.filepath)
    if script_dir not in sys.path:
        sys.path.append(script_dir)

    bpy.ops.wm.open_mainfile(filepath=args.base_scene_blendfile)

    rules_json_file = args.rules_json_file
    num_rules = args.num_rules
    num_examples = args.num_examples
    num_invalid_examples = args.num_invalid_examples
    generate_invalid_examples = args.generate_invalid_examples

    # Write CSV header
    csv_file_path = os.path.join(args.output_dir, "ground_truth.csv")
    with open(csv_file_path, "w", newline="") as csvfile:
        csv_writer = csv.writer(csvfile)
        csv_writer.writerow(["scene_name", "rule", "query", "object_name",
                             "bounding_box_min_x", "bounding_box_min_y", "bounding_box_min_z",
                             "bounding_box_max_x", "bounding_box_max_y", "bounding_box_max_z",
                             "world_pos_x", "world_pos_y", "world_pos_z"])
    r = 0
    while r < num_rules:
        # get rule in string form and query, negative query in prolog form
        rule, query, n_query = generate_rule(rules_json_file)

        collection = bpy.data.collections.new("Structure")
        bpy.context.scene.collection.children.link(collection)

        generated_successfully = generate_blender_examples(args, collection, num_examples, r, rule, query, False)
        # If result is not true, than prolog query took to long, therefore try again
        if not generated_successfully:
            continue

        # If bool is set for generating also scenes which doesn't fulfill the rule
        if generate_invalid_examples:
            generate_blender_examples(args, collection, num_invalid_examples, r, rule, n_query, True)
        r += 1

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
