import platform
import sys, argparse
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from argparse import Namespace
import yaml
from rules.rules import generate_rule
import time
import csv
import subprocess
from multiprocessing import get_context
from zendo_objects import *
from generate import generate_structure, get_image_bounding_box
import utils
from utils import debug
import gc
import json

import os

global prolog_pool

def init_pool():
    global prolog_pool
    ctx = get_context("fork")
    prolog_pool = ctx.Pool(processes=1, maxtasksperchild=1)

def shutdown_pool():
    global prolog_pool
    if prolog_pool is not None:
        prolog_pool.terminate()
        prolog_pool.join()
        prolog_pool = None

def load_zendo_rules(filepath):
    if not os.path.exists(filepath):
        print(f"Error: The file {filepath} does not exist.")
        return None, None, None

    rules = []
    queries = []
    queries_n = []

    with open(filepath, "r", encoding="utf-8") as f:
        current_rule = None
        current_query = None
        current_query_n = None

        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue

            if line.startswith("rule:"):
                current_rule = line.split("rule:", 1)[1].strip().strip("'")
                rules.append(current_rule)
            elif line.startswith("query:"):
                current_query = line.split("query:", 1)[1].strip().strip("'")
                queries.append(current_query)
            elif line.startswith("query_n:"):
                current_query_n = line.split("query_n:", 1)[1].strip().strip("'")
                queries_n.append(current_query_n)

    return rules, queries, queries_n


def render(args, output_path, name):
    """
    Renders a scene using Blender's Cycles engine with specified settings.

    This function sets up the rendering configuration, including the compute device,
    resolution, sampling, and output file format. It then performs the rendering
    and optionally saves the Blender scene file.

    :param args: Configuration arguments for rendering, including resolution,
                 sample count, output directory, and rendering options.
    :param output_path: The subdirectory within the output directory where
                        the rendered image will be saved.
    :param name: The name of the rendered image file (without extension).
    """

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
        if preferences.compute_device_type == "OPTIX" and device.type in ["OPTIX", "CUDA"]:
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
    debug(f"Using compute_device_type: {preferences.compute_device_type}")
    debug(f"Render device set to: {bpy.context.scene.cycles.device}")
    for device in preferences.devices:
        debug(f"Device: {device.name}, Type: {device.type}, Active: {device.use}")

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

    debug(f"Saving output image to: {bpy.context.scene.render.filepath}")

    # Redirect output to log file
    logfile = 'blender_render.log'
    open(logfile, 'a').close()
    old = os.dup(sys.stdout.fileno())
    sys.stdout.flush()
    os.close(sys.stdout.fileno())
    fd = os.open(logfile, os.O_WRONLY)

    # Do the rendering
    render_start = time.time()
    bpy.ops.render.render(write_still=True)
    render_end = time.time()

    # Log render time
    render_duration = render_end - render_start
    print(f"Render time: {render_duration:.4f} seconds")

    # Disable output redirection
    os.close(fd)
    os.dup(old)
    os.close(old)

    if args.save_blendfile:
        bpy.context.preferences.filepaths.save_version = 0
        bpy.ops.wm.save_as_mainfile(filepath=os.path.join(args.output_dir, output_path, f"{name}.blend"))

    return render_duration


def get_all_scene_objects():
    """
    Retrieves all mesh objects in the current Blender scene that match
    specific object types (Pyramid, Wedge, Block).

    This function updates the view layer and filters objects based on
    their names to return only those relevant to the scene.

    :return: A list of Blender mesh objects that match the specified types.
    """

    bpy.context.view_layer.update()
    object_list = []
    for obj in bpy.data.objects:
        if obj.type == 'MESH' and any(k in obj.name for k in ["Pyramid", "Wedge", "Block"]):
            object_list.append(obj)
    return object_list


def call_prolog_subprocess(n, query, prolog_file):
    try:
        result = subprocess.check_output(
            ['python3', 'call_generate_prolog.py', str(n), query, prolog_file],
            timeout=6
        )
        return json.loads(result)
    except subprocess.TimeoutExpired:
        debug(f"Timeout: Prolog query took too long.")
    except Exception as e:
        debug(f"Error in subprocess: {e}")
    return None

def generate_blender_examples(args, num_examples, rule_idx, rule, query, start_rule, negative=False):
    """
    Generates Blender scenes based on Prolog query results and renders them.

    This function now internally manages its own Blender collection and cleans it up safely.
    """

    total_start = time.time()
    render_time_total = 0.0

    # Create a new collection internally
    collection = bpy.data.collections.new("Structure")
    bpy.context.scene.collection.children.link(collection)

    rule_output_dir = os.path.join(args.output_dir, f"{rule_idx}")
    os.makedirs(rule_output_dir, exist_ok=True)

    rule_name = f"query_{rule_idx}_n" if negative else f"query_{rule_idx}"
    query_file_path = os.path.join(rule_output_dir, rule_name + ".txt")
    with open(query_file_path, "w") as f:
        f.write(query)

    examples_data = []
    seen_structures = set()
    i = 0
    retry_attempts = 0
    max_total_retries = args.resolve_attempts * num_examples

    while i < num_examples:
        scenes = call_prolog_subprocess(1, query, args.rules_prolog_file)

        if not scenes:
            retry_attempts += 1
            if retry_attempts >= max_total_retries:
                print(f"❌ Failed to generate scene {i} after too many retries.")
                break
            continue

        structure = scenes[0]
        structure_hashable = frozenset(tuple(sorted(str(x) for x in structure)))
        if structure_hashable in seen_structures:
            print(f"🔁 Duplicate structure detected for index {i}, regenerating...")
            retry_attempts += 1
            if retry_attempts >= max_total_retries:
                print(f"❌ Too many retries for scene {i}.")
                break
            continue

        seen_structures.add(structure_hashable)
        scene_name = f"{rule_idx}_{i}_n" if negative else f"{rule_idx}_{i}"
        img_path = os.path.join(rule_output_dir, scene_name + ".png")

        try:
            generate_structure(args, structure, collection)
            render_time = render(args, str(rule_idx), scene_name)
            render_time_total += render_time

            scene_objects = ZendoObject.instances
            for obj in scene_objects:
                min_bb, max_bb = obj.get_world_bounding_box()
                world_pos = obj.get_position()
                x_min, y_min, x_max, y_max = get_image_bounding_box(obj, bpy.context.scene)

                examples_data.append([
                    scene_name, img_path, rule, query, obj.name, obj.grounded,
                    obj.touching, obj.rays, obj.pointing,
                    min_bb.x, min_bb.y, min_bb.z, max_bb.x, max_bb.y, max_bb.z,
                    world_pos.x, world_pos.y, world_pos.z, x_min, y_min, x_max, y_max
                ])

            for obj in collection.objects:
                bpy.data.objects.remove(obj, do_unlink=True)
            ZendoObject.instances.clear()

            # Orphan cleanup
            for block in bpy.data.meshes:
                if block.users == 0:
                    bpy.data.meshes.remove(block)
            for block in bpy.data.materials:
                if block.users == 0:
                    bpy.data.materials.remove(block)
            for block in bpy.data.images:
                if block.users == 0:
                    bpy.data.images.remove(block)

            print(f"✅ Successfully generated scene {scene_name}.")
            i += 1

        except Exception as e:
            print(f"❌ Exception during scene generation {scene_name}: {e}")
            retry_attempts += 1
            if retry_attempts >= max_total_retries:
                print(f"❌ Too many retries for rule {rule_idx}, aborting.")
                break

            for obj in collection.objects:
                bpy.data.objects.remove(obj, do_unlink=True)
            ZendoObject.instances.clear()

    # Cleanup: remove collection at the end of generation
    if collection.name in bpy.data.collections:
        bpy.data.collections.remove(collection, do_unlink=True)
    for col in bpy.data.collections:
        if col.users == 0:
            bpy.data.collections.remove(col)
    bpy.ops.outliner.orphans_purge(do_recursive=True)

    gc.collect()

    # ✅ Write to CSV only after all scenes are complete
    if examples_data:
        csv_file_path = os.path.join(args.output_dir, f"ground_truth_{start_rule}.csv")
        with open(csv_file_path, "a", newline="") as csvfile:
            csv_writer = csv.writer(csvfile)
            csv_writer.writerows(examples_data)

    total_end = time.time()
    cpu_time = total_end - total_start - render_time_total
    return bool(examples_data), render_time_total, cpu_time

def handle_interrupt(signum, frame):
    print("🛑 Caught interrupt signal, shutting down pool and exiting.")
    shutdown_pool()
    sys.exit(1)

def main(args):
    """
    Main function to generate and render structured scenes based on specified rules.

    This function initializes the Blender scene, loads rules, generates structures
    according to Prolog queries, renders the scenes, and stores the resulting data.

    :param args: Configuration arguments for rule generation, scene creation,
                 rendering, and file paths.
    """

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
    rules, queries, n_queries = load_zendo_rules(args.zendo_rules_fixed_file)
    start_rule = getattr(args, "start_rule", 0)
    end_rule = getattr(args, "end_rule", len(rules) - 1)
    # Write CSV header
    os.makedirs(args.output_dir, exist_ok=True)
    csv_file_path = os.path.join(args.output_dir, f"ground_truth_{start_rule}.csv")
    with open(csv_file_path, "w", newline="") as csvfile:
        csv_writer = csv.writer(csvfile)
        csv_writer.writerow(["scene_name", "img_path", "rule", "query", "object_name", "grounded",
                             "touching", "rays", "pointing", "bounding_box_min_x", "bounding_box_min_y",
                             "bounding_box_min_z", "bounding_box_max_x", "bounding_box_max_y",
                             "bounding_box_max_z", "world_pos_x", "world_pos_y", "world_pos_z",
                             "image_x_min", "image_y_min", "image_x_max", "image_y_max"])

    total_gpu_time = 0.0
    total_cpu_time = 0.0
    total_failed_time = 0.0
    failed_attempts = 0

    if rules is None or len(rules) == 0:
        r = start_rule
        while r <= end_rule:
            print(f"Generating rule {r + 1}/{num_rules}...")
            # get rule in string form and query, negative query in prolog form
            rule, query, n_query = generate_rule(rules_json_file)
            print(f"Rule: {rule}")
            print(f"Query: {query}")
            print(f"Negative Query: {n_query}")
            collection = bpy.data.collections.new("Structure")
            bpy.context.scene.collection.children.link(collection)

            attempt_start = time.time()
            generated_successfully, render_time, cpu_time = generate_blender_examples(args, num_examples, r,
                                                                                      rule, query, start_rule, False)
            attempt_end = time.time()

            # If result is not true, then prolog query took to long, therefore try again
            if not generated_successfully:
                total_failed_time += (attempt_end - attempt_start)
                failed_attempts += 1
                continue

            total_gpu_time += render_time
            total_cpu_time += cpu_time

            # If bool is set for generating also scenes which doesn't fulfill the rule
            if generate_invalid_examples:
                inv_start = time.time()
                success_invalid, render_time_invalid, cpu_time_invalid = generate_blender_examples(args, num_invalid_examples,
                                                                                     r, rule, n_query, start_rule, True)
                inv_end = time.time()

                if not success_invalid:
                    total_failed_time += (inv_end - inv_start)
                    failed_attempts += 1
                else:
                    total_gpu_time += render_time_invalid
                    total_cpu_time += cpu_time_invalid

            r += 1

        print(f"\nDataset generation complete.")

        print(f"\nTime to complete: {(time.time() - start_time):.2f}s")
        print(f"Total GPU time: {total_gpu_time:.2f}s")
        print(f"Total CPU time: {total_cpu_time:.2f}s")
        print(f"Total failed attempts time: {total_failed_time:.2f}s")
        print(f"Total failed attempts: {failed_attempts}")
        print(f"Total execution iterations: {num_rules}")
    else:
        print(f"Using existing rules from fixed_zendo_rules.txt")
        i = start_rule
        while i <= end_rule:
            print(f"Generating rule {i + 1}/{len(rules)}...")
            # get rule in string form and query, negative query in prolog form
            rule = rules[i]
            query = queries[i]
            n_query = n_queries[i]
            print(f"Rule: {rule}")
            print(f"Query: {query}")
            print(f"Negative Query: {n_query}")
            collection = bpy.data.collections.new("Structure")
            bpy.context.scene.collection.children.link(collection)

            attempt_start = time.time()
            generated_successfully, render_time, cpu_time = generate_blender_examples(args, num_examples, i,
                                                                                      rule, query, start_rule, False)
            attempt_end = time.time()

            # If result is not true, then prolog query took to long, therefore try again
            if not generated_successfully:
                total_failed_time += (attempt_end - attempt_start)
                failed_attempts += 1
                i += 1
                continue

            total_gpu_time += render_time
            total_cpu_time += cpu_time

            # If bool is set for generating also scenes which doesn't fulfill the rule
            if generate_invalid_examples:
                inv_start = time.time()
                success_invalid, render_time_invalid, cpu_time_invalid = generate_blender_examples(args, num_invalid_examples,
                                                                                     i, rule, n_query, start_rule, True)
                inv_end = time.time()

                if not success_invalid:
                    total_failed_time += (inv_end - inv_start)
                    failed_attempts += 1
                    i += 1
                    continue
                
                total_gpu_time += render_time_invalid
                total_cpu_time += cpu_time_invalid

            i += 1

        print(f"\nDataset generation complete.")

        print(f"\nTime to complete: {(time.time() - start_time):.2f}s")
        print(f"Total GPU time: {total_gpu_time:.2f}s")
        print(f"Total CPU time: {total_cpu_time:.2f}s")
        print(f"Total failed attempts time: {total_failed_time:.2f}s")
        print(f"Total failed attempts: {failed_attempts}")
        print(f"Total execution iterations: {num_rules}")


if __name__ == '__main__':
    """
    Entry point for executing the rendering pipeline.

    Parses command-line arguments, loads configuration settings from a YAML file, 
    and initiates the main function to generate and render structured scenes.
    """

    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config-file", type=str, default="configs/simple_config.yml",
                        help='config file for rendering')
    parser.add_argument("--start-rule", type=int, default=None,
                    help="Start index of rules to render (inclusive)")
    parser.add_argument("--end-rule", type=int, default=None,
                    help="End index of rules to render (inclusive)")
    conf = parser.parse_args(sys.argv[sys.argv.index("--") + 1:])

    with open(conf.config_file) as f:
        args = yaml.safe_load(f.read())  # load the config file
    print(conf.start_rule, conf.end_rule)
    args = Namespace(**args)
    if conf.start_rule is not None:
        args.start_rule = conf.start_rule
    if conf.end_rule is not None:
        args.end_rule = conf.end_rule

    utils.DEBUG_PRINTING = args.debug_printing

    main(args)
