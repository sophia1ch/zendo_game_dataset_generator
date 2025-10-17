import time
import os
import sys
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import argparse
from argparse import Namespace
import csv
import gc
import yaml
import ast
import platform
from zendo_objects import *
from generate import generate_structure, get_image_bounding_box
from encoding import convert_to_tensor


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

def generate_blender_examples(args, scene):
    """
    Generates Blender scene.
    """

    total_start = time.time()
    render_time_total = 0.0
    print(f"Generating scene: {scene}")

    # Create a new collection internally
    collection = bpy.data.collections.new("Structure")
    bpy.context.scene.collection.children.link(collection)

    rule_output_dir = args.output_dir
    os.makedirs(rule_output_dir, exist_ok=True)

    rule_name = f"query"
    query_file_path = os.path.join(rule_output_dir, rule_name + ".txt")
    with open(query_file_path, "w") as f:
        f.write(str(scene))

    scene_name = f"{0}"
    img_path = os.path.join(rule_output_dir, scene_name + ".png")
    examples_data = []
    try:
        generate_structure(args, scene, collection, grounded="grounded" in scene)
        render_time = render(args, str(0), scene_name)
        render_time_total += render_time

        scene_objects = ZendoObject.instances
        for obj in scene_objects:
            x_min, y_min, x_max, y_max = get_image_bounding_box(obj, bpy.context.scene)

            examples_data.append([
                obj.name, obj.touching, obj.pointing,
                x_min, y_min, x_max, y_max
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

    except Exception as e:
        print(f"❌ Exception during scene generation {scene_name}: {e}")
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

    if examples_data:
        convert_to_tensor(examples_data, os.path.join(rule_output_dir, scene_name + ".pt"))

    total_end = time.time()
    cpu_time = total_end - total_start - render_time_total
    return bool(examples_data), render_time_total, cpu_time

def main(args):
      """
      Main function to generate and render structured scenes based on specified rules.

      This function initializes the Blender scene, loads rules, generates structures
      according to Prolog queries, renders the scenes, and stores the resulting data.

      :param args: Configuration arguments for rule generation, scene creation,
                 rendering, and file paths.
      """

      start_time = time.time()
      script_dir = os.path.dirname(bpy.data.filepath)
      if script_dir not in sys.path:
            sys.path.append(script_dir)

      bpy.ops.wm.open_mainfile(filepath=args.base_scene_blendfile)
      # Write CSV header
      os.makedirs(args.output_dir, exist_ok=True)

      total_gpu_time = 0.0
      total_cpu_time = 0.0
      total_failed_time = 0.0
      failed_attempts = 0
      try:
            scene = ast.literal_eval(args.scene)
      except Exception as e:
            print("Failed to parse argument using ast.literal_eval:", e)
            return
      print(f"Generating scene...")
      collection = bpy.data.collections.new("Structure")
      bpy.context.scene.collection.children.link(collection)

      attempt_start = time.time()
      generated_successfully, render_time, cpu_time = generate_blender_examples(args, scene)
      attempt_end = time.time()

      # If result is not true, then prolog query took to long, therefore try again
      if not generated_successfully:
            total_failed_time += (attempt_end - attempt_start)
            failed_attempts += 1

      total_gpu_time += render_time
      total_cpu_time += cpu_time

      print(f"\nDataset generation complete.")

      print(f"\nTime to complete: {(time.time() - start_time):.2f}s")
      print(f"Total GPU time: {total_gpu_time:.2f}s")
      print(f"Total CPU time: {total_cpu_time:.2f}s")
      print(f"Total failed attempts time: {total_failed_time:.2f}s")
      print(f"Total failed attempts: {failed_attempts}")


if __name__ == '__main__':
    """
    Entry point for executing the rendering pipeline.

    Parses command-line arguments, loads configuration settings from a YAML file, 
    and initiates the main function to generate and render structured scenes.
    """

    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config-file", type=str, default="configs/simple_config.yml",
                        help='config file for rendering')
    parser.add_argument("--scene", type=str, default=None,
                    help="Scene to render")
    conf = parser.parse_args(sys.argv[sys.argv.index("--") + 1:])

    with open(conf.config_file) as f:
        args = yaml.safe_load(f.read())  # load the config file
    print(conf.scene)
    args = Namespace(**args)
    if conf.scene is not None:
        args.scene = conf.scene

    utils.DEBUG_PRINTING = args.debug_printing

    main(args)