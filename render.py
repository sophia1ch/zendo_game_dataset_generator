import bpy, _cycles
import mathutils, platform
from pathlib import Path
import os, sys, argparse
import numpy as np
import random

# add directory to sys.path
script_dir = os.path.dirname(bpy.data.filepath)
if script_dir not in sys.path:
    sys.path.append(script_dir)

from blender_objects import blender_obj, check_collision
import utils


#######################################################
# Argument Parsing
#######################################################

parser = argparse.ArgumentParser()

# Input options
# TODO: do this in an extra config file...
parser.add_argument('--base_scene_blendfile', default='data/base_scene.blend',
    help="Base blender file on which all scenes are based; includes " +
          "ground plane, lights, and camera.")
parser.add_argument('--properties_json', default='data/properties.json',
    help="JSON file defining objects, materials, sizes, and colors. " +
         "The \"colors\" field maps from CLEVR color names to RGB values; " +
         "The \"sizes\" field maps from CLEVR size names to scalars used to " +
         "rescale object models; the \"materials\" and \"shapes\" fields map " +
         "from CLEVR material and shape names to .blend files in the " +
         "--object_material_dir and --shape_dir directories respectively.")
parser.add_argument('--shape_dir', default='data/shapes',
    help="Directory where .blend files for object models are stored")
parser.add_argument('--output_dir', default='output',
    help="The directory where output images will be stored. It will be " +
         "created if it does not exist.")
parser.add_argument('--output_image_file', default='test.jpg',
    help="The file name with which the output images will be stored.")
parser.add_argument('--save_blendfile', type=int, default=0,
    help="Setting --save_blendfiles 1 will cause the blender scene file for " +
         "each generated image to be stored in the directory specified by " +
         "the --output_blend_dir flag. These files are not saved by default " +
         "because they take up ~5-10MB each.")

parser.add_argument('--width', default=320, type=int,
    help="The width (in pixels) for the rendered images")
parser.add_argument('--height', default=240, type=int,
    help="The height (in pixels) for the rendered images")
parser.add_argument('--render_num_samples', default=1024, type=int,
    help="The number of samples to use when rendering. Larger values will " +
         "result in nicer images but will cause rendering to take longer.")
parser.add_argument('--render_min_bounces', default=8, type=int,
    help="The minimum number of bounces to use for rendering.")
parser.add_argument('--render_max_bounces', default=8, type=int,
    help="The maximum number of bounces to use for rendering.")
parser.add_argument('--render_tile_size', default=256, type=int,
    help="The tile size to use for rendering. This should not affect the " +
         "quality of the rendered image but may affect the speed; CPU-based " +
         "rendering may achieve better performance using smaller tile sizes " +
         "while larger tile sizes may be optimal for GPU-based rendering.")


# Parse arguments
argv = utils.extract_args()
args = parser.parse_args(argv)


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