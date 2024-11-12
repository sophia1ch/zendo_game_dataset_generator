import bpy, _cycles
import mathutils, platform
from pathlib import Path
import os, sys
import numpy as np
import random

# add directory to sys.path
script_dir = os.path.dirname(bpy.data.filepath)
if script_dir not in sys.path:
    sys.path.append(script_dir)

from blender_objects import blender_obj, check_collision


#######################################################
# Initialize render settings
#######################################################

# Set Cycles as the render engine
bpy.context.scene.render.engine = "CYCLES"

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
# Main
#######################################################

# Initialize zendo objects
sphere = blender_obj('Sphere')
sphere.set_position(0, 0)
sphere.set_color(0.2, 0.2, 0.2)

cone_l = blender_obj('Cone-L')
cone_l.set_position(-1, 5)
cone_l.set_color(0, 0.1, 0.6)
print(cone_l.get_position())

cone_m = blender_obj('Cone-M')
cone_m.set_position(0, 2)
cone_m.set_color(0.9, 0, 0.7)

cone_s = blender_obj('Cone-S')
cone_s.set_position(0, -2)
cone_s.set_color(0, 0.6, 0.2)

# Create random cones
num = 20
cone_arr = []
col = np.random.rand(num, 3)
for i in range(num):
    c = blender_obj('Cone-S')
    c.set_random_position()
    c.set_color(col[i, 0], col[i, 1], col[i, 2])


#######################################################
# Render
#######################################################

bpy.ops.render.render(True)
img_dirpath = Path(bpy.path.abspath("//output"))
img_dirpath.mkdir(exist_ok=True)
img_path = img_dirpath.joinpath("test.jpg")
rendered_image = bpy.data.images["Render Result"]
rendered_image.save_render(filepath=str(img_path))