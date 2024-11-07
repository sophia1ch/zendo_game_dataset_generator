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
# Init
#######################################################

bpy.data.scenes[0].render.engine = "CYCLES"

# Set the device_type
system = platform.system()

if system == "Darwin":  # macOS
    bpy.context.preferences.addons["cycles"].preferences.compute_device_type = "METAL"
elif system in ["Windows", "Linux"]:  # Windows or Linux
    bpy.context.preferences.addons["cycles"].preferences.compute_device_type = "OPTIX"
else:
    print("Unsupported OS for GPU rendering configuration")

# Set the device and feature set
bpy.context.scene.cycles.device = "GPU"
bpy.context.preferences.addons["cycles"].preferences.get_devices()
print(bpy.context.preferences.addons["cycles"].preferences.compute_device_type)
for d in bpy.context.preferences.addons["cycles"].preferences.devices:
    d["use"] = 1  # Using all devices, include GPU and CPU
    print(d["name"], d["use"])


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