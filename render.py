import bpy, _cycles
import mathutils, platform
from pathlib import Path
import os
import random

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
    d["use"] = 1 # Using all devices, include GPU and CPU
    print(d["name"], d["use"])


pyramid_1 = bpy.data.objects['Cone-S']

new_pyramid = pyramid_1.copy()
bpy.context.collection.objects.link(new_pyramid)

vec = mathutils.Vector((1, 0, 0))

new_pyramid.location = new_pyramid.location + vec

new_pyramid.data.materials[0].node_tree.nodes['Principled BSDF'].inputs["Base Color"].default_value = (0, 1, 1, 1)

#pyramid_1.hide_render = True


bpy.ops.render.render(True)
img_dirpath = Path(bpy.path.abspath("//output"))
img_dirpath.mkdir(exist_ok=True)
img_path = img_dirpath.joinpath("test.jpg")
rendered_image = bpy.data.images["Render Result"]
rendered_image.save_render(filepath=str(img_path))