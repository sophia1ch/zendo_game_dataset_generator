import math
import bpy
import mathutils
import numpy as np


def check_collision(obj_name, obj_x, obj_y, dim_x, dim_y):
    """
    Checks if the given postion (x, y) collides with any blender object.
    Assume a circle around the center of the objects with a radius of the maximum dimension (x, y).
    The algo checks if the circles of two objects are colliding.
    :param obj_name: name of the object to check
    :param obj_x: x position of the object
    :param obj_y: x position of the object
    :param dim_x: dimension (width) of the object
    :param dim_y: dimension (height) of the object
    :return: True if collision and False if not
    """
    # Exclude Camera, Sun, Plane and the object itself
    exclude_arr = ['Camera', 'Sun', 'Plane', obj_name]
    obj_radius = max(dim_x, dim_y)

    # Iterate blender objects
    for obj_idx in range(len(bpy.data.objects)):
        tmp_obj = bpy.data.objects[obj_idx]
        if tmp_obj.name in exclude_arr:
            continue
        # get pos
        tmp_loc_x, tmp_loc_y = tmp_obj.location[:2]
        tmp_radius = max(tmp_obj.dimensions[:2])

        distance = math.sqrt((tmp_loc_x - obj_x) ** 2 + (tmp_loc_y - obj_y) ** 2)
        if distance < (tmp_radius + obj_radius):
            return True
    return False


class blender_obj:
    def __init__(self, name):
        """
        Copy a blender object based on a given object in blender file
        :param name: Name of the object in blender file
        """
        self.name = name
        self.obj = bpy.data.objects[name].copy()
        # needs to copy the data mesh, therefore you can change the materials independent of the original object
        self.obj.data = bpy.data.objects[name].data.copy()
        # copy the material so you can change it independently
        self.material = bpy.data.objects[name].data.materials[0].copy()

        bpy.context.collection.objects.link(self.obj)
        # delete the material of the copied original and set the new copied material
        self.obj.data.materials.pop(index=0)
        self.obj.data.materials.append(self.material)
        self.material.use_nodes = True

    def get_position(self):
        return self.obj.location[:2]

    def get_dimensions(self):
        return self.obj.dimensions[:2]

    def set_position(self, x, y):
        self.obj.location = mathutils.Vector((x, y, 0))

    def move(self, x, y):
        self.obj.location = self.obj.location + mathutils.Vector((x, y, 0))

    def set_random_position(self):
        """
        Sets a new position if no collision is detected
        :return:
        """
        # set random position with boundary of the ground-plane
        rnd_x, rnd_y = np.random.randint(-9, 9, 2)
        while check_collision(self.obj.name, rnd_x, rnd_y, self.obj.dimensions[0], self.obj.dimensions[1]):
            rnd_x, rnd_y = np.random.randint(-9, 9, 2)
        self.obj.location = mathutils.Vector([rnd_x, rnd_y, 0])

    def set_color(self, r, g, b, t=1):
        bsdf = self.material.node_tree.nodes.get("Principled BSDF")
        if bsdf:
            bsdf.inputs["Base Color"].default_value = (r, g, b, t)

    def hide(self):
        self.obj.hide_render = True

    def show(self):
        self.obj.hide_render = False
