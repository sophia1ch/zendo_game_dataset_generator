import math, os
import bpy
import mathutils
import numpy as np


def check_collision(obj_name, obj_x, obj_y, dim_x, dim_y):
    # TODO: isn't working yet due to changes
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
        print(tmp_obj)
        if tmp_obj.name in exclude_arr:
            continue
        # get pos
        tmp_loc_x, tmp_loc_y = tmp_obj.location[:2]
        tmp_radius = max(tmp_obj.dimensions[:2])

        distance = math.sqrt((tmp_loc_x - obj_x) ** 2 + (tmp_loc_y - obj_y) ** 2)
        if distance < (tmp_radius + obj_radius):
            return True
    return False


def get_object_count(name):
    count = 0
    for obj in bpy.data.objects:
        if obj.name.startswith(name):
            count += 1
    return count


class blender_obj:
    def __init__(self, args, name):
        """
        Copy a blender object based on a given object in blender file
        :param name: Name of the object in blender file
        """
        # Load object from file
        filename = os.path.join(args.shape_dir, '%s.blend' % name, 'Object', name)
        bpy.ops.wm.append(filename=filename)
        # Rename and set object
        self.name = '%s_%d' % (name, get_object_count(name))
        bpy.data.objects[name].name = self.name
        self.obj = bpy.data.objects[self.name]
        self.material = self.obj.data.materials[0]

    def get_position(self):
        return self.obj.location

    def get_dimensions(self):
        return self.obj.dimensions

    def set_position(self, x, y, z):
        self.obj.location = mathutils.Vector((x, y, z))

    def move(self, x, y, z):
        self.obj.location = self.obj.location + mathutils.Vector((x, y, z))

    def set_random_position(self):
        # TODO: isn't working yet
        """
        Sets a new position if no collision is detected
        :return:
        """
        # set random position with boundary of the ground-plane
        rnd_x, rnd_y = np.random.randint(-3, 3, 2)
        while check_collision(self.obj.name, rnd_x, rnd_y, self.obj.dimensions[0], self.obj.dimensions[1]):
            rnd_x, rnd_y = np.random.randint(-3, 3, 2)
        self.obj.location = mathutils.Vector([rnd_x, rnd_y, 0])

    def set_color(self, color):
        r, g, b, t = color
        # TODO: setting the color is not working for different materials if the color is set with different shaders?
        bsdf = self.material.node_tree.nodes.get("Glass BSDF")
        print(bsdf)
        if bsdf:
            bsdf.inputs["Color"].default_value = (r, g, b, t)

    def hide(self):
        self.obj.hide_render = True

    def show(self):
        self.obj.hide_render = False

    def scale(self):
        # TODO: implement
        pass