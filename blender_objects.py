import math, os
import bpy
import mathutils
import numpy as np


class blender_obj:
    def __init__(self, args, name):
        """
        Copy a blender object based on a given object in blender file
        :param args: Given configuration arguments of the render file
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
        self.scale_factor = 1.0

        self.type = name

    def get_position(self):
        return self.obj.location

    def get_dimensions(self):
        return self.obj.dimensions

    def set_position(self, x, y, z):
        print("DEPRECATED: use set_pose")
        #self.obj.location = mathutils.Vector((x, y, z))

    def move(self, x, y, z):
        self.obj.location = self.obj.location + mathutils.Vector((x, y, z))

    def set_pose(self, x, y, rad_z, type):
        rotation = mathutils.Quaternion((1.0, 0.0, 0.0), 0.0)
        if type == "upright":
            pass
        elif type == "side":
            side_angle = 110.0 if self.type == "Pyramid" else 90.0
            rotation = mathutils.Quaternion((0.0, 0.0, 1.0), math.radians(45.0))
            rotation = mathutils.Quaternion((1.0, 0.0, 0.0), math.radians(side_angle))@rotation
        rotation = mathutils.Quaternion((0.0, 0.0, 1.0), rad_z)@rotation

        self.obj.rotation_mode = "QUATERNION"
        self.obj.rotation_quaternion = rotation

        # Set to ground
        self.obj.location = mathutils.Vector((0.0, 0.0, 0.0))
        rotated_vertices = [rotation@(self.scale_factor*vertex.co) for vertex in self.obj.data.vertices]
        bottom_p = min(rotated_vertices, key=lambda p: p.z)
        #top_p = max(rotated_vertices, key=lambda p: p.z)
        center_z = self.obj.location.z
        self.obj.location = mathutils.Vector((x, y, center_z - bottom_p.z))
        bpy.context.view_layer.update()

    def get_ground_radius(self):
        rotated_vertices = [self.obj.rotation_quaternion@(self.scale_factor*vertex.co) for vertex in self.obj.data.vertices]
        return max([math.sqrt(p.x**2 + p.y**2) for p in rotated_vertices])

    def set_to_ground(self):
        """
        Sets the object on the ground due to origin is center of object
        :return:
        """
        print("DEPRECATED: use set_pose")

    def set_color(self, color):
        r, g, b, a = color
        # Check if the material exists and uses nodes
        if not self.material:
            print("No material found.")
            return

        if not self.material.use_nodes:
            print(f"Material '{self.material.name}' does not use nodes.")
            return

        nodes = self.material.node_tree.nodes

        # Look for an RGB node labeled "Color"
        color_node = None
        for node in nodes:
            if node.type == 'RGB':# and (node.label == "Color" or node.name == "Color"):
                color_node = node
                break

        if not color_node:
            print("RGB node labeled 'Color' not found.")
            return

        # Set the color
        color_node.outputs[0].default_value = (r, g, b, a)
        print(f"Setting color to: {r:.2f}, {g:.2f}, {b:.2f}, {a:.2f}")

    def hide(self):
        self.obj.hide_render = True

    def show(self):
        self.obj.hide_render = False

    def scale(self, scale_factor):
        """
        Scales the object according to the given scale_factor. This factor is stored as a class variable.
        Due to simpler mathematical reason, we use this factor instead of the scale values of the "bpy.object".
        :param scale_factor: float for scaling the object
        """
        if self.scale_factor != 1.0:
            # If the object has already been scaled, scale it back to original size
            self.obj.scale = mathutils.Vector(self.obj.scale) / self.scale_factor
        # Afterwards scale with new factor
        self.obj.scale = mathutils.Vector(self.obj.scale) * scale_factor
        self.scale_factor = scale_factor
        bpy.context.view_layer.depsgraph.update()


################################################
# Functions
################################################


def set_random_position(obj: blender_obj, obj_list: list[blender_obj]):
    """
    Sets a new position for a given object on ground level if no collision is detected
    :return:
    """
    # set random position with boundary of the ground-plane
    # range given by base_scene of clevr project
    
    keep_searching = True
    while keep_searching:
        rnd_x, rnd_y = np.random.uniform(low=-3, high=3, size=2)
        pose_type = "upright" if np.random.uniform() > 0.8 else "side"
        obj.set_pose(rnd_x, rnd_y, np.random.uniform(high=np.pi), pose_type)

        keep_searching = check_collision(obj, rnd_x, rnd_y, obj_list)


def check_collision(obj: blender_obj, obj_x: float, obj_y: float, obj_list: list[blender_obj]):
    """
    Checks if the given postion (x, y) collides with any blender object in the given list of objects.
    Assume a circle around the center of the objects with a radius of the maximum dimension (x, y).
    The algo checks if the circles of two objects are colliding.
    :param obj: the object to check
    :param obj_x: x position of the object
    :param obj_y: x position of the object
    :param obj_list: list of all other objects of type blender_obj
    :return: True if collision and False if not
    """
    obj_radius = obj.get_ground_radius()#(max(obj.get_dimensions()[:2]) * obj.scale_factor) / 2

    # Iterate blender objects
    for tmp_obj in obj_list:
        if tmp_obj.name == obj.name:
            continue
        tmp_loc_x, tmp_loc_y, _ = tmp_obj.get_position()
        tmp_radius = tmp_obj.get_ground_radius()#(max(tmp_obj.get_dimensions()[:2]) * tmp_obj.scale_factor) / 2

        distance = math.sqrt((tmp_loc_x - obj_x) ** 2 + (tmp_loc_y - obj_y) ** 2)
        if distance < (tmp_radius + obj_radius):
            return True
    return False


def place_ontop(top: blender_obj, bottom: blender_obj, args):
    """
    Places a blender object ontop of another object.
    :param args: Config arguments, needed for width factor to place stacked object slightly on top
    :param top: The object to be placed on another object
    :param bottom: The object on which another object gets placed upon
    :return:
    """
    top.obj.location.x = bottom.obj.location.x
    top.obj.location.y = bottom.obj.location.y
    bottom_peak = bottom.obj.location.z + (bottom.obj.dimensions.z / 2)
    top_peak = (top.obj.dimensions.z / 2)
    width_factor = args.stacking_offset
    height_offset = (bottom_peak - top_peak) + width_factor
    top.obj.location.z = height_offset
    bpy.context.view_layer.update()



def get_object_count(name):
    count = 0
    for obj in bpy.data.objects:
        if obj.name.startswith(name):
            count += 1
    return count
