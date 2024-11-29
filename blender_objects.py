import math, os
import bpy
import mathutils
import numpy as np
from dataclasses import dataclass

class blender_obj:
    @dataclass
    class computed_values:
        ground_radius: float
        """The smallest distance from any vertex to the object center in the xy plane"""

        local_top: float
        """Max y of the object relative to its center"""

        local_bottom: float
        """Min y of the object relative to its center"""

    def __init__(self, args, name: str, scale: float, color: list[float]):
        """
        Copy a blender object based on a given object in blender file
        :param args: Given configuration arguments of the render file
        :param name: Name of the object in blender file
        """
        # Load object from file
        filename = os.path.join(args.shape_dir, '%s.blend' % name.lower(), 'Object', name)
        bpy.ops.wm.append(filename=filename)
        # Rename and set object
        self.name = '%s_%d' % (name, get_object_count(name))
        bpy.data.objects[name].name = self.name
        self.obj = bpy.data.objects[self.name]
        self.material = self.obj.data.materials[0]

        self.args = args
        self.type = name

        self.set_color(color)

        self.obj.scale = mathutils.Vector((scale, scale, scale))
        self.force_recompute()

    def get_scaled_and_rotated_vertices(self):
        """
        Returns a list of the non-translated world space vertices of the object.
        """
        return [self.obj.rotation_quaternion@(self.obj.scale*vertex.co) for vertex in self.obj.data.vertices]

    def force_recompute(self, vertices: list[mathutils.Vector]|None=None):
        """
        Recomputes self.computed and blender properties.
        This is done using the given vertices which are expected to be returned from self.get_scaled_and_rotated_vertices
        or None, in which case self.get_scaled_and_rotated_vertices is called.
        """
        if vertices is None:
            vertices = self.get_scaled_and_rotated_vertices()
        self.computed = blender_obj.computed_values(
            ground_radius=max([math.sqrt(p.x**2 + p.y**2) for p in vertices]),
            local_top=max([p.z for p in vertices]),
            local_bottom=min([p.z for p in vertices]),
        )
        bpy.context.view_layer.update()

    def set_pose(self, x: float, y: float, rad_z: float, type: str):
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
        transformed_vertices = self.get_scaled_and_rotated_vertices()
        bottom_p = min(transformed_vertices, key=lambda p: p.z)
        #top_p = max(transformed_vertices, key=lambda p: p.z)
        center_z = self.obj.location.z
        self.obj.location = mathutils.Vector((x, y, center_z - bottom_p.z))
        
        self.force_recompute(vertices=transformed_vertices)

    def set_ontop(self, other: "blender_obj", rad_z: float|None=None):
        """
        Places self on top of the other object.
        Currently only works with upright pose type.
        Also stacking pyramids on already stacked discs might not work as expected.
        """
        # TODO: Placing a pyramid on top of a disc should compute the inner radius of the pyramid
        # at the bottom level or at the height where is matches the radius of the disc
        min_z = -self.computed.local_bottom
        location_z = min_z
        if self.type == "Pyramid":
            if other.type == "Pyramid":
                location_z = other.obj.location.z + other.computed.local_top - self.computed.local_top + self.args.stacking_offset
            elif other.type == "Disc":
                if self.computed.ground_radius/other.computed.ground_radius < 1.7:
                    location_z += other.obj.location.z + other.computed.local_top
        elif self.type == "Disc":
            location_z += other.obj.location.z + other.computed.local_top
        else:
            raise Exception("set_ontop: unhandled self type")
        location_z = max(min_z, location_z)
        rotation = other.obj.rotation_quaternion if rad_z is None else mathutils.Quaternion((0.0, 0.0, 1.0), rad_z)

        self.obj.location.x = other.obj.location.x
        self.obj.location.y = other.obj.location.y
        self.obj.location.z = location_z
        self.obj.rotation_mode = "QUATERNION"
        self.obj.rotation_quaternion = rotation

        self.force_recompute()

    def set_color(self, color: list[float]):
        r, g, b, a = color
        # Check if the material exists and uses nodes
        if not self.material:
            raise Exception("No material found.")

        if not self.material.use_nodes:
            raise Exception(f"Material '{self.material.name}' does not use nodes.")

        nodes = self.material.node_tree.nodes

        # Look for an RGB node labeled "Color"
        color_node = None
        for node in nodes:
            if node.type == 'RGB':# and (node.label == "Color" or node.name == "Color"):
                color_node = node
                break

        if not color_node:
            raise Exception("RGB node labeled 'Color' not found.")

        # Set the color
        color_node.outputs[0].default_value = (r, g, b, a)
        #print(f"Setting color to: {r:.2f}, {g:.2f}, {b:.2f}, {a:.2f}")

    def hide(self):
        self.obj.hide_render = True

    def show(self):
        self.obj.hide_render = False


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
        obj.set_pose(rnd_x, rnd_y, np.random.uniform(high=2.0*np.pi), pose_type)

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
    obj_radius = obj.computed.ground_radius

    # Iterate blender objects
    for tmp_obj in obj_list:
        if tmp_obj.name == obj.name:
            continue
        tmp_loc_x, tmp_loc_y, _ = tmp_obj.obj.location
        tmp_radius = tmp_obj.computed.ground_radius

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
