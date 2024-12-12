from enum import Enum
import math, os
import bpy
import mathutils
from mathutils import Vector, Euler, Quaternion
import numpy as np
from dataclasses import dataclass


class ZendoObject:
    @dataclass
    class computed_values:
        ground_radius: float
        """The smallest distance from any vertex to the object center in the xy plane"""

        local_top: float
        """Max y of the object relative to its center"""

        local_bottom: float
        """Min y of the object relative to its center"""
    poses = {}
    def __init__(self, args, name: str, scale: float, color: list[float], pose: str = "upright"):
        """
        Initialize a Zendo object.

        :param color: Color of the object (e.g., "red", "blue", "green").
        :param size: Size of the object (e.g., "small", "medium", "large").
        :param orientation: Orientation of the object, must be a value of Orientation.
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
        self.pose = pose
        self.set_color(color)
        self.set_pose(pose)
        self.set_to_ground()
        self.grounded = True
        self.touching = {
            'left': None,
            'right': None,
            'front': None,
            'back': None,
            'top': None,
            'bottom': None,
        }

        self.rays = []
        ray_path = os.path.join(args.shape_dir, '%s.blend' % name.lower(), 'Object')

    def set_touching(self, face: str, obj):
        self.touching[face] = obj

    def get_touching(self):
        return self.touching

    def set_pose(self, pose):
        self.check_pose(pose)
        self.obj.rotation_mode = "QUATERNION"
        self.obj.rotation_quaternion = self.__class__.poses[pose]
        self.pose = pose
        self.set_to_ground()
        self.update_rays(pose)
        bpy.context.view_layer.update()
        print("test")

    def set_position(self, position: Vector):
        self.obj.location = position

    def get_position(self):
        return self.obj.location

    def get_scaled_and_rotated_vertices(self):
        """
        Returns a list of the non-translated world space vertices of the object.
        """
        return [self.obj.rotation_quaternion @ (self.obj.scale * vertex.co) for vertex in self.obj.data.vertices]

    def set_to_ground(self):
        self.obj.location = mathutils.Vector((0.0, 0.0, 0.0))
        transformed_vertices = self.get_scaled_and_rotated_vertices()
        bottom_p = min(transformed_vertices, key=lambda p: p.z)
        # top_p = max(transformed_vertices, key=lambda p: p.z)
        center_z = self.obj.location.z
        self.obj.location.z = center_z - bottom_p.z

    def check_pose(self, pose):
        if pose not in self.__class__.poses:
            raise ValueError(
                f"{pose} is not a valid pose for {self.__class__.__name__}. "
                f"Valid poses are: {[p for p in self.__class__.poses]}"
            )

    def force_recompute(self, vertices: list[mathutils.Vector] | None = None):
        """
        Recomputes self.computed and blender properties.
        This is done using the given vertices which are expected to be returned from self.get_scaled_and_rotated_vertices
        or None, in which case self.get_scaled_and_rotated_vertices is called.
        """
        if vertices is None:
            vertices = self.get_scaled_and_rotated_vertices()
        self.computed = ZendoObject.computed_values(
            ground_radius=max([math.sqrt(p.x ** 2 + p.y ** 2) for p in vertices]),
            local_top=max([p.z for p in vertices]),
            local_bottom=min([p.z for p in vertices]),
        )
        bpy.context.view_layer.update()

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
            if node.type == 'RGB':  # and (node.label == "Color" or node.name == "Color"):
                color_node = node
                break

        if not color_node:
            raise Exception("RGB node labeled 'Color' not found.")

        # Set the color
        color_node.outputs[0].default_value = (r, g, b, a)
        # print(f"Setting color to: {r:.2f}, {g:.2f}, {b:.2f}, {a:.2f}")

    def update_rays(self, pose):
        pass

    def get_world_bounding_box(self):
        """
        Calculate the world-space bounding box of an object.
        This accounts for rotation, scale, and position.

        :param self: The ZendoObject object.
        :return: A tuple of min and max coordinates (min_x, max_x, min_y, max_y, min_z, max_z).
        """
        bpy.context.view_layer.update()

        # Get the world-space coordinates of the object's vertices
        world_vertices = [self.obj.matrix_world @ mathutils.Vector(v.co) for v in self.obj.data.vertices]

        # Find the min and max values along each axis
        min_coords = mathutils.Vector((min(v.x for v in world_vertices),
                                       min(v.y for v in world_vertices),
                                       min(v.z for v in world_vertices)))
        max_coords = mathutils.Vector((max(v.x for v in world_vertices),
                                       max(v.y for v in world_vertices),
                                       max(v.z for v in world_vertices)))

        return min_coords, max_coords


class Pyramid(ZendoObject):
    poses = {
        "upright": Quaternion(Vector((0.0, 0.0, 0.0)), math.radians(0)),
        "flat": Quaternion(Vector((1.0, 0.0, 0.0)), math.radians(110))
    }
    def __init__(self, args, scale: float, color: list[float], pose: str):
        super(Pyramid, self).__init__(args, "Pyramid", scale, color, pose)


class Block(ZendoObject):
    poses = {
        "upright": Quaternion(Vector((0.0, 0.0, 0.0)), math.radians(0)),
        "upside_down": Quaternion(Vector((0.0, 1.0, 0.0)), math.radians(180)),
        "flat": Quaternion(Vector((0.0, 1.0, 0.0)), math.radians(90))
    }
    def __init__(self, args, scale: float, color: list[float], pose: str):
        super(Block, self).__init__(args, "Block", scale, color, pose)


class Wedge(ZendoObject):
    poses = {
        "upright": Quaternion(Vector((0.0, 0.0, 0.0)), math.radians(0)),
        "cheesecake": Quaternion(Vector((0.0, 1.0, 0.0)), math.radians(90)),
        "flat": Quaternion(Vector((1.0, 0.0, 0.0)), math.radians(112))
    }
    def __init__(self, args, scale: float, color: list[float], pose: str):
        super(Wedge, self).__init__(args, "Wedge", scale, color, pose)

    def update_rays(self, pose):
        pass


def get_object_count(name):
    count = 0
    for obj in bpy.data.objects:
        if obj.name.startswith(name):
            count += 1
    return count
