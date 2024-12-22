from enum import Enum
import math, os
import bpy
import mathutils
from mathutils import Vector, Euler, Quaternion
import numpy as np
import utils
from dataclasses import dataclass


class ZendoObject:
    instances = []
    poses = {}

    def __init__(self, args, idx: int, shape: str, color: str, pose: str = "upright"):
        """
        Initialize a Zendo object.

        :param color: Color of the object (e.g., "red", "blue", "green").
        :param size: Size of the object (e.g., "small", "medium", "large").
        :param orientation: Orientation of the object, must be a value of Orientation.
        """

        ZendoObject.instances.append(self)
        object_shapes, object_colors, object_sizes = utils.read_properties_json(args.properties_json)
        color_code = object_colors[color]
        # Load object from file
        filename = os.path.join(args.shape_dir, '%s.blend' % shape.lower(), 'Object', shape)
        bpy.ops.wm.append(filename=filename)
        # Rename and set object
        #self.name = '%s_%d' % (name, get_object_count(name))
        unique_name = f"{idx}_{shape}"
        bpy.data.objects[shape].name = unique_name
        self.obj = bpy.data.objects[unique_name]
        self.material = self.obj.data.materials[0]

        self.args = args
        self.shape = shape.lower()
        self.idx = idx
        self.name = unique_name
        self.pose = pose
        self.set_color(color_code)
        self.color = color
        self.set_pose(pose)
        self.grounded = True
        self.touching = {
            'left': None,
            'right': None,
            'front': None,
            'back': None,
            'top': None,
            'bottom': None,
        }
        self.nests = None
        self.pointing = None

        self.rays = []
        #ray_path = os.path.join(args.shape_dir, '%s.blend' % name.lower(), 'Object')

    def remove(self):
        bpy.data.objects.remove(self.obj, do_unlink=True)
        ZendoObject.instances.remove(self)

    def get_namestring(self):
        return f"{self.color} {self.shape} {self.pose}"

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
        bpy.context.view_layer.update()

    def set_position(self, position: Vector):
        self.obj.location = position

    def set_position_xy(self, position: Vector):
        self.obj.location.x = position.x
        self.obj.location.y = position.y

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
        ground_ofset = center_z - bottom_p.z
        self.obj.location.z = ground_ofset
        return ground_ofset

    def check_pose(self, pose):
        if pose not in self.__class__.poses:
            raise ValueError(
                f"{pose} is not a valid pose for {self.__class__.__name__}. "
                f"Valid poses are: {[p for p in self.__class__.poses]}"
            )

    def get_free_face(self):
        avail = [f for f in self.touching if self.touching.get(f) is None and f not in ('top', 'bottom')]
        return avail

    def rotate(self, axis: str, rad: float):
        self.obj.rotation_mode = "QUATERNION"
        if axis.upper() == 'X':
            rotation = Quaternion(Vector((1.0, 0.0, 0.0)), math.radians(rad))
        elif axis.upper() == 'Y':
            rotation = Quaternion(Vector((0.0, 1.0, 0.0)), math.radians(rad))
        elif axis.upper() == 'Z':
            rotation = Quaternion(Vector((0.0, 0.0, 1.0)), math.radians(rad))
        else:
            raise ValueError(
                f"{axis} is an invalid axis!"
            )
        self.obj.rotation_quaternion = rotation @ self.obj.rotation_quaternion
        self.set_to_ground()
        bpy.context.view_layer.update()

    def set_rotation_quaternion(self, rotation: Quaternion):
        self.obj.rotation_mode = "QUATERNION"
        self.obj.rotation_quaternion = rotation

    def rotate_z(self, rad: float):
        self.obj.rotation_mode = "QUATERNION"
        rotation = Quaternion(Vector((0, 0, 1)), math.radians(rad))
        self.obj.rotation_quaternion = rotation @ self.obj.rotation_quaternion
        self.set_to_ground()
        bpy.context.view_layer.update()

    def move(self, vec: Vector):
        self.obj.location += vec

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
        "flat": Quaternion(Vector((1.0, 0.0, 0.0)), math.radians(106))
    }

    def __init__(self, args, idx: int, color: str, pose: str):
        super(Pyramid, self).__init__(args, idx,"Pyramid", color, pose)
        self.nested = None

    def get_rays(self):
        mesh = self.obj.data
        tip = max(mesh.vertices, key=lambda v: v.co.z)  # Highest vertex in local Z
        if self.pose == 'flat' and self.grounded:
            origin = project_to_xy(self.obj.matrix_world @ tip.co)
            origin.z = 0.01  # Ground offset because raycasting on 0 Z coordinate doesn't work reliably
            direction = project_to_xy(self.obj.matrix_world.to_3x3() @ mathutils.Vector((0, 0, 1))).normalized()
        else:
            origin = self.obj.matrix_world @ tip.co
            direction = self.obj.matrix_world.to_3x3() @ mathutils.Vector((0, 0, 1)).normalized()
        return [(origin, direction)]


class Block(ZendoObject):
    poses = {
        "upright": Quaternion(Vector((0.0, 0.0, 0.0)), math.radians(0)),
        "upside_down": Quaternion(Vector((0.0, 1.0, 0.0)), math.radians(180)),
        "flat": Quaternion(Vector((0.0, 1.0, 0.0)), math.radians(90))
    }

    def __init__(self, args, idx: int, color: str, pose: str):
        super(Block, self).__init__(args, idx,"Block", color, pose)

    def get_rays(self):
        mesh = self.obj.data
        highest_z = max(v.co.z for v in mesh.vertices)
        top_vertices = [v for v in mesh.vertices if v.co.z == highest_z]

        if len(top_vertices) < 4:
            raise ValueError("Not enough vertices to interpolate rays.")

        v1_world = self.obj.matrix_world @ top_vertices[0].co
        v2_world = self.obj.matrix_world @ top_vertices[1].co
        v3_world = self.obj.matrix_world @ top_vertices[2].co
        v4_world = self.obj.matrix_world @ top_vertices[3].co

        rays = []

        # Interpolate within the topmost face
        for i in range(self.args.ray_interpolation + 2):  # +2 includes the edges
            t1 = i / (self.args.ray_interpolation + 1)
            # Interpolate along the two opposite edges
            edge1_point = v1_world.lerp(v2_world, t1)
            edge2_point = v4_world.lerp(v3_world, t1)

            for j in range(self.args.ray_interpolation + 2):
                t2 = j / (self.args.ray_interpolation + 1)
                # Interpolate between the two edges to create grid points
                interpolated_point = edge1_point.lerp(edge2_point, t2)

                # Generate ray from the interpolated point
                origin = interpolated_point
                direction = self.obj.matrix_world.to_3x3() @ mathutils.Vector((0, 0, 1)).normalized()

                rays.append((origin, direction))

        for v in top_vertices:
            origin = self.obj.matrix_world @ v.co
            direction = self.obj.matrix_world.to_3x3() @ mathutils.Vector((0, 0, 1)).normalized()
            rays.append((origin, direction))
        return rays


class Wedge(ZendoObject):
    poses = {
        "upright": Quaternion(Vector((0.0, 0.0, 0.0)), math.radians(0)),
        "cheesecake": Quaternion(Vector((0.0, 1.0, 0.0)), math.radians(90)),
        "flat": Quaternion(Vector((1.0, 0.0, 0.0)), math.radians(106.1))
    }

    def __init__(self, args, idx: int, color: str, pose: str):
        super(Wedge, self).__init__(args, idx, "Wedge", color, pose)

    def get_rays(self):
        mesh = self.obj.data
        highest_z = max(v.co.z for v in mesh.vertices)
        top_vertices = [v for v in mesh.vertices if v.co.z == highest_z]

        if len(top_vertices) < 2:
            raise ValueError("Not enough vertices to interpolate rays.")

        v1_world = self.obj.matrix_world @ top_vertices[0].co
        v2_world = self.obj.matrix_world @ top_vertices[1].co

        rays = []
        for i in range(self.args.ray_interpolation + 2):  # +2 includes the start and end points
            t = i / (self.args.ray_interpolation + 1)  # Interpolation factor (0 to 1)
            interpolated_point = v1_world.lerp(v2_world, t)  # Linear interpolation

            if self.pose == 'flat' and self.grounded:
                origin = project_to_xy(interpolated_point)
                origin.z = 0.01  # Ground offset because raycasting on 0 Z coordinate doesn't work reliably
                direction = project_to_xy(self.obj.matrix_world.to_3x3() @ mathutils.Vector((0, 0, 1))).normalized()
            else:
                origin = interpolated_point
                direction = self.obj.matrix_world.to_3x3() @ mathutils.Vector((0, 0, 1)).normalized()

            rays.append((origin, direction))
        return rays


def get_object_count(name):
    count = 0
    for obj in bpy.data.objects:
        if obj.name.startswith(name):
            count += 1
    return count

def get_object(idx: int):
    obj = [o for o in ZendoObject.instances if o.idx == idx][0]
    return obj

def project_to_xy(vec):
    """Project a 3D vector to the XY plane."""
    return mathutils.Vector((vec.x, vec.y, 0))

def get_from_blender_obj(obj):
    obj = [o for o in ZendoObject.instances if o.obj is obj]
    if len(obj) > 0:
        return obj[0]
    else:
        return None