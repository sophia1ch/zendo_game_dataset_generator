import bpy
import mathutils
from mathutils import Vector
from zendo_objects import ZendoObject, Pyramid, Block, Wedge
import math
import copy

face_map = {
        "front": ('X', 1),
        "back": ('X', -1),
        "right": ('Y', 1),
        "left": ('Y', -1),
        "top": ('Z', 1),
        "bottom": ('Z', -1),
    }


def check_beneath(object: ZendoObject):
    beneath_objects = []

    # Get the bounding box of the target object
    target_min, target_max = object.get_world_bounding_box()

    # Iterate over all objects in the scene
    for obj in bpy.data.objects:
        # Skip the target object itself
        if obj == object.obj:
            continue

        # Calculate the bounding box for the current object
        obj_min, obj_max = obj.calculate_world_bounding_box()

        # Check if the current object is beneath the target object
        if obj_max.z <= target_min.z:
            beneath_objects.append(obj)

    return beneath_objects


def rel_touching(object_1: ZendoObject, object_2: ZendoObject, face: str):
    """
    Place object_1 against object_2 along the specified axis.

    :param object_1: Blender object to move.
    :param object_2: Blender object to align with.
    :param face: The face of object_2 to align object_1 with as a string ('front', 'back', 'right', 'left', 'top').
    """
    # Ensure the requested face is valid
    if face not in face_map:
        raise ValueError(
            f"{face} is not a valid face! "
            f"Valid faces are: {[f for f in face_map]}"
        )

    # Ensure the face of the second object is free
    if object_2.get_touching()[face] is not None:
        raise ValueError(
            f"{face} of {object_2.name} is already occupied!"
        )

    # Ensure both objects are properly updated
    axis, direction = face_map.get(face.lower(), None)
    loc_object_2 = object_2.get_position()
    object_1.set_position(Vector((loc_object_2[0], loc_object_2[1], object_1.get_position()[2])))

    obj1_min, obj1_max = object_1.get_world_bounding_box()
    obj2_min, obj2_max = object_2.get_world_bounding_box()

    # Get the axis index ('X' = 0, 'Y' = 1, 'Z' = 2)
    axis_index = 'XYZ'.index(axis.upper())

    # Calculate the offset to align the objects
    if direction > 0:
        offset = obj2_max[axis_index] - obj1_min[axis_index]
    else:
        offset = obj2_min[axis_index] - obj1_max[axis_index]

    # Move object_1 to touch object_2
    object_1.obj.location[axis_index] += offset
    object_2.set_touching(face, object_1)
    object_1_face = list(face_map.keys())[list(face_map.values()).index((axis, direction*(-1)))]
    object_1.set_touching(object_1_face, object_2)

    if face == "top":
        object_1.grounded = False


def rel_nested(object_1: ZendoObject, object_2: Pyramid):
    """
    Nests object_2 inside object_1, only pyramids can be nested inside other objects

    :param object_1: Blender object to nest inside.
    :param object_2: Blender object to nest.
    """
    # Move the first object inside the second one
    obj_2_pos = object_2.get_position()
    object_1.set_position(obj_2_pos)

    # Apply the same rotation to the first object
    obj_2_rot = object_2.obj.rotation_quaternion
    object_1.set_rotation_quaternion(obj_2_rot)
    top_vector = object_2.get_top_vector()

    # Move the first object alongside the top vector for offset
    scaled_vector = top_vector * 0.4
    object_1.move(scaled_vector)

    # Update properties of objects to reflect relation
    object_2.nested = object_1
    object_1.nests = object_2
    object_2.touching["top"] = object_1
    object_1.touching["bottom"] = object_2
    #object_1.set_to_ground()


def rel_weird(object_1: ZendoObject, object_2: ZendoObject, face: str):
    pass


def rel_pointing(object_1: ZendoObject, target: ZendoObject):
    """
    Points object_1 towards object_2

    :param object_1: Blender object to point towards object_2.
    :param object_2: Blender object 2.
    """
    # Ensure the scene is updated
    bpy.context.view_layer.update()


    origin = object_1.obj.matrix_world.translation

    tip_vector = object_1.get_top_vector().normalized()
    tip_vector_xy = Vector((tip_vector.x, tip_vector.y, 0)).normalized()

    target_position = copy.deepcopy(target.obj.matrix_world.translation)
    target_direction_xy = mathutils.Vector((target_position.x - origin.x,
                                            target_position.y - origin.y,
                                            0)).normalized()

    # Compute the rotation angle in the XY plane
    rotation_angle = tip_vector_xy.angle(target_direction_xy)

    # Determine the rotation direction (clockwise or counterclockwise)
    cross_z = tip_vector_xy.cross(target_direction_xy).z
    if cross_z < 0:
        rotation_angle = -rotation_angle

    # Create a quaternion for rotation around the world Z-axis
    rotation_quaternion = mathutils.Quaternion(Vector((0, 0, 1)), rotation_angle)

    # Apply the rotation while preserving other rotations
    object_1.obj.rotation_mode = 'QUATERNION'
    object_1.obj.rotation_quaternion = rotation_quaternion @ object_1.obj.rotation_quaternion

    object_1.pointing = target
