import bpy
import mathutils
from mathutils import Vector
from zendo_objects import ZendoObject
import math

face_map = {
        "front": ('X', 1),
        "back": ('X', -1),
        "right": ('Y', 1),
        "left": ('Y', -1),
        "top": ('Z', 1),
        "bottom": ('Z', -1),
    }


def rel_pointing(target: ZendoObject, relatives: list[ZendoObject]):
    pass


def rel_touching(object_1: ZendoObject, object_2: ZendoObject, face: str):
    """
    Place object_1 against object_2 along the specified axis.

    :param object_1: Blender object to move.
    :param object_2: Blender object to align with.
    :param face: The face name as a string ('front', 'back', 'right', 'left', 'top').
    """
    # Ensure the requested face is valid
    if face not in face_map:
        raise ValueError(
            f"{face} is not a valid face! "
            f"Valid faces are: {[f for f in face_map]}"
        )

    # Ensure the face of the second object is free
    if object_2.get_touching()[face] != None:
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

