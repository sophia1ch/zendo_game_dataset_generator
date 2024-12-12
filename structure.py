import bpy
import mathutils
from zendo_objects import ZendoObject
import math


def rel_pointing(target: ZendoObject, relatives: list[ZendoObject]):
    pass


def rel_touching(object_1: ZendoObject, object_2: ZendoObject, axis='X', direction=1):
    """
    Place object_1 against object_2 along the specified axis.

    :param object_1: Blender object to move.
    :param object_2: Blender object to align with.
    :param axis: The axis along which to align ('X', 'Y', 'Z').
    :param direction: Direction to move (1 for positive, -1 for negative).
    """
    # Ensure both objects are properly updated
    bpy.context.view_layer.update()

    # Get the bounding boxes of both objects
    bbox1 = [mathutils.Vector(corner) @ object_1.obj.matrix_world for corner in object_1.obj.bound_box]
    bbox2 = [mathutils.Vector(corner) @ object_2.obj.matrix_world for corner in object_2.obj.bound_box]

    # Extract axis index
    axis_index = 'XYZ'.index(axis.upper())

    # Get the min and max values for each object's bounding box
    obj1_min = min([v[axis_index] for v in bbox1])
    obj1_max = max([v[axis_index] for v in bbox1])
    obj2_min = min([v[axis_index] for v in bbox2])
    obj2_max = max([v[axis_index] for v in bbox2])

    # Compute the offset required to touch object_2
    if direction > 0:
        offset = obj2_max - obj1_min
    else:
        offset = obj2_min - obj1_max

    # Move object_1 to touch object_2
    object_1.obj.location[axis_index] += offset