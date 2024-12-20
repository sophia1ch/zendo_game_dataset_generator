import ast
import re

import zendo_objects
import utils
from math import cos, sin, pi
from structure import *
import bpy
import random
from mathutils import Vector


def is_point_in_triangle(origin, direction, triangle_coords):
    """
    Checks if the ray intersects a triangle in the XY plane.
    :param origin: The origin of the ray (observer's position)
    :param direction: The normalized direction of the ray
    :param triangle_coords: The three points of the triangle in 2D space
    :return: True if the ray intersects the triangle, False otherwise
    """
    # Project the triangle vertices into 2D space
    v0, v1, v2 = triangle_coords

    # Using a simple method like the Barycentric coordinate method to check if the point is inside
    # Check if the ray intersects the triangle in the XY plane by projecting onto the plane
    edge1 = v1 - v0
    edge2 = v2 - v0
    h = direction.cross(edge2)
    a = edge1.dot(h)

    if abs(a) < 1e-8:
        return False

    f = 1.0 / a
    s = origin - v0
    u = f * s.dot(h)

    if u < 0.0 or u > 1.0:
        return False

    q = s.cross(edge1)
    v = f * direction.dot(q)

    if v < 0.0 or u + v > 1.0:
        return False

    t = f * edge2.dot(q)
    return t > 0  # Check if the intersection is in front of the observer

def bounding_box_overlap(origin, direction, target_obj):
    """
    Check if a ray from the origin in the given direction intersects
    with the target object's bounding box in the XY plane.
    """
    bbox_corners = [project_to_xy(target_obj.matrix_world @ Vector(corner)) for corner in target_obj.bound_box]
    bbox_min = mathutils.Vector((min(c.x for c in bbox_corners), min(c.y for c in bbox_corners), 0))
    bbox_max = mathutils.Vector((max(c.x for c in bbox_corners), max(c.y for c in bbox_corners), 0))

    # Simple box projection check
    ray_end = origin + direction * 1000  # Extend ray far enough
    return (
            bbox_min.x <= ray_end.x <= bbox_max.x and
            bbox_min.y <= ray_end.y <= bbox_max.y
    )

def project_to_xy(vec):
    """Project a 3D vector to the XY plane."""
    return mathutils.Vector((vec.x, vec.y, 0))


def world_space_coords(obj, verts):
    """Convert local coordinates of vertices to world space."""
    return [project_to_xy(obj.matrix_world @ vert.co) for vert in verts]

def check_pointing(observer: ZendoObject):
    """
    Checks if the given zendo object points towards an object
    :param observer: The zendo object to check
    :return: A list of object it currently points towards
    """
    bpy.context.view_layer.update()
    results = []


    for target in ZendoObject.instances:
        if target is observer:
            continue

        if observer.shape == 'pyramid':
            origin, direction = observer.get_rays()
            direction = direction.normalized()
            origin.z = 0.5
            direction.z = 0.5
            
            for target_face in target.obj.data.polygons:

                target_verts = [target.obj.data.vertices[v] for v in target_face.vertices]
                target_coords = [target.obj.matrix_world @ vert.co for vert in target_verts]

                if len(target_coords) == 3:  # Triangle face
                    # Check intersection with the triangle in the XY plane
                    if is_point_in_triangle(origin, direction, target_coords):
                        dist = (target_coords[0] - origin).length
                        results.append((target, dist))

                elif len(target_coords) == 4:  # Square face
                    # Check intersection with the square in the XY plane
                    for coord in target_coords:
                        to_target = (coord - origin).normalized()
                        dot_product = to_target.dot(direction)
                        if dot_product > 0.95:  # Adjust for threshold tolerance
                            dist = (coord - origin).length
                            results.append((target, dist))
                            break

        # Sort objects by proximity

    results.sort(key=lambda x: x[1])


    return [obj for obj, _ in results]


def check_collision(zendo_object: ZendoObject, omit: ZendoObject = None):
    # Ensure the scene is updated
    bpy.context.view_layer.update()

    obj_bb = [zendo_object.obj.matrix_world @ Vector(corner) for corner in zendo_object.obj.bound_box]
    obj_bb_min = Vector((min(v.x for v in obj_bb), min(v.y for v in obj_bb), min(v.z for v in obj_bb)))
    obj_bb_max = Vector((max(v.x for v in obj_bb), max(v.y for v in obj_bb), max(v.z for v in obj_bb)))

    # List to store objects colliding with the given object
    colliding_objects = []

    # Iterate over all other objects in the scene
    for other_obj in ZendoObject.instances:
        if other_obj is zendo_object or other_obj is omit:
            continue  # Skip checking collision with itself or omit object

        # Get the world-space bounding box of the other object
        other_bb = [other_obj.obj.matrix_world @ Vector(corner) for corner in other_obj.obj.bound_box]
        other_bb_min = Vector((min(v.x for v in other_bb), min(v.y for v in other_bb), min(v.z for v in other_bb)))
        other_bb_max = Vector((max(v.x for v in other_bb), max(v.y for v in other_bb), max(v.z for v in other_bb)))

        # Check for overlap in bounding box (AABB collision detection)
        if (obj_bb_min.x <= other_bb_max.x and obj_bb_max.x >= other_bb_min.x and
                obj_bb_min.y <= other_bb_max.y and obj_bb_max.y >= other_bb_min.y and
                obj_bb_min.z <= other_bb_max.z and obj_bb_max.z >= other_bb_min.z):
            colliding_objects.append(other_obj)

    return colliding_objects

def get_random_position(anchor, radius):
    angle = random.uniform(0, 2 * pi)
    # Random distance from the center (with uniform distribution in area)
    distance = random.uniform(0, radius)
    # Convert polar coordinates to Cartesian coordinates
    x = distance * cos(angle)
    y = distance * sin(angle)

    random_position = Vector(anchor) + Vector((x, y, 0))

    return random_position

def get_grounded(instructions):
    grounded_objects = [i for i in instructions if i.get('action') == 'grounded']
    grounded_objects = sorted(grounded_objects, key=lambda x: x['id'])
    return grounded_objects

def get_relations(instructions):
    related_objects = [i for i in instructions if i.get('action') != 'grounded']
    related_objects = sorted(related_objects, key=lambda x: x['id'])
    return related_objects

def get_free_face(args, obj: ZendoObject):
    random_faces = args.random_face_choice
    faces = obj.get_free_face()
    if random_faces:
        return random.choice(faces)
    else:
        return faces[0]

def generate_relation(instruction):
    relation = instruction['action']
    relation_type = relation.split('(')[0]
    relation_target = int(relation.split('(')[1][0])
    target = zendo_objects.get_object(relation_target)

    return relation_type, target


def generate_creation(args, instruction):
    """
        Generates Python command for creating an object

        :param args: Config arguments
        :param instruction: Instruction dictionary.
        :return: Object creation command as string.
        """
    idx = instruction['id']
    shape = instruction['shape']
    color = instruction['color']
    orientation = instruction['orientation']
    action = instruction['action']
    #name = f"{idx}_{shape}"

    if shape == 'block':
        obj = zendo_objects.Block(args, idx, color, orientation)
    elif shape == 'wedge':
        obj = zendo_objects.Wedge(args, idx, color, orientation)
    elif shape == 'pyramid':
        obj = zendo_objects.Pyramid(args, idx, color, orientation)

    if args.random_object_rotation:
        d = random.uniform(0, 360)
        obj.rotate_z(d)
    return obj

def generate_structure(args, prolog_string: str):
    placement_radius = args.placement_radius
    anchor_position = args.anchor_position
    random_faces = args.random_face_choice


    items = ast.literal_eval(prolog_string)
    instructions = []
    for item in items:
        match = re.match(r"item\((\d+),\s*(\w+),\s*(\w+),\s*(\w+),\s*(.+)\)", item)
        if match:
            item_id = int(match.group(1))
            color = match.group(2)
            shape = match.group(3)
            orientation = match.group(4)
            action = match.group(5)
            instructions.append({
                'id': item_id,
                'color': color,
                'shape': shape,
                'orientation': orientation,
                'action': action
            })

    # get all grounded objects to place first
    grounded_objects = get_grounded(instructions)
    related_objects = get_relations(instructions)

    # place one grounded object in the center as "anchor"
    anchor = grounded_objects[0]
    anchor_obj = generate_creation(args, anchor)
    anchor_obj.move(Vector(anchor_position))
    #anchor_obj.rotate_z(90)


    grounded_objects.remove(anchor)

    # Place grounded objects
    for instruction in grounded_objects:
        current_object = generate_creation(args, instruction)
        while True:
            # Try to place the object at a random position
            pos = get_random_position(anchor=anchor_position, radius=placement_radius)

            current_object.move(pos)
            colliding_objects = check_collision(current_object)
            if len(colliding_objects) == 0:
                break
            else:
                print("Collision!")


    # Place related objects
    for instruction in related_objects:

        current_object = generate_creation(args, instruction)
        relation_type, target = generate_relation(instruction)

        while True:
            if relation_type == 'touching':
                random_faces = args.random_face_choice
                faces = target.get_free_face()
                if random_faces:
                    face = random.choice(faces)
                else:
                    face = faces[0]

                touching(current_object, target, face='right')
                #current_object.move(Vector((0, 2, 0)))

                colliding_objects = check_collision(current_object, target)
                if len(colliding_objects) == 0:
                    target.set_touching(face, current_object)
                    axis, direction = face_map[face]
                    object_1_face = list(face_map.keys())[list(face_map.values()).index((axis, direction * (-1)))]
                    current_object.set_touching(object_1_face, target)
                    break
                else:
                    print("Collision!")

            elif relation_type == 'pointing':
                pos = get_random_position(anchor=anchor_position, radius=placement_radius)
                current_object.move(pos)
                pointing(current_object, target)
                colliding_objects = check_collision(current_object)
                if len(colliding_objects) == 0:
                    break
                else:
                    print("Collision!")

            elif relation_type == 'on_top_of':
                print("test")


    for instance in ZendoObject.instances:
        if instance.shape == 'pyramid':
            pointing_obj = check_pointing(instance)
            print(instance.color)
            for p in pointing_obj:
                print(instance.get_namestring(), "points at", p.get_namestring())

















    #placement_commands = generate_placements(grounded_objects, 0,0)




