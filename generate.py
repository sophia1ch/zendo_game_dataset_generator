import ast
import re

from numpy.core.numeric import infty

import zendo_objects
import utils
from math import cos, sin, pi
from structure import *
import bpy
import random
from mathutils import Vector


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
            continue  # Skip checking collision with itself

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
    object_shapes, object_colors, object_sizes = utils.read_properties_json(args.properties_json)

    idx = instruction['id']
    shape = instruction['shape']
    color = object_colors[instruction['color']]
    orientation = instruction['orientation']
    action = instruction['action']
    #name = f"{idx}_{shape}"

    if shape == 'block':
        return zendo_objects.Block(args, idx, color, orientation)
    elif shape == 'wedge':
        return zendo_objects.Wedge(args, idx, color, orientation)
    elif shape == 'pyramid':
        return zendo_objects.Pyramid(args, idx, color, orientation)


def generate_commands(instructions):
    """
    Generates Python commands for creating objects and defining relationships.

    :param instructions: List of instruction dictionaries.
    :return: Tuple of object creation commands and relation commands as strings.
    """
    # Mapping of shape names to their corresponding classes
    shape_to_class = {
        'block': 'zendo_objects.Block',
        'wedge': 'zendo_objects.Wedge',
        'pyramid': 'zendo_objects.Pyramid'
    }

    object_creation_commands = []
    relation_commands = []

    for instruction in instructions:
        shape_class = shape_to_class.get(instruction['shape'], None)
        if not shape_class:
            raise ValueError(f"Unknown shape: {instruction['shape']}")

        # Generate a unique name for the object
        unique_name = f"{instruction['shape']}_{instruction['id']}"

        # Build the object creation command
        object_command = (
            f"{unique_name} = {shape_class}("
            f"args, 1.0, object_colors['{instruction['color']}'], "
            f"'{instruction['orientation']}')"
        )
        object_creation_commands.append(object_command)

        # Check for relationships in the 'action' field
        action = instruction['action']
        match = re.match(r"(\w+)\((\d+)\)", action)
        if match:
            rel_type = match.group(1)  # e.g., 'inside', 'pointing', 'on_top_of'
            target_id = int(match.group(2))  # Target object ID

            # Generate the target object's unique name
            target_object = next(
                (f"{target_instr['shape']}_{target_instr['id']}" for target_instr in instructions if target_instr['id'] == target_id),
                None
            )
            if not target_object:
                raise ValueError(f"Target object with ID {target_id} not found for relation '{rel_type}'.")


            # Generate the relationship command
            relation_command = f"{rel_type}({unique_name}, {target_object})"
            relation_commands.append(relation_command)

    return object_creation_commands, relation_commands


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
                touching(current_object, target, face=face)
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















    #placement_commands = generate_placements(grounded_objects, 0,0)




