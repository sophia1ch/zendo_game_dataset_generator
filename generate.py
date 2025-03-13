import ast
import re
from collections import defaultdict, deque
from math import cos, sin, pi

import zendo_objects
from structure import *


def check_pointing(observer: ZendoObject):
    """
    Determines which objects the given Zendo object is pointing toward.

    :param observer: The ZendoObject to evaluate.
    :return: A list of ZendoObjects that the observer is pointing at.
    """

    bpy.context.view_layer.update()
    results = []

    for origin, direction in observer.get_rays():
        ray_path = [origin.copy()]
        direction = direction.normalized()
        current_location = origin.copy()
        # origin.z = 0.001 # Ground offset because raycasting on 0 Z coordinate doesn't work reliably
        hit_location = origin.copy()
        while True:
            hit, hit_location, _, _, obj, _ = bpy.context.scene.ray_cast(
                bpy.context.view_layer.depsgraph, current_location, direction
            )
            if not hit:
                break
            else:
                zendo_obj = zendo_objects.get_from_blender_obj(obj)
                if zendo_obj is not None and zendo_obj not in results and zendo_obj is not observer:
                    ray_path.append(hit_location)
                    results.append(zendo_obj)
                current_location = hit_location + direction * 0.01
                hit_location *= 1.01

    return results


def check_collision(zendo_object: ZendoObject, omit: ZendoObject = None, margin: float = 0.0):
    """
    Checks for collisions between the given Zendo object and other objects in the scene.

    :param zendo_object: The ZendoObject to check for collisions.
    :param omit: An optional object to exclude from collision checks.
    :param margin: A margin around the bounding box to extend the collision check.
    :return: A list of ZendoObjects that are colliding with the given object.
    """

    # Ensure the scene is updated
    bpy.context.view_layer.update()

    # Compute the bounding box of the object, including collision margin
    obj_bb = [zendo_object.obj.matrix_world @ Vector(corner) for corner in zendo_object.obj.bound_box]
    obj_bb_min = Vector((min(v.x for v in obj_bb), min(v.y for v in obj_bb), min(v.z for v in obj_bb))) - Vector(
        (margin,) * 3)
    obj_bb_max = Vector((max(v.x for v in obj_bb), max(v.y for v in obj_bb), max(v.z for v in obj_bb))) + Vector(
        (margin,) * 3)

    # List to store objects colliding with the given object
    colliding_objects = []

    # Iterate over all other objects in the scene
    for other_obj in ZendoObject.instances:
        if other_obj is zendo_object or other_obj is omit:
            continue  # Skip checking collision with itself or omit object

        # Get the world-space bounding box of the other object, including collision margin
        other_bb = [other_obj.obj.matrix_world @ Vector(corner) for corner in other_obj.obj.bound_box]
        other_bb_min = Vector(
            (min(v.x for v in other_bb), min(v.y for v in other_bb), min(v.z for v in other_bb))) - Vector(
            (margin,) * 3)
        other_bb_max = Vector(
            (max(v.x for v in other_bb), max(v.y for v in other_bb), max(v.z for v in other_bb))) + Vector(
            (margin,) * 3)

        # Check for overlap in bounding box (AABB collision detection)
        if (obj_bb_min.x <= other_bb_max.x and obj_bb_max.x >= other_bb_min.x and
                obj_bb_min.y <= other_bb_max.y and obj_bb_max.y >= other_bb_min.y and
                obj_bb_min.z <= other_bb_max.z and obj_bb_max.z >= other_bb_min.z):
            colliding_objects.append(other_obj)

    return colliding_objects


def get_random_position(anchor, radius):
    """
    Generates a random position within a circular area centered around an anchor point.

    :param anchor: The central point from which the random position is generated.
    :param radius: The maximum distance from the anchor within which the position is generated.
    :return: A Vector representing the random position.
    """

    angle = random.uniform(0, 2 * pi)
    # Random distance from the center (with uniform distribution in area)
    distance = random.uniform(0, radius)
    # Convert polar coordinates to Cartesian coordinates
    x = distance * cos(angle)
    y = distance * sin(angle)

    random_position = Vector(anchor) + Vector((x, y, 0))

    return random_position


def get_grounded(instructions):
    """
    Extracts and sorts objects that have the 'grounded' action from the given instructions.

    :param instructions: A list of dictionaries representing object instructions.
    :return: A sorted list of grounded objects based on their 'id' value.
    """

    grounded_objects = [i for i in instructions if i.get('action') == 'grounded']
    grounded_objects = sorted(grounded_objects, key=lambda x: x['id'])
    return grounded_objects


def get_relations(instructions):
    """
    Extracts and sorts objects based on their dependencies in relation to other objects.

    :param instructions: A list of dictionaries representing object instructions.
    :return: A sorted list of objects based on dependency resolution.
    """

    related_objects = [i for i in instructions if i.get('action') != 'grounded']

    dependencies = defaultdict(list)
    id_to_object = {obj['id']: obj for obj in instructions}
    dependents_count = {obj['id']: 0 for obj in instructions}

    for obj in instructions:
        action = obj['action']
        if '(' in action and ')' in action:
            dep_id = int(action.split('(')[-1].split(')')[0])
            dependencies[dep_id].append(obj['id'])
            dependents_count[obj['id']] += 1

    # Topological sorting
    sorted_objects = []
    queue = deque([obj_id for obj_id, count in dependents_count.items() if count == 0])

    while queue:
        current = queue.popleft()
        sorted_objects.append(id_to_object[current])
        for dependent in dependencies[current]:
            dependents_count[dependent] -= 1
            if dependents_count[dependent] == 0:
                queue.append(dependent)

    return sorted_objects


def get_free_face(args, obj: ZendoObject):
    """
    Determines a free face of a given Zendo object.

    :param args: Configuration arguments containing the random face choice setting.
    :param obj: The ZendoObject whose free face is to be determined.
    :return: A randomly selected free face if random selection is enabled, otherwise the first available face.
    """

    random_faces = args.random_face_choice
    faces = obj.get_free_face()
    if random_faces:
        return random.choice(faces)
    else:
        return faces[0]


def generate_relation(instruction):
    """
    Parses an instruction to extract the relation type and the target object.

    :param instruction: A dictionary containing the object instruction with an 'action' field.
    :return: A tuple containing the relation type (string) and the target ZendoObject.
    """

    relation = instruction['action']
    relation_type = relation.split('(')[0]
    relation_target = int(relation.split('(')[1][0])
    target = zendo_objects.get_object(relation_target)

    return relation_type, target


def generate_creation(args, instruction, collection):
    """
    Generates a Zendo object based on the provided instruction and adds it to the collection.

    :param args: Configuration arguments for object creation.
    :param instruction: A dictionary containing object properties such as id, shape, color, orientation, and action.
    :param collection: The Blender collection to which the created object will be added.
    :return: The created ZendoObject instance.
    """

    idx = instruction['id']
    shape = instruction['shape']
    color = instruction['color']
    orientation = instruction['orientation']
    action = instruction['action']
    # name = f"{idx}_{shape}"

    if orientation == 'vertical':
        if shape == 'block':
            orientation = random.choice(['upright', 'upside_down'])
        else:
            orientation = 'upright'

    if shape == 'block':
        obj = zendo_objects.Block(args, idx, color, orientation)
    elif shape == 'wedge':
        obj = zendo_objects.Wedge(args, idx, color, orientation)
    elif shape == 'pyramid':
        obj = zendo_objects.Pyramid(args, idx, color, orientation)

    collection.objects.link(obj.obj)

    if args.random_object_rotation:
        d = random.uniform(0, 360)
        obj.rotate_z(d)
    return obj


def generate_structure(args, items: list[str], collection, attempt: int = 1):
    """
    Generates a structured scene based on a set of item descriptions and configuration parameters.

    :param args: Configuration arguments for scene generation.
    :param items: A list of item descriptions in Prolog format.
    :param collection: The Blender collection where the generated objects will be stored.
    :param attempt: The current attempt count for scene generation (used for retries).
    :raises Exception: If the maximum number of generation attempts is exceeded.
    """

    if attempt > args.generation_attempts:
        raise Exception(f"Exceeded generation attempts, unable to generate a scene for rule:\n {items}")

    placement_radius = args.placement_radius
    anchor_position = args.anchor_position
    collision_margin = args.collision_margin
    touching_margin = args.touching_margin
    placement_attempts = args.placement_attempts
    los_threshold = args.los_threshold

    # items = prolog_string
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

    related_objects = get_relations(instructions)
    if len(related_objects) != len(instructions):
        raise Exception(f"Rule not resolvable!\n {instructions}")

    integrity = True

    # Place related objects
    for instruction in related_objects:
        idx = related_objects.index(instruction)
        current_object = generate_creation(args, instruction, collection)

        # randomly rotate the objects in 90 degree steps
        random_rotation = random.choice([0, 90, 180, 270])
        current_object.rotate_z(random_rotation)

        if instruction['action'] == 'grounded':
            attempts = 0
            while True:
                if attempts >= placement_attempts:
                    print(f"Exceeded maximum placement attempts!")
                    integrity = False
                    break
                # Try to place the object at a random position
                if idx == 0:
                    pos = Vector(anchor_position)
                else:
                    pos = get_random_position(anchor=anchor_position, radius=placement_radius)

                current_object.set_position_xy(pos)
                colliding_objects = check_collision(current_object, margin=collision_margin)
                if len(colliding_objects) == 0:
                    break
                else:
                    attempts += 1
                    print(
                        f"{current_object.get_namestring()} colliding with {[o.get_namestring() for o in colliding_objects]}!")
        else:
            relation_type, target = generate_relation(instruction)
            attempts = 0
            while True:
                if attempts >= placement_attempts:
                    print(f"Exceeded maximum placement attempts!")
                    integrity = False
                    break

                if relation_type == 'touching':
                    random_faces = args.random_face_choice
                    faces = target.get_free_face()
                    if random_faces:
                        face = random.choice(faces)
                    else:
                        face = faces[0]
                    touching(current_object, target, face=face, margin=touching_margin)
                    colliding_objects = check_collision(current_object, target)
                    if len(colliding_objects) == 0:
                        target.set_touching(face, current_object)
                        axis, direction = face_map[face]
                        object_1_face = list(face_map.keys())[list(face_map.values()).index((axis, direction * (-1)))]
                        current_object.set_touching(object_1_face, target)
                        break
                    else:
                        attempts += 1
                        print(
                            f"{current_object.get_namestring()} colliding with {[o.get_namestring() for o in colliding_objects]}!")

                elif relation_type == 'pointing':
                    pos = get_random_position(anchor=anchor_position, radius=placement_radius)
                    current_object.set_position_xy(pos)
                    pointing(current_object, target)
                    colliding_objects = check_collision(current_object, margin=collision_margin)
                    pointing_objects = check_pointing(current_object)
                    if len(colliding_objects) == 0 and len(pointing_objects) == 1:
                        break
                    else:
                        attempts += 1
                        print(
                            f"{current_object.get_namestring()} pointing towards {[o.get_namestring() for o in pointing_objects]}!")

                elif relation_type == 'on_top_of':
                    on_top(current_object, target, margin=touching_margin)
                    break

    # Scene integrity check

    print("Integrity check")
    for instruction in related_objects:
        # Check pointing
        current_object = zendo_objects.get_object(instruction['id'])
        if current_object.pose == 'upright' or current_object.pose == 'upside_down':
            continue
        pointing_objects = check_pointing(current_object)
        colliding_objects = check_collision(current_object)
        print(
            f"{current_object.get_namestring()} pointing towards {[o.get_namestring() for o in pointing_objects]}!\n"
            f"{current_object.get_namestring()} colliding with {[o.get_namestring() for o in colliding_objects]}!")
        if instruction['action'].split('(')[0] == 'pointing':
            if len(pointing_objects) != 1:
                integrity = False
        else:
            if len(pointing_objects) != 0:
                integrity = False

    if check_scene_occlusion(los_threshold):
        integrity = False

    print("Integrity:", integrity)

    if not integrity:
        for obj in collection.objects:
            bpy.data.objects.remove(obj, do_unlink=True)
        ZendoObject.instances.clear()

        generate_structure(args, items, collection, attempt=attempt + 1)


def check_scene_occlusion(threshold):
    """
    Checks whether objects in the scene are occluded beyond a given threshold.

    :param threshold: The occlusion threshold, beyond which the scene is considered occluded.
    :return: True if the occlusion exceeds the threshold, otherwise False.
    """

    cam = bpy.data.objects["Camera.001"]
    camera_loc = cam.location
    targets = [
        o for o in bpy.data.objects
        if o.type == 'MESH' and any(k in o.name for k in ["Pyramid", "Wedge", "Block"])
    ]

    for obj in targets:
        vertices = obj.data.vertices
        if not vertices:
            continue

        mw = obj.matrix_world
        total = len(vertices)
        blocked = 0

        for v in vertices:
            wv = mw @ v.co
            dir_vec = wv - camera_loc
            hit, hit_loc, _, _, hit_obj, _ = bpy.context.scene.ray_cast(
                bpy.context.view_layer.depsgraph, camera_loc, dir_vec
            )

            if (
                    hit
                    and hit_obj != obj
                    and "Ground" not in hit_obj.name
                    and (hit_loc - camera_loc).length < dir_vec.length - 1e-5
            ):
                blocked += 1

        print(f"Actual threshold {obj.name}: {blocked / total:.2f}")
        if blocked / total >= threshold:
            return True
    return False
