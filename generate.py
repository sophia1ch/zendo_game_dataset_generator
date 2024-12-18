import ast
import re
import zendo_objects
import utils
import zendo_objects

example_string = "['item(3, red, pyramid, vertical, inside(1))', 'item(2, red, wedge, upright, grounded)', 'item(1, blue, block, flat, pointing(3))', 'item(0, red, block, flat, on_top_of(1))']"


def generate_object_creation_commands(instructions, object_colors):
    """
    Generates Python commands for creating Blender objects with unique names.

    :param instructions: List of instruction dictionaries.
    :param object_colors: Dictionary mapping color names to actual color values.
    :return: List of Python commands as strings.
    """
    # Mapping of shape names to their corresponding classes
    shape_to_class = {
        'block': 'zendo_objects.Block',
        'wedge': 'zendo_objects.Wedge',
        'pyramid': 'zendo_objects.Pyramid'
    }

    commands = []
    for instruction in instructions:
        shape_class = shape_to_class.get(instruction['shape'], None)
        if not shape_class:
            raise ValueError(f"Unknown shape: {instruction['shape']}")

        # Generate a unique name for the object
        unique_name = f"{instruction['shape']}_{instruction['id']}"

        # Build the command string, including the unique name
        command = (
            f"{unique_name} = {shape_class}("
            f"args, 1.0, object_colors['{instruction['color']}'], "
            f"'{instruction['orientation']}')"
        )
        commands.append(command)

    return commands



def generate_structure(args, prolog_string: str):
    object_shapes, object_colors, object_sizes = utils.read_properties_json(args.properties_json)
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


    command = generate_object_creation_commands(instructions, object_colors)
    for c in command:
        exec(c)

