import sys, json


def extract_args(input_argv=None):
    """
    Extracts command-line arguments provided after "--" to forward them to the script.

    :param input_argv: Optional list of command-line arguments. If None, defaults to sys.argv.
    :return: List of arguments following the "--" separator.
    """

    if input_argv is None:
        input_argv = sys.argv
    output_argv = []
    if '--' in input_argv:
        idx = input_argv.index('--')
        output_argv = input_argv[(idx + 1):]
    return output_argv


def parse_args(parser, argv=None):
    """
    Parses command-line arguments using a provided ArgumentParser, considering only the arguments after --.

    :param parser: An instance of argparse.ArgumentParser used to parse the arguments.
    :param argv: Optional list of command-line arguments. If None, defaults to sys.argv.
    :return: Namespace containing parsed arguments.
    """

    return parser.parse_args(extract_args(argv))


def read_properties_json(file_path):
    """
    Reads properties from a JSON file and returns mappings for object shapes, colors, and sizes.

    :param file_path: Path to the JSON file containing properties.
    :return: Tuple of dictionaries for object shapes, color mappings, and size values.
    """

    file = open(file_path, "r")
    properties = json.load(file)
    color_name_to_rgba = {}
    for name, rgb in properties['colors'].items():
        rgba = [float(c) / 255.0 for c in rgb] + [1.0]
        color_name_to_rgba[name] = rgba
    object_mapping = {}
    for k, v in properties['shapes'].items():
        object_mapping[k] = v
    size_mapping = {}
    for k, v in properties['sizes'].items():
        size_mapping[k] = v

    return object_mapping, color_name_to_rgba, size_mapping
