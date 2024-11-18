import sys, json


def extract_args(input_argv=None):
    """
    Pull out command-line arguments after "--". Blender ignores command-line flags
    after --, so this lets us forward command line arguments from the blender
    invocation to our own script.
    """
    if input_argv is None:
        input_argv = sys.argv
    output_argv = []
    if '--' in input_argv:
        idx = input_argv.index('--')
        output_argv = input_argv[(idx + 1):]
    return output_argv


def parse_args(parser, argv=None):
    return parser.parse_args(extract_args(argv))


def read_properties_json(file_path):
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