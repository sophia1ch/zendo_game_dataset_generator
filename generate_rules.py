import sys, argparse
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from argparse import Namespace
import yaml
from rules.rules import generate_rule
from zendo_objects import *
import utils

def main(args):
    """
    Main function to generate and render structured scenes based on specified rules.

    This function initializes the Blender scene, loads rules, generates structures
    according to Prolog queries, renders the scenes, and stores the resulting data.

    :param args: Configuration arguments for rule generation, scene creation,
                 rendering, and file paths.
    """

    #######################################################
    # Main
    #######################################################

    rules_json_file = args.rules_json_file
    num_rules = args.num_rules
    rules = []
    queries = []
    queries_n = []
    while len(rules) < num_rules:
        # get rule in string form and query, negative query in prolog form
        rule, query, n_query = generate_rule(rules_json_file)
        if "Error" not in rule and "on top" in rule:
            rules.append(rule)
            queries.append(query)
            queries_n.append(n_query)
    rules_path = os.path.join("configs", f"pointing_rules.txt")
    with open(rules_path, "w", encoding="utf-8") as f:
        for rule, query, n_query in zip(rules, queries, queries_n):
            f.write(f"rule: '{rule}'\n")
            f.write(f"query: '{query}'\n")
            f.write(f"query_n: '{n_query}'\n\n")

    print(f"✅ Saved {num_rules} rules to {rules_path}")


if __name__ == '__main__':
    """
    Entry point for executing the rendering pipeline.

    Parses command-line arguments, loads configuration settings from a YAML file, 
    and initiates the main function to generate and render structured scenes.
    """

    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config-file", type=str, default="configs/simple_config.yml",
                        help='config file for rendering')
    parser.add_argument("--start-rule", type=int, default=None,
                    help="Start index of rules to render (inclusive)")
    parser.add_argument("--end-rule", type=int, default=None,
                    help="End index of rules to render (inclusive)")
    conf = parser.parse_args(sys.argv[sys.argv.index("--") + 1:])

    with open(conf.config_file) as f:
        args = yaml.safe_load(f.read())  # load the config file
    print(conf.start_rule, conf.end_rule)
    args = Namespace(**args)
    if conf.start_rule is not None:
        args.start_rule = conf.start_rule
    if conf.end_rule is not None:
        args.end_rule = conf.end_rule

    utils.DEBUG_PRINTING = args.debug_printing

    main(args)
