import sys, argparse
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from argparse import Namespace
import yaml
from rules.rules import generate_valid_rule
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
    rules_prolog_file = getattr(args, "rules_prolog_file", "rules/rules.pl")
    num_rules = args.num_rules
    rules = []
    queries = []
    queries_n = []

    # Each slot draws random rules and verifies (via generate_valid_rule) that they can
    # actually be realized by some structure before accepting one, so a rule that reads
    # fine as text but is logically impossible (e.g. "more upright pieces than vertical
    # pieces", contradictory "and"-ed attribute counts, ...) gets discarded and re-drawn
    # instead of silently ending up in the output. This bounds the outer loop so a broken
    # rules file can't hang the script forever if a slot keeps failing.
    max_slots = num_rules * 3
    slots_tried = 0
    while len(rules) < num_rules and slots_tried < max_slots:
        slots_tried += 1
        result = generate_valid_rule(rules_json_file, rules_prolog_file)
        if result is None:
            print("⚠️ Gave up on a rule slot after exhausting its retries, trying another...")
            continue
        rule, query, n_query, attempts_used = result
        rules.append(rule)
        queries.append(query)
        queries_n.append(n_query)

    rules_path = os.path.join("configs", f"rules.txt")
    with open(rules_path, "w", encoding="utf-8") as f:
        for rule, query, n_query in zip(rules, queries, queries_n):
            f.write(f"rule: '{rule}'\n")
            f.write(f"query: '{query}'\n")
            f.write(f"query_n: '{n_query}'\n\n")

    if len(rules) < num_rules:
        print(f"⚠️ Warning: Only generated {len(rules)}/{num_rules} realizable rules "
              f"after {slots_tried} attempts.")
    print(f"✅ Saved {len(rules)} rules to {rules_path}")


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
