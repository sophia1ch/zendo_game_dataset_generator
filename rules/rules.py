from pyswip import Prolog


def parse_rule(rule_str):
    # TODO: implement
    pass


def generate_scene():
    prolog = Prolog()
    rule_path = "rules.pl"
    prolog.consult(rule_path)

    # Parse the rule string to Prolog
    # prolog_rule = parse_rule(rule_str)
    prolog_rule = f"rule_opposite_directions(Structure)"

    # Query Prolog with the parsed rule
    correct_scenes = []
    query_str = f"rule_opposite_directions([item(pyramid, blue, flat), item(block, yellow, upside_down)])"

    # Execute the query
    for scene in prolog.query(query_str):
        correct_scenes.append(scene)

    return correct_scenes


# Example usage
scenes = generate_scene()
print("Generierte Szenen:", scenes)
