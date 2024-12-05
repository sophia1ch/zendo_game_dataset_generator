from pyswip import Prolog
import os


def generate_scene():
    prolog = Prolog()
    rule_path = os.path.join(os.path.dirname(__file__), 'rules_third.pl')
    prolog.consult(rule_path)

    # Define a structure of items
    structure = [
        "item(red, pyramid, flat)",
        "item(red, wedge, vertical)",
        "item(red, block, upright)"
    ]

    # Convert the structure into Prolog format
    structure_str = "[" + ", ".join(structure) + "]"

    # Example query to check if there's at least one red item in the structure
    query = f"at_least(red, 1, {structure_str}, Count)"
    print(query)

    # Execute the query
    result = list(prolog.query(query))
    print(result)


# Example usage
generate_scene()
