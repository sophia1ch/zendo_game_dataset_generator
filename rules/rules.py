from pyswip import Prolog
import os


def generate_scene():
    prolog = Prolog()
    rule_path = os.path.join(os.path.dirname(__file__), 'rules.pl')
    prolog.consult(rule_path)

    #query = "and([either_or(7, 5, Structure), at_least(red, 3, Structure)], Structure)"
    #query = "generate_valid_structure([check_exactly(red,3,Structure)], Structure)"
    query = "generate_valid_structure([or_checks([check_exactly(red,3,Structure), check_more_than(red,blue,Structure)])], Structure)"
    # Execute the query
    results = []
    for _ in range(100):
        prolog_query = prolog.query(query)
        for i, szene in enumerate(prolog_query):
            res = szene["Structure"]
            print(i, len(res), res)
            results.append(res)

# Example usage
generate_scene()
