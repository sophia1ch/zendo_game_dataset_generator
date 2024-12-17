from pyswip import Prolog
import os


def generate_scene():
    prolog = Prolog()
    rule_path = os.path.join(os.path.dirname(__file__), 'rules.pl')
    prolog.consult(rule_path)

    #query = "generate_valid_structure([exactly(red,3,Structure)], Structure)"
    #query = "generate_valid_structure([and([exactly(red,1,Structure), exactly(blue,1,Structure), exactly(yellow,1,Structure), exactly(flat,3,Structure)])], Structure)"
    query = "generate_valid_structure([at_least_interaction(block, blue, on_top_of, 3, Structure)], Structure)"
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
