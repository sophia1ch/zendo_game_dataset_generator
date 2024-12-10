from pyswip import Prolog
import os


def generate_scene():
    prolog = Prolog()
    rule_path = os.path.join(os.path.dirname(__file__), 'rules.pl')
    prolog.consult(rule_path)

    query = f"odd_number_of(red, Structure)"

    # Execute the query
    results = []
    for _ in range(100):
        prolog_query = prolog.query(query)
        for i, szene in enumerate(prolog_query):
            res = szene["Structure"]
            print(i, res)
            results.append(res)

# Example usage
generate_scene()
