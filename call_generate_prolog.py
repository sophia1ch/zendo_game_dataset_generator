import sys, json
from rules.rules import generate_prolog_structure

if __name__ == "__main__":
    n = int(sys.argv[1])
    query = sys.argv[2]
    path = sys.argv[3]
    result = generate_prolog_structure(n, query, path)
    print(json.dumps(result))
