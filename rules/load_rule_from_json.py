import json

# Load the JSON file
with open('zendo_rules.json', 'r') as file:
    data = json.load(file)

# Access a specific rule
rule = data["QUANTITY"]["at least"][0]  # First rule in "at least"
template = rule["template"]
placeholders = rule["placeholders"]

# Example values to substitute
values = {"NUMBER": 3, "COLOR": "red", "OPERATION": "and"}

# Replace placeholders in the template
filled_rule = template.format(**values)

print(filled_rule)  # Output: "at least 3 red pieces and"
