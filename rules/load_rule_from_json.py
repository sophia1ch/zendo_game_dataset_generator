import random, json
from dataclasses import dataclass

@dataclass
class PlaceholderTemplate:
    template: str
    placeholders: list[str]

@dataclass
class Rules:
    placeholders: dict[str, dict[str, list[PlaceholderTemplate]]]

@dataclass
class TemplateGenerator:
    rules: Rules
    depth: int
    used_operators: list[str]
    two_random_steps: bool
    print_tree: bool

def load_json_rules(filename) -> Rules|None:
    with open(filename) as f:
        data = json.load(f)
        placeholders = {}
        
        def make_placeholder_template_list(templates):

            return [PlaceholderTemplate(str(t["template"]), t["placeholders"]) for t in templates]
        
        for attribute_identifier, attribute_values in data["ATTRIBUTES"].items():
            placeholders[attribute_identifier] = { "": [PlaceholderTemplate(str(value), []) for value in attribute_values] }
        del data["ATTRIBUTES"]

        for placeholder_identifier, placeholder_values in data.items():
            placeholder_type = type(placeholder_values)
            if placeholder_type == dict:
                placeholders[placeholder_identifier] = { value: make_placeholder_template_list(templates) for value, templates in placeholder_values.items() }
            else:
                assert(placeholder_type == list)
                placeholders[placeholder_identifier] = { "": make_placeholder_template_list(placeholder_values) }

        
        return Rules(placeholders=placeholders)
    return None

def template_from_text(rules: Rules, text: str):
    used_placehoolders = []
    template = (text + ".")[:-1]
    for placeholder_identifier in rules.placeholders.keys():
        if placeholder_identifier in text:
            template = template.replace(placeholder_identifier, f"{{{placeholder_identifier}}}")
            used_placehoolders.append(placeholder_identifier)
    return PlaceholderTemplate(template, used_placehoolders)

def print_rules(rules: Rules):
    for placeholder_identifier, placeholder_values in rules.placeholders.items():
        print(placeholder_identifier)
        for placeholder_value, placeholder_templates in placeholder_values.items():
            tabs_str = "\t"
            if placeholder_value != "":
                print(f"{tabs_str}{placeholder_value}")
                tabs_str += "\t"
            
            for template in placeholder_templates:
                print(f"{tabs_str}{template.template}")

def random_placeholder_template_or_error(rules: Rules, placeholder_identifier: str, two_steps: bool=False, used_operators: list[str]|None=None) -> PlaceholderTemplate|str:
    # NOTE(kilian): if two_steps is true, first a random placeholder category (like "at least" for QUANTITY) is drawn randomly,
    # then a template of that category. Otherwise, a random template is drawn from the templates of all placeholder categories

    if placeholder_identifier not in rules.placeholders:
        return f"[ERROR: Unknown placeholder {placeholder_identifier}]"

    # TODO(kilian): Make 
    if two_steps:
        placeholder_values = rules.placeholders[placeholder_identifier]
        picked_placeholder_value = None
        if len(placeholder_values) == 1:
            if "" not in placeholder_values:
                return f"[ERROR: Single value of placeholder {placeholder_identifier} is not \"\"]"
            picked_placeholder_value = ""
        else:
            placeholder_value_keys = list(placeholder_values.keys())
            picked_placeholder_value = random.choice(placeholder_value_keys)
            
        placeholder_templates = placeholder_values[picked_placeholder_value]
        if len(placeholder_templates) == 0:
                return f"[ERROR: No templates for placeholder {placeholder_identifier}] at value {picked_placeholder_value}"
    else:
        all_placeholder_templates = []
        for _, placeholder_templates in rules.placeholders[placeholder_identifier].items():
            all_placeholder_templates += placeholder_templates
        placeholder_templates = all_placeholder_templates

        if len(placeholder_templates) == 0:
                return f"[ERROR: No templates for placeholder {placeholder_identifier}]"
    
    if placeholder_identifier == "OPERATION":
        picked_template = random.choice([template for template in placeholder_templates if template.template not in used_operators])
        if len(picked_template.placeholders) > 0: # Check if operation is none (a word independent way of checking)
            used_operators.append(picked_template.template)
    else:
        picked_template = random.choice(placeholder_templates)
    return picked_template

def template_to_string_random_recursive(generator: TemplateGenerator, template: PlaceholderTemplate) -> str:
    if generator.print_tree:
        tabs_string = '\t'*generator.depth
        print(f"{tabs_string}{template.template}")

    result = (template.template + ".")[:-1]
    for placeholder in template.placeholders:
        placeholder_key = f"{{{placeholder}}}"
        i = 0
        while placeholder_key in result:
            if generator.print_tree:
                print(f"{tabs_string}-> [{i}] {placeholder_key}")
            random_replacement = random_placeholder_template_or_error(generator.rules, placeholder, two_steps=generator.two_random_steps, used_operators=generator.used_operators)
            if isinstance(random_replacement, PlaceholderTemplate):
                generator.depth += 1
                random_replacement = template_to_string_random_recursive(generator, random_replacement)
                generator.depth -= 1
            result = result.replace(placeholder_key, random_replacement, 1)
            i += 1
    return result

def random_rule(rules: Rules, starting_template: PlaceholderTemplate, two_random_steps: bool=False, print_tree: bool=False) -> str:
    generator = TemplateGenerator(rules, depth=0, used_operators=[], two_random_steps=two_random_steps, print_tree=print_tree)
    string = template_to_string_random_recursive(generator, starting_template)
    return string

rules = load_json_rules('zendo_rules.json')
print_rules(rules)

# TODO(kilian): Fix none operation being displayed as "none"
starting_template = template_from_text(rules, "A structure must contain QUANTITY.")
print(random_rule(rules, starting_template, two_random_steps=False, print_tree=True))

'''
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
'''
