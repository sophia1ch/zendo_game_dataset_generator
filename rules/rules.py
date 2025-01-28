import sys, os, random, json, re
from dataclasses import dataclass, field
from pyswip import Prolog


@dataclass
class PlaceholderTemplate:
    """
    A placeholder template describes a text to be inserted in the place of its placeholder_id.
    It can itself contain placeholders, which is why it is split up into tokens.
    """

    @dataclass(frozen=True)
    class Token:
        string: str
        is_placeholder: bool
    
    placeholder_id: str
    template: str
    placeholders: set[str]
    tokens: list[Token]
    prolog: list[str]
    orientations: list[str]


@dataclass
class Placeholder:
    """
    A placeholder is to be replaced by any of its placeholder templates stored in all_templates.
    """

    categories: dict[str, list[PlaceholderTemplate]]
    templates_of_start_match: dict[str, list[PlaceholderTemplate]]
    all_templates: list[PlaceholderTemplate]
    start_pattern: re.Pattern[str]


@dataclass
class Rules:
    # Maps QUANTITY, OPERATION, INTERACTION, ORIENTATION, COLOR, etc. to their respective placeholder rules.
    placeholders: dict[str, Placeholder]

    # Maps the first token string of a template to itself
    templates_of_start_match: dict[str, list[PlaceholderTemplate]]
    templates_start_pattern: re.Pattern[str]


@dataclass
class RuleNode:
    template: PlaceholderTemplate
    children: list["RuleNode"]


@dataclass
class RuleParser:
    rules: Rules
    depth: int
    print_tree: bool


@dataclass
class TemplateGenerator:
    rules: Rules
    depth: int
    used_templates: dict[str, list[PlaceholderTemplate]]
    two_random_steps: bool = False
    print_tree: bool = False


# MARK: Exceptions

def raise_unrecognized_placeholder(placeholder_id: str, recognized_placeholders):
    recognized_placeholders_string = ", ".join(list(recognized_placeholders))
    raise Exception(f"Placeholder '{placeholder_id}' is not recognized as it is not part of the rules placeholders ({recognized_placeholders_string}).")


# MARK: General

def make_placeholder_template(placeholder_id: str, template_text: str, prolog_list: list[str], orientations: list[str]) -> PlaceholderTemplate:
    """
    Creates a placeholder template by tokenizing the given template_text. Placeholders in the text must be surrounded by curly braces: {IDENTIFIER}.
    """

    tokens: list[PlaceholderTemplate.Token] = []
    placeholders: set[str] = set()
    
    last_end_index = 0
    for placeholder_match in re.finditer(r"\{([^\{\}]*)\}", template_text):
        match_start_index = placeholder_match.start()
        if last_end_index < match_start_index:
            tokens.append(PlaceholderTemplate.Token(template_text[last_end_index:match_start_index], False))

        placeholder = placeholder_match.group(1)
        placeholders.add(placeholder)
        tokens.append(PlaceholderTemplate.Token(placeholder, True))
        last_end_index = placeholder_match.end()

    # Add last template_text part or empty string if there are no tokens (like for the none operation, which doesn't have a text)
    if last_end_index < len(template_text) or len(tokens) == 0:
        tokens.append(PlaceholderTemplate.Token(template_text[last_end_index:], False))
    
    return PlaceholderTemplate(placeholder_id, template_text, placeholders, tokens, prolog_list, orientations)


def load_json_rules(filename) -> Rules|None:
    with open(filename) as f:
        data = json.load(f)
        placeholders: dict[str, Placeholder] = {}
        
        def make_placeholder_template_list(identifier: str, templates: list[dict]|list[str]):
            result = []
            placeholder_pattern = re.compile(r"{([A-Z]+(?:\|[A-Z]+)*)}")
            placeholder_empty_pattern = re.compile(r"{}")
            for template in templates:
                # NOTE(kilian): Allow for short list where "template" is omitted and only the template values are stored
                template_prolog = template["prolog"] if type(template) == dict and "prolog" in template else []
                template_orientations = template["orientations"] if type(template) == dict and "orientations" in template else []
                template_string = template["template"] if type(template) == dict else str(template)

                # NOTE(kilian): Compute list of placeholders contained in the template
                template_placeholders = []
                while True:
                    placeholder_match = re.search(placeholder_pattern, template_string)
                    if not placeholder_match:
                        break
                    template_placeholder_multiple = placeholder_match.group(1)
                    template_placeholders.append(template_placeholder_multiple.split("|"))
                    template_string = template_string[:placeholder_match.start(1)] + template_string[placeholder_match.end(1):]
                
                def configure_placeholders(result, template_string, i, template_placeholders, selected_placeholders):
                    if i >= len(template_placeholders):
                        result.append(make_placeholder_template(identifier, template_string, template_prolog, template_orientations))
                    else:
                        for p in template_placeholders[i]:
                            selected_placeholders[i] = p
                            replaced_template_string = re.sub(placeholder_empty_pattern, f"{{{p}}}", template_string, count=1)
                            configure_placeholders(result, replaced_template_string, i + 1, template_placeholders, selected_placeholders)
                
                selected_placeholders = [0]*len(template_placeholders)
                configure_placeholders(result, template_string, 0, template_placeholders, selected_placeholders)
            return result
        
        def update_templates_by_match(templates_by_match, all_templates: list[PlaceholderTemplate]):
            for template in all_templates:
                token = template.tokens[0]
                # NOTE(kilian): If the first token is a placeholder, matching the start won't work like for the other templates, so make it match always by using ""
                token_string = "" if token.is_placeholder else token.string
                if token_string != "" and token_string not in templates_by_match:
                    for key in templates_by_match.keys():
                        if key != "" and token_string.startswith(key):
                            token_string = key
                            break
                        if key.startswith(token_string):
                            templates_by_match[token_string] = templates_by_match[key]
                            del templates_by_match[key]
                            break
                    else:
                        templates_by_match[token_string] = []
                templates_by_match[token_string].append(template)
        
        def make_templates_by_match_pattern(templates_by_match):
            templates_start_patern_string = "|".join([re.escape(key) for key in templates_by_match.keys()])
            return re.compile(f"^(?:{templates_start_patern_string})")

        def make_placeholder(categories: dict[str, list[PlaceholderTemplate]]):
            all_templates = [t for templates in categories.values() for t in templates]
            placeholder_templates_of_start_match = { "": [] }
            update_templates_by_match(placeholder_templates_of_start_match, all_templates)
            placeholder_start_pattern = make_templates_by_match_pattern(placeholder_templates_of_start_match)
            return Placeholder(categories, placeholder_templates_of_start_match, all_templates, placeholder_start_pattern)

        for placeholder_identifier, placeholder_values in data.items():
            placeholder_type = type(placeholder_values)
            # NOTE(kilian): Differentiate between categorized templates (like for QUANTITY with "at least") and plain lists of templates
            if placeholder_type == dict:
                placeholders[placeholder_identifier] = make_placeholder({ value: make_placeholder_template_list(placeholder_identifier, templates) for value, templates in placeholder_values.items() })
            else:
                assert(placeholder_type == list)
                placeholders[placeholder_identifier] = make_placeholder({ "": make_placeholder_template_list(placeholder_identifier, placeholder_values) })
        
        # NOTE(kilian): Group all templates and per placeholder by starting pattern to be able to match more easily later
        templates_of_match = { "": [] }
        for placeholder in placeholders.values():
            update_templates_by_match(templates_of_match, placeholder.all_templates)
        rules = Rules(placeholders=placeholders, templates_of_start_match=templates_of_match, templates_start_pattern=make_templates_by_match_pattern(templates_of_match))

        return rules
    return None


def print_rules(rules: Rules):
    for placeholder_identifier, placeholder in rules.placeholders.items():
        print(placeholder_identifier)
        for placeholder_value, placeholder_templates in placeholder.categories.items():
            tabs_str = "\t"
            if placeholder_value != "":
                print(f"{tabs_str}{placeholder_value}")
                tabs_str += "\t"
            
            for template in placeholder_templates:
                print(f"{tabs_str}{template.template}")


def template_from_text(rules: Rules, text: str, placeholders_naked: bool=True) -> PlaceholderTemplate:
    """
    Creates a placeholder template from plain text. Only placeholder identifiers of the given rules are recognized.

    If placeholders_naked is True, the placeholder identifiers are not expected to be surrounded by curly braces.
    """

    template = text[:]
    recognized_placeholders: set[str] = set(rules.placeholders.keys())
    if placeholders_naked:
        for placeholder_identifier in recognized_placeholders:
            if placeholder_identifier in text:
                template = template.replace(placeholder_identifier, f"{{{placeholder_identifier}}}")

    result = make_placeholder_template("CUSTOM", template, [], [])
    for placeholder in result.placeholders:
        if placeholder not in recognized_placeholders:
            raise_unrecognized_placeholder(placeholder, recognized_placeholders)
    return result


# MARK: Generating

def random_placeholder_template(rules: Rules, placeholder_id: str, two_steps: bool=False, used_templates: dict[str, list[PlaceholderTemplate]]|None = None, allowed_orientations: list[PlaceholderTemplate]|None = None, no_interaction_when_operator_used: bool = True) -> PlaceholderTemplate:
    """
    Randomly draws a placeholder template of the rules placeholder templates with given placeholder_id.
    Placeholder templates that are stored in used_templates are not considered.

    By setting allowed_orientations it is also possible to restrict the set of orientation templates further to just those specified in the list. used_templates is still applied.

    If two_steps is true, first a random placeholder category (like "at least" for QUANTITY in zendo_rules.json) is drawn and then a template of that category.
    Otherwise, a random template is drawn from the templates of all placeholder categories.

    If no_interaction_when_operator_used is True, then no templates with INTERACTION placeholder will be considered if any operator was used previously.
    """ 

    if placeholder_id not in rules.placeholders:
        raise_unrecognized_placeholder(placeholder_id, rules.placeholders.keys())

    # NOTE(kilian): First, pick placeholder_templates
    if placeholder_id == "ORIENTATION" and allowed_orientations:
        placeholder_templates = allowed_orientations

        if len(placeholder_templates) == 0:
            raise Exception("No templates specified as allowed orientations.")
    elif two_steps:
        placeholder = rules.placeholders[placeholder_id]
        if len(placeholder.categories) == 1:
            if "" not in placeholder.categories:
                raise Exception(f"Single category of placeholder '{placeholder_id}' is not \"\".")
            picked_placeholder_category = ""
        else:
            picked_placeholder_category = random.choice(list(placeholder.categories.keys()))
            
        placeholder_templates = placeholder.categories[picked_placeholder_category]
        if len(placeholder_templates) == 0:
            raise Exception(f"No templates for placeholder '{placeholder_id}' in category '{picked_placeholder_category}'.")
    else:
        placeholder_templates = rules.placeholders[placeholder_id].all_templates

        if len(placeholder_templates) == 0:
            raise Exception(f"No templates for placeholder '{placeholder_id}'.")

    # NOTE(kilian): Next, pick one
    picked_template = None
    if placeholder_id == "OPERATION":
        # NOTE(kilian): If any operator has already been used, choose the none operator (pick by choosing the one without placeholders)
        used_operators = used_templates.get(placeholder_id, [])
        if len(used_operators) > 0:
            for template in placeholder_templates:
                if len(template.placeholders) == 0:
                    picked_template = template
    if picked_template is None:
        # NOTE(kilian): Pick any placeholder template that wasn't used already
        # TODO(kilian): Find better to check if two templates are equal? Currently checking if their template texts are equal
        used_placeholder_template_texts = [template.template for template in used_templates.get(placeholder_id, [])]
        remaining_placeholder_templates = [template for template in placeholder_templates if template.template not in used_placeholder_template_texts]
        # TODO(kilian): Possibly an issue in the future because none is also considered as an operator here?
        if no_interaction_when_operator_used and len(used_templates.get("OPERATION", [])) > 0:
            # NOTE(kilian): Remove any templates that contain interaction placeholders
            remaining_placeholder_templates = [template for template in remaining_placeholder_templates if "INTERACTION" not in template.placeholders]
        if len(remaining_placeholder_templates) == 0:
            raise Exception(f"Every template of placeholder '{placeholder_id}' was already used.")
        picked_template = random.choice(remaining_placeholder_templates)

    if placeholder_id not in used_templates:
        used_templates[placeholder_id] = []
    used_templates[placeholder_id].append(picked_template)

    return picked_template


def template_to_string_random_recursive(generator: TemplateGenerator, template: PlaceholderTemplate) -> str:
    if generator.print_tree:
        tabs_string = '\t'*generator.depth
        print(f"{tabs_string}{template.template}")
    
    shape_replacements = []
    # NOTE(kilian): Pre generate all shape placeholders such that orientation restrictions can be applied
    for token in template.tokens:
        if token.is_placeholder and token.string == "SHAPE":
            try:
                shape_replacement = random_placeholder_template(generator.rules, token.string, generator.two_random_steps, generator.used_templates)
            except Exception as e:
                shape_replacement = f"[Error: {str(e)}]"
            shape_replacements.append(shape_replacement)

    def compute_allowed_orientations(shape_index: int) -> list[PlaceholderTemplate]|None:
        allowed_orientations = []
        if len(shape_replacements) == 0:
            return None

        if shape_index >= len(shape_replacements):
            # NOTE(kilian): This shouldn't happen
            assert False

        shape_replacement = shape_replacements[shape_index]
        
        # TODO(kilian): This is quite bad, should turn list of orientation strings into list of templates when loading the rules
        if isinstance(shape_replacement, PlaceholderTemplate):
            for orientation_string in shape_replacement.orientations:
                if orientation_string in generator.rules.placeholders["ORIENTATION"].templates_of_start_match:
                    orientation_templates = generator.rules.placeholders["ORIENTATION"].templates_of_start_match[orientation_string]
                    assert(len(orientation_templates) == 1)
                    allowed_orientations.append(orientation_templates[0])
                else:
                    print(f"Warning: ORIENTATION '{orientation_string}' drawn SHAPE {shape_replacement.template} doesn't exist.")
        return allowed_orientations
    
    next_shape_index = 0

    result = ""
    token_placeholder_count = 0
    for i, token in enumerate(template.tokens):
        if token.is_placeholder:
            if generator.print_tree:
                print(f"{tabs_string}-> [{token_placeholder_count}] {token.string}")
            if token.string == "SHAPE":
                replacement = shape_replacements[next_shape_index]
                if isinstance(replacement, PlaceholderTemplate):
                    result += replacement.template
                else:
                    result += replacement
                next_shape_index += 1
            else:
                allowed_orientations = None
                if i + 1 < len(template.tokens):
                    next_token = template.tokens[i + 1]
                    if next_token.is_placeholder and next_token.string == "SHAPE":
                        allowed_orientations = compute_allowed_orientations(next_shape_index)
                try:
                    replacement = random_placeholder_template(generator.rules, token.string, generator.two_random_steps, generator.used_templates, allowed_orientations)
                    generator.depth += 1
                    replacement = template_to_string_random_recursive(generator, replacement)
                    generator.depth -= 1
                    result += replacement
                except Exception as e:
                    result += f"[Error: {str(e)}]"

            token_placeholder_count += 1
        else:
            result += token.string
    return result


def random_rule(rules: Rules, starting_template: PlaceholderTemplate, two_random_steps: bool=False, print_tree: bool=False) -> str:
    generator = TemplateGenerator(rules, depth=0, used_templates={}, two_random_steps=two_random_steps, print_tree=print_tree)
    string = template_to_string_random_recursive(generator, starting_template)
    return string


# MARK: Parsing

# NOTE(kilian): Returns a rule node or a reason why the template does not match.
def parse_rule_text_match(parser: RuleParser, rule: str, template: PlaceholderTemplate) -> tuple[RuleNode, str]|str:
    remaining_rule = rule
    nodes = []
    for i, token in enumerate(template.tokens):
        if parser.print_tree:
            tabs_string = '\t'*parser.depth
            print(f"{tabs_string}[{i}] '{token.string}': '{remaining_rule}'")
        if token.is_placeholder:
            token_placeholder = parser.rules.placeholders[token.string]
            
            # NOTE(kilian): Use longest match (otherwise it will just match none "" anywhere possible)
            possible_templates: list[PlaceholderTemplate] = []
            for possible_match in re.findall(token_placeholder.start_pattern, remaining_rule):
                possible_templates += token_placeholder.templates_of_start_match[possible_match]

            longest_match_node = None
            longest_match_length = 0
            longest_match_remaining_rule = ""
            for test_template in possible_templates:
                parser.depth += 1
                parse_match = parse_rule_text_match(parser, remaining_rule, test_template)
                parser.depth -= 1
                if isinstance(parse_match, str):
                    pass
                else:
                    parse_match, new_remaining_rule = parse_match
                    match_length = len(remaining_rule) - len(new_remaining_rule)
                    if match_length >= longest_match_length:
                        longest_match_node = parse_match
                        longest_match_length = match_length
                        longest_match_remaining_rule = new_remaining_rule
                    # TODO(kilian): Prevent this from happening somewhere else?
                    #if test_template.template != "":
                    #    nodes.append(parse_match)
            
            if not longest_match_node:
                return "ERROR"
            nodes.append(longest_match_node)
            remaining_rule = longest_match_remaining_rule
        else:
            m = re.match(re.escape(token.string), remaining_rule)
            if not m:
                return f"Expected '{token.string}', found '{remaining_rule[:len(token.string)]}'."
            remaining_rule = remaining_rule[m.end(0):]
    return (RuleNode(template, nodes), remaining_rule)


def parse_rule_text(rules: Rules, rule: str, starting_template: PlaceholderTemplate, debug_print=False) -> RuleNode|None:
    parser = RuleParser(rules, depth=0, print_tree=debug_print)
    parse_match = parse_rule_text_match(parser, rule, starting_template)
    if isinstance(parse_match, str):
        return None
    root, remaining_rule = parse_match
    return root


def print_rule_nodes(root: RuleNode):
    nodes = [(0, root)]
    depth = 0
    while nodes:
        new_nodes = []
        i = 0
        for (num, node) in nodes:
            print(depth, num, node.template.template)
            for c in node.children:
                new_nodes.append((i, c))
            i += 1
        nodes = new_nodes
        depth += 1


# MARK: Prolog conversion

def rule_to_prolog(root: RuleNode) -> tuple[str, str]:
    @dataclass
    class PrologCall:
        function_name: str|None = None
        quantity: list[str] = field(default_factory=lambda: [])
        number: list[str] = field(default_factory=lambda: [])
        interaction: list[str] = field(default_factory=lambda: [])
        interaction_name: str|None = None

    def nodes_dfs(ors: list[list[PrologCall]], node: RuleNode):
        # NOTE(kilian): Generates an "or" list of "anded" PrologCall objects
        for child in node.children:
            if len(child.template.prolog) == 0:
                if node.template.placeholder_id == "QUANTITY":
                    if child.template.placeholder_id == "NUMBER":
                        ors[-1][-1].number.append(child.template.template)
                    else:
                        ors[-1][-1].quantity.append(child.template.template)
                elif node.template.placeholder_id == "INTERACTION":
                    ors[-1][-1].interaction.append(child.template.template)
                else:
                    print(f"ERROR: Unexpected template identifier '{node.template.placeholder_id}' with no prolog data.")
            else:
                match child.template.placeholder_id:
                    case "INTERACTION":
                        ors[-1][-1].interaction_name = child.template.prolog[0]
                    case "QUANTITY":
                        ors[-1][-1].function_name = child.template.prolog[0]
                    case "OPERATION":
                        if "and" in child.template.template:
                            ors[-1].append(PrologCall())
                        elif "or" in child.template.template:
                            ors.append([PrologCall()])
                nodes_dfs(ors, child)

    prologs = [[PrologCall()]]
    nodes_dfs(prologs, root)

    anded_call_strings = []
    for anded_calls in prologs:
        call_strings = []
        for call in anded_calls:
            # NOTE(kilian): Combine arguments in correct order according to rules.pl
            argument_parts = []
            function_name = call.function_name
            if call.quantity:
                argument_parts.append(", ".join(call.quantity))
            if call.interaction:
                argument_parts.append(", ".join(call.interaction))
            if call.interaction_name:
                argument_parts.append(call.interaction_name)
                if call.interaction_name == "grounded":
                    function_name = function_name.removesuffix("_interaction")
            if call.number:
                argument_parts.append(", ".join(call.number))
            argument_parts.append("Structure")
            
            call_strings.append(f"{function_name}({', '.join(argument_parts)})")

        if len(call_strings) == 1:
            anded_call_strings.append(call_strings[0])
        else:
            anded_call_strings.append(f"and([{', '.join(call_strings)}])")

    #for orx in anded_call_strings:
    #    print(orx)
    ored_call_string = f"or([{', '.join(anded_call_strings)}])" if len(anded_call_strings) != 1 else anded_call_strings[0]
    return f"generate_valid_structure([{ored_call_string}], Structure)", f"generate_invalid_structure([{ored_call_string}], Structure)"


# MARK: Combined

def rule_text_to_prolog(rules: Rules, rule: str, starting_template: PlaceholderTemplate, debug_print_parse=False, debug_print_nodes=False) -> tuple[str, str]:
    root = parse_rule_text(rules, rule, starting_template, debug_print=debug_print_parse)
    if debug_print_nodes:
        print_rule_nodes(root)
    return rule_to_prolog(root)


def generate_rule(rules_json_file = 'rules/zendo_rules.json'):
    rules = load_json_rules(rules_json_file)
    starting_template = template_from_text(rules, "A structure must contain QUANTITY.")

    # Generate random rule and parse it into a prolog query
    rule = random_rule(rules, starting_template, two_random_steps=False, print_tree=False)
    query, n_query = rule_text_to_prolog(rules, rule, starting_template, debug_print_parse=False, debug_print_nodes=False)
    return rule, query, n_query


def generate_prolog_structure(num_examples, query, prolog_file = 'rules/rules.pl'):
    prolog = Prolog()
    prolog.consult(prolog_file)

    # Execute the random queries
    results = []
    # Generate num_example structure arrays per rule
    for _ in range(num_examples):
        prolog_query = prolog.query(query)
        for i, szene in enumerate(prolog_query):
            structure = szene["Structure"]
            results.append(structure)

    return results


if __name__ == "__main__":
    rule, query, n_query = generate_rule()
    r1 = generate_prolog_structure(1, query)
    r2 = generate_prolog_structure(1, n_query)

    print(rule)
    print(query, "\n", r1)
    print(n_query, "\n", r2)