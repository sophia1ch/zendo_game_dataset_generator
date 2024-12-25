import sys, os, random, json, re
from dataclasses import dataclass, field
from pyswip import Prolog

@dataclass
class PlaceholderTemplate:
    @dataclass(frozen=True)
    class Token:
        string: str
        is_placeholder: bool
    identifier: str
    template: str
    placeholders: list[str]
    tokens: list[Token]
    prolog: list[str]

@dataclass
class Placeholder:
    categories: dict[str, list[PlaceholderTemplate]]
    templates_of_start_match: dict[str, list[PlaceholderTemplate]]
    all_templates: list[PlaceholderTemplate]
    start_pattern: re.Pattern[str]
    max_references_depth: int = 0
    #referenced_by: dict[str, int] = field(default_factory=lambda: {})
    references: dict[str, int] = field(default_factory=lambda: {})

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

def random_placeholder_template_or_error(rules: Rules, placeholder_identifier: str, two_steps: bool=False, used_templates: dict[str, list[PlaceholderTemplate]]|None = None, no_interaction_when_operator_used: bool = True) -> PlaceholderTemplate|str:
    # NOTE(kilian): if two_steps is true, first a random placeholder category (like "at least" for QUANTITY) is drawn randomly,
    # then a template of that category. Otherwise, a random template is drawn from the templates of all placeholder categories
    # if no_interaction_when_operator_used is True, then no templates with INTERACTION placeholder will be considered if any operator was used previously

    if placeholder_identifier not in rules.placeholders:
        return f"[ERROR: Unknown placeholder {placeholder_identifier}]"

    if two_steps:
        placeholder = rules.placeholders[placeholder_identifier]
        picked_placeholder_value = None
        if len(placeholder.categories) == 1:
            if "" not in placeholder.categories:
                return f"[ERROR: Single value of placeholder {placeholder_identifier} is not \"\"]"
            picked_placeholder_value = ""
        else:
            placeholder_value_keys = list(placeholder.categories.keys())
            picked_placeholder_value = random.choice(placeholder_value_keys)
            
        placeholder_templates = placeholder.categories[picked_placeholder_value]
        if len(placeholder_templates) == 0:
            return f"[ERROR: No templates for placeholder {placeholder_identifier}] at value {picked_placeholder_value}"
    else:
        all_placeholder_templates = []
        for _, placeholder_templates in rules.placeholders[placeholder_identifier].categories.items():
            all_placeholder_templates += placeholder_templates
        placeholder_templates = all_placeholder_templates

        if len(placeholder_templates) == 0:
            return f"[ERROR: No templates for placeholder {placeholder_identifier}]"
    
    picked_template = None
    if placeholder_identifier == "OPERATION":
        # NOTE(kilian): If any operator has already been used, choose the none operator (pick by choosing the one without placeholders)
        used_operators = used_templates.get(placeholder_identifier, [])
        if len(used_operators) > 0:
            for template in placeholder_templates:
                if len(template.placeholders) == 0:
                    picked_template = template
    if picked_template is None:
        # NOTE(kilian): Pick any placeholder template that wasn't used already
        used_placeholder_templates = used_templates.get(placeholder_identifier, [])
        remaining_placeholder_templates = [template for template in placeholder_templates if template not in used_placeholder_templates]
        # TODO(kilian): Possibly an issue in the future because none is also considered as an operator here?
        if no_interaction_when_operator_used and len(used_templates.get("OPERATION", [])) > 0:
            # NOTE(kilian): Remove any templates that contain interaction placeholders
            remaining_placeholder_templates = [template for template in placeholder_templates if "INTERACTION" not in template.placeholders]
        if len(remaining_placeholder_templates) == 0:
            return f"[ERROR: Every template of placeholder {placeholder_identifier} was already used]"
        picked_template = random.choice(remaining_placeholder_templates)

    if placeholder_identifier not in used_templates:
        used_templates[placeholder_identifier] = []
    used_templates[placeholder_identifier].append(picked_template)

    return picked_template

def template_to_string_random_recursive(generator: TemplateGenerator, template: PlaceholderTemplate) -> str:
    if generator.print_tree:
        tabs_string = '\t'*generator.depth
        print(f"{tabs_string}{template.template}")
    
    result = ""
    token_placeholder_count = 0
    for token in template.tokens:
        if token.is_placeholder:
            if generator.print_tree:
                print(f"{tabs_string}-> [{token_placeholder_count}] {token.string}")
            replacement = random_placeholder_template_or_error(generator.rules, token.string, generator.two_random_steps, generator.used_templates)
            if isinstance(replacement, PlaceholderTemplate):
                generator.depth += 1
                replacement = template_to_string_random_recursive(generator, replacement)
                generator.depth -= 1
            result += replacement
            token_placeholder_count += 1
        else:
            result += token.string
    return result

def random_rule(rules: Rules, starting_template: PlaceholderTemplate, two_random_steps: bool=False, print_tree: bool=False) -> str:
    generator = TemplateGenerator(rules, depth=0, used_templates={}, two_random_steps=two_random_steps, print_tree=print_tree)
    string = template_to_string_random_recursive(generator, starting_template)
    return string

def make_placeholder_template(identifier: str, template_text: str, placeholders_list: list[str], prolog_list: list[str]) -> PlaceholderTemplate:
    tokens: list[PlaceholderTemplate.Token] = []
    if placeholders_list:
        placeholders_pattern = re.compile("|".join([re.escape(f"{{{p}}}") for p in placeholders_list]))
        remaining_text = template_text
        
        while True:
            first_match = re.search(placeholders_pattern, remaining_text)
            if first_match is None:
                break
            
            prefix_string = remaining_text[:first_match.start(0)]
            match_string = remaining_text[first_match.start(0) + 1:first_match.end(0) - 1]
            remaining_text = remaining_text[first_match.end(0):]
            if prefix_string != "":
                tokens.append(PlaceholderTemplate.Token(prefix_string, False))
            tokens.append(PlaceholderTemplate.Token(match_string, True))
        if remaining_text != "":
            tokens.append(PlaceholderTemplate.Token(remaining_text, False))
    else:
        tokens.append(PlaceholderTemplate.Token(template_text, False))
    return PlaceholderTemplate(identifier, template_text, placeholders_list, tokens, prolog_list)

def load_json_rules(filename) -> Rules|None:
    with open(filename) as f:
        data = json.load(f)
        placeholders: dict[str, Placeholder] = {}
        
        def find_placeholder_references(rules: Rules):
            for placeholder_identifier, placeholder in rules.placeholders.items():
                for _, placeholder_templates in placeholder.categories.items():
                    for template in placeholder_templates:
                        for referenced_placeholder in template.placeholders:
                            #rules.placeholders[referenced_placeholder].referenced_by[placeholder_identifier] = 1
                            placeholder.references[referenced_placeholder] = 1
            
            # NOTE(kilian): Iteratively update references and referenced by until nothing changes
            while True:
                changed_any = False
                for b, b_placeholder in rules.placeholders.items():
                    '''
                    # NOTE(kilian): For each placeholder c that references b, check if c_referenced_by are all contained in b, else add
                    deeper_b_referenced_by = {}
                    for c, c_depth in b_placeholder.referenced_by.items():
                        for d, d_depth in rules.placeholders[c].referenced_by.items():
                            if d not in b_placeholder.referenced_by:
                                deeper_b_referenced_by[d] = c_depth + d_depth
                                changed_any = True
                    b_placeholder.referenced_by.update(deeper_b_referenced_by)
                    '''
                    deeper_b_references = {}
                    for c, c_depth in b_placeholder.references.items():
                        for d, d_depth in rules.placeholders[c].references.items():
                            if d not in b_placeholder.references:
                                deeper_b_references[d] = c_depth + d_depth
                                changed_any = True
                    b_placeholder.references.update(deeper_b_references)
                if not changed_any:
                    break

            # NOTE(kilian): Compute max depth ("what is the max depth of a reference chain until it ends", might be infinity ^= -1) and categorize
            zero_placeholders: dict[str, Placeholder] = {}
            deep_placeholders: dict[str, Placeholder] = {}
            inf_placeholders: dict[str, Placeholder] = {}
            deep_placeholders_max_depth = 0
            for placeholder_identifier, placeholder in rules.placeholders.items():
                if len(placeholder.references) == 0:
                    zero_placeholders[placeholder_identifier] = placeholder
                    placeholder.max_references_depth = 0
                elif placeholder_identifier in placeholder.references:
                    inf_placeholders[placeholder_identifier] = placeholder
                    placeholder.max_references_depth = -1
                else:
                    deep_placeholders[placeholder_identifier] = placeholder
                    placeholder.max_references_depth = max(placeholder.references.values())
                    deep_placeholders_max_depth = max(deep_placeholders_max_depth, placeholder.max_references_depth)

            for placeholder_identifier, placeholder in zero_placeholders.items():
                raw_pattern_string = "|".join([template.template for templates in placeholder.categories.values() for template in templates ])
                placeholder.pattern_string = raw_pattern_string

        def make_placeholder_template_list(identifier: str, templates):
            result = []
            placeholder_pattern = re.compile(r"{([A-Z]+(?:\|[A-Z]+)*)}")
            placeholder_empty_pattern = re.compile(r"{}")
            for template in templates:
                # NOTE(kilian): Compute list of placeholders
                template_string = template["template"]
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
                        result.append(make_placeholder_template(identifier, template_string, selected_placeholders[:], template["prolog"] if "prolog" in template else []))
                    else:
                        for p in template_placeholders[i]:
                            selected_placeholders[i] = p
                            replaced_template_string = re.sub(placeholder_empty_pattern, f"{{{p}}}", template_string, count=1)
                            configure_placeholders(result, replaced_template_string, i + 1, template_placeholders, selected_placeholders)
                
                selected_placeholders = [0]*len(template_placeholders)
                configure_placeholders(result, template_string, 0, template_placeholders, selected_placeholders)
            #make_placeholder_template(str(t["template"]), t["placeholders"], t["prolog"] if "prolog" in t else []) for t in templates                    
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

        for attribute_identifier, attribute_values in data["ATTRIBUTES"].items():
            placeholders[attribute_identifier] = make_placeholder({ "": [make_placeholder_template(attribute_identifier, str(value), [], []) for value in attribute_values] })
        del data["ATTRIBUTES"]

        for placeholder_identifier, placeholder_values in data.items():
            placeholder_type = type(placeholder_values)
            if placeholder_type == dict:
                placeholders[placeholder_identifier] = make_placeholder({ value: make_placeholder_template_list(placeholder_identifier, templates) for value, templates in placeholder_values.items() })
            else:
                assert(placeholder_type == list)
                placeholders[placeholder_identifier] = make_placeholder({ "": make_placeholder_template_list(placeholder_identifier, placeholder_values) })
        
        templates_of_match = { "": [] }
        for placeholder in placeholders.values():
            update_templates_by_match(templates_of_match, placeholder.all_templates)
        rules = Rules(placeholders=placeholders, templates_of_start_match=templates_of_match, templates_start_pattern=make_templates_by_match_pattern(templates_of_match))
        find_placeholder_references(rules)

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

def template_from_text(rules: Rules, text: str):
    used_placeholders = []
    template = (text + ".")[:-1]
    for placeholder_identifier in rules.placeholders.keys():
        if placeholder_identifier in text:
            template = template.replace(placeholder_identifier, f"{{{placeholder_identifier}}}")
            used_placeholders.append(placeholder_identifier)
    return make_placeholder_template("CUSTOM", template, used_placeholders, [])

# NOTE(kilian): Returns a rule node or a reason why the template does not match.
def parse_rule_text_match(parser: RuleParser, rule: str, template: PlaceholderTemplate) -> tuple[RuleNode, str]|str:
    remaining_rule = rule
    nodes = []
    for i, token in enumerate(template.tokens):
        if parser.print_tree:
            tabs_string = '\t'*parser.depth
            print(f"{tabs_string}[{i}] '{token.string}': '{remaining_rule}'")
        if token.is_placeholder:
            token_placeholder = rules.placeholders[token.string]
            
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

def rule_to_prolog(root: RuleNode):
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
                if node.template.identifier == "QUANTITY":
                    if child.template.identifier == "NUMBER":
                        ors[-1][-1].number.append(child.template.template)
                    else:
                        ors[-1][-1].quantity.append(child.template.template)
                elif node.template.identifier == "INTERACTION":
                    ors[-1][-1].interaction.append(child.template.template)
                else:
                    print(f"ERROR: Unexpected template identifier '{node.template.identifier}' with no prolog data.")
            else:
                match child.template.identifier:
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
    return f"generate_valid_structure([{ored_call_string}], Structure)"

def rule_text_to_prolog(rules: Rules, rule: str, starting_template: PlaceholderTemplate, debug_print_parse=False, debug_print_nodes=False) -> str:
    root = parse_rule_text(rules, rule, starting_template, debug_print=debug_print_parse)
    if debug_print_nodes:
        print_rule_nodes(root)
    return rule_to_prolog(root)

if __name__ == "__main__":
    # TODO(kilian):
    # ~~1 no interaction after and or or~~
    # ~~2 remove used attributes from list of choices~~
    # 3 check valid orientations for shapes (pyramid can not be upside_down, etc.)
    # ~~ 4 grounded as interaction is not handled right (wrong prolog query format) ~~

    prolog = Prolog()
    rule_path = os.path.join(os.path.dirname(__file__), 'rules.pl')
    prolog.consult(rule_path)

    rules = load_json_rules('rules/zendo_rules.json')
    starting_template = template_from_text(rules, "A structure must contain QUANTITY.")

    
    # Execute the random queries
    results = []
    for _ in range(1):
        rule = random_rule(rules, starting_template, two_random_steps=False, print_tree=False)
        #rule = "A structure must contain an even number of block pieces grounded."
        print(rule)
        query = rule_text_to_prolog(rules, rule, starting_template, debug_print_parse=False, debug_print_nodes=False)
        #prolog_query = prolog.query(query)
        #for i, szene in enumerate(prolog_query):
        #    structure = szene["Structure"]
        #    results.append([rule, query, structure])
        print(query)
    '''
    file_name = "rules/rules_output.txt"
    with open(file_name, "w") as file:
        for rule, query, structure in results:
            file.write(str(rule) + "\n" + str(structure) + "\n\n")
    print("Done: saved to " + file_name)
    '''


