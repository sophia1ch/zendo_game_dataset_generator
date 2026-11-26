import sys, os, random, json, re, subprocess
from dataclasses import dataclass, field
from typing import Callable
from pyswip import Prolog
from utils import debug
from hashlib import sha256


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
    # NOTE(claude): Optional override for picking among COLOR/SHAPE/ORIENTATION candidates
    # (e.g. a global-usage-weighted picker for curriculum generation). Left as None, behavior
    # is unchanged (uniform random.choice), so this is opt-in and doesn't affect existing callers.
    attribute_pick_fn: Callable[[list["PlaceholderTemplate"]], "PlaceholderTemplate"] | None = None


# MARK: Exceptions

def raise_unrecognized_placeholder(placeholder_id: str, recognized_placeholders: set[str]):
    """
    Raises an exception saying that placeholder_id is not recognized because it is not part of recognized_placeholders.

    :param placeholder_id: The placeholder identifier that is not recognized.
    :param recognized_placeholders: A set of valid placeholder identifiers.
    :raises Exception: That placeholder_id is not found in the recognized_placeholders.
    """

    recognized_placeholders_string = ", ".join(list(recognized_placeholders))
    raise Exception(
        f"Placeholder '{placeholder_id}' is not recognized as it is not part of the rules placeholders ({recognized_placeholders_string}).")


# MARK: General

def make_placeholder_template(placeholder_id: str, template_text: str, prolog_list: list[str],
                              orientations: list[str]) -> PlaceholderTemplate:
    """
    Creates a placeholder template by tokenizing the given template_text.
    Placeholders in the text must be surrounded by curly braces: {IDENTIFIER}.

    :param placeholder_id: The identifier for the placeholder template.
    :param template_text: The text containing placeholders enclosed in curly braces.
    :param prolog_list: The equivalent prolog identifier of this placeholder template in a list. Should contain a single or no element.
    :param orientations: A list of possible orientations for the placeholder template. Only applicable if of type SHAPE.
    :return: A PlaceholderTemplate object with tokenized text and extracted placeholders.
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


def load_json_rules(filename) -> Rules | None:
    """
    Loads placeholder rules from a JSON file and constructs a Rules object.

    :param filename: The path to the JSON file containing the rules.
    :return: A Rules object containing parsed placeholder rules, or None if loading fails.
    """

    with open(filename) as f:
        data = json.load(f)
        placeholders: dict[str, Placeholder] = {}

        def make_placeholder_template_list(identifier: str, templates: list[dict] | list[str]):
            """
            Creates a list of PlaceholderTemplate objects from a list of template definitions.

            :param identifier: The identifier for the placeholder category.
            :param templates: A list of template definitions, either as dictionaries or strings.
            :return: A list of PlaceholderTemplate objects.
            """

            result = []
            placeholder_pattern = re.compile(r"{([A-Z]+(?:\|[A-Z]+)*)}")
            placeholder_empty_pattern = re.compile(r"{}")
            for template in templates:
                if isinstance(template, dict) and "group" in template and "includes" in template:
                    continue  # Group info is handled outside here
                # NOTE(kilian): Allow for short list where "template" is omitted and only the template values are stored
                template_prolog = template["prolog"] if type(template) == dict and "prolog" in template else []
                template_orientations = template["orientations"] if type(
                    template) == dict and "orientations" in template else []
                template_string = template['template'] if type(template) == dict else str(template)

                # NOTE(kilian): Compute list of placeholders contained in the template
                template_placeholders = []
                while True:
                    placeholder_match = re.search(placeholder_pattern, template_string)
                    if not placeholder_match:
                        break
                    template_placeholder_multiple = placeholder_match.group(1)
                    template_placeholders.append(template_placeholder_multiple.split("|"))
                    template_string = template_string[:placeholder_match.start(1)] + template_string[
                                                                                     placeholder_match.end(1):]

                def configure_placeholders(result, template_string, i, template_placeholders, selected_placeholders):
                    """
                    Recursively configures placeholders by replacing multiple options with actual values.

                    :param result: List to store generated PlaceholderTemplate objects.
                    :param template_string: The template string being processed.
                    :param i: The current index in template_placeholders.
                    :param template_placeholders: A list of possible placeholder values.
                    :param selected_placeholders: Selected placeholders for replacement.
                    """

                    if i >= len(template_placeholders):
                        result.append(make_placeholder_template(identifier, template_string, template_prolog,
                                                                template_orientations))
                    else:
                        for p in template_placeholders[i]:
                            selected_placeholders[i] = p
                            replaced_template_string = re.sub(placeholder_empty_pattern, f"{{{p}}}", template_string,
                                                              count=1)
                            configure_placeholders(result, replaced_template_string, i + 1, template_placeholders,
                                                   selected_placeholders)

                selected_placeholders = [0] * len(template_placeholders)
                configure_placeholders(result, template_string, 0, template_placeholders, selected_placeholders)
            return result

        def update_templates_by_match(templates_by_match, all_templates: list[PlaceholderTemplate]):
            """
            Updates a dictionary mapping starting template tokens to their respective templates.

            :param templates_by_match: Dictionary mapping start tokens to placeholder templates.
            :param all_templates: List of all PlaceholderTemplate objects to be indexed.
            """

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
            """
            Constructs a regular expression pattern to match starting tokens of templates.

            :param templates_by_match: Dictionary mapping start tokens to placeholder templates.
            :return: A compiled regex pattern for template matching.
            """

            templates_start_patern_string = "|".join([re.escape(key) for key in templates_by_match.keys()])
            return re.compile(f"^(?:{templates_start_patern_string})")

        def make_placeholder(categories: dict[str, list[PlaceholderTemplate]]):
            """
            Creates a Placeholder object from categorized templates.

            :param categories: A dictionary mapping categories to lists of PlaceholderTemplate objects.
            :return: A Placeholder object containing categorized templates.
            """

            all_templates = [t for templates in categories.values() for t in templates]
            placeholder_templates_of_start_match = {"": []}
            update_templates_by_match(placeholder_templates_of_start_match, all_templates)
            placeholder_start_pattern = make_templates_by_match_pattern(placeholder_templates_of_start_match)
            return Placeholder(categories, placeholder_templates_of_start_match, all_templates,
                               placeholder_start_pattern)

        for placeholder_identifier, placeholder_values in data.items():
            placeholder_type = type(placeholder_values)
            # NOTE(kilian): Differentiate between categorized templates (like for QUANTITY with "at least") and plain lists of templates
            if placeholder_type == dict:
                placeholders[placeholder_identifier] = make_placeholder(
                    {value: make_placeholder_template_list(placeholder_identifier, templates) for value, templates in
                     placeholder_values.items()})
            else:
                assert (placeholder_type == list)
                placeholders[placeholder_identifier] = make_placeholder(
                    {"": make_placeholder_template_list(placeholder_identifier, placeholder_values)})

        # NOTE(kilian): Group all templates and per placeholder by starting pattern to be able to match more easily later
        templates_of_match = {"": []}
        for placeholder in placeholders.values():
            update_templates_by_match(templates_of_match, placeholder.all_templates)
        rules = Rules(placeholders=placeholders, templates_of_start_match=templates_of_match,
                      templates_start_pattern=make_templates_by_match_pattern(templates_of_match))
        return rules
    return None


def print_rules(rules: Rules):
    """
    Prints all placeholder rules in a structured format.

    :param rules: A Rules object containing placeholders and their templates.
    """

    for placeholder_identifier, placeholder in rules.placeholders.items():
        debug(placeholder_identifier)
        for placeholder_value, placeholder_templates in placeholder.categories.items():
            tabs_str = "\t"
            if placeholder_value != "":
                debug(f"{tabs_str}{placeholder_value}")
                tabs_str += "\t"

            for template in placeholder_templates:
                debug(f"{tabs_str}{template.template}")


def template_from_text(rules: Rules, text: str, placeholders_naked: bool = True) -> PlaceholderTemplate:
    """
    Creates a placeholder template from plain text. Only placeholder identifiers of the given rules are recognized.

    :param rules: A Rules object containing recognized placeholders.
    :param text: The input text containing placeholder identifiers.
    :param placeholders_naked: If True, placeholder identifiers are expected without curly braces and will be wrapped.
    :return: A PlaceholderTemplate object created from the text.
    :raises Exception: If any unrecognized placeholder is found in the text.
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

def random_placeholder_template(rules: Rules, placeholder_id: str, two_steps: bool = False,
                                used_templates: dict[str, list[PlaceholderTemplate]] | None = None,
                                allowed_orientations: list[PlaceholderTemplate] | None = None,
                                no_interaction_when_operator_used: bool = True,
                                attribute_pick_fn: Callable[[list[PlaceholderTemplate]], PlaceholderTemplate] | None = None) -> PlaceholderTemplate:
    """
    Randomly selects a placeholder template from the rules based on the given placeholder_id.
    Ensures that previously used templates (if provided) are not reused.

    :param rules: A Rules object containing placeholder definitions.
    :param placeholder_id: The identifier of the placeholder category to select from.
    :param two_steps: If True, selects a category first, then a template from that category.
    If False, selects from the templates of all categories uniformly.
    :param used_templates: A dictionary tracking already used templates to avoid repetition.
    :param allowed_orientations: A list of specific orientation templates to be considered, if applicable.
    :param no_interaction_when_operator_used: If True, avoids interaction templates if an operator was used previously.
    :param attribute_pick_fn: If given and placeholder_id is COLOR/SHAPE/ORIENTATION, used instead of a
        uniform random.choice to pick among the remaining candidates (e.g. a global-usage-weighted picker).
    :return: A randomly chosen PlaceholderTemplate object.
    :raises Exception: If no valid templates are available.
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
            raise Exception(
                f"No templates for placeholder '{placeholder_id}' in category '{picked_placeholder_category}'.")
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
        remaining_placeholder_templates = [template for template in placeholder_templates if
                                           template.template not in used_placeholder_template_texts]
        # TODO(kilian): Possibly an issue in the future because none is also considered as an operator here?
        if no_interaction_when_operator_used and len(used_templates.get("OPERATION", [])) > 0:
            # NOTE(kilian): Remove any templates that contain interaction placeholders
            remaining_placeholder_templates = [template for template in remaining_placeholder_templates if
                                               "INTERACTION" not in template.placeholders]
        if len(remaining_placeholder_templates) == 0:
            raise Exception(f"Every template of placeholder '{placeholder_id}' was already used.")
        if attribute_pick_fn is not None and placeholder_id in ("COLOR", "SHAPE", "ORIENTATION"):
            picked_template = attribute_pick_fn(remaining_placeholder_templates)
        else:
            picked_template = random.choice(remaining_placeholder_templates)

    if placeholder_id not in used_templates:
        used_templates[placeholder_id] = []
    used_templates[placeholder_id].append(picked_template)

    return picked_template


def template_to_string_random_recursive(generator: TemplateGenerator, template: PlaceholderTemplate) -> str:
    """
    Recursively generates a random string representation of a template by replacing its placeholders with corresponding values.

    :param generator: A TemplateGenerator object containing the rules and used templates.
    :param template: The PlaceholderTemplate object to be processed.
    :return: A string with placeholders replaced by randomly selected values.
    """

    if generator.print_tree:
        tabs_string = '\t' * generator.depth
        debug(f"{tabs_string}{template.template}")

    shape_replacements = []
    # NOTE(kilian): Pre generate all shape placeholders such that orientation restrictions can be applied
    for token in template.tokens:
        if token.is_placeholder and token.string == "SHAPE":
            try:
                shape_replacement = random_placeholder_template(generator.rules, token.string,
                                                                generator.two_random_steps, generator.used_templates,
                                                                attribute_pick_fn=generator.attribute_pick_fn)
            except Exception as e:
                shape_replacement = f"[Error: {str(e)}]"
            shape_replacements.append(shape_replacement)

    def compute_allowed_orientations(shape_index: int) -> list[PlaceholderTemplate] | None:
        """
        Determines the allowed orientation templates for a given shape index.

        :param shape_index: The index of the shape in the list of shape replacements.
        :return: A list of allowed orientation templates or None if unrestricted.
        """

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
                    orientation_templates = generator.rules.placeholders["ORIENTATION"].templates_of_start_match[
                        orientation_string]
                    assert (len(orientation_templates) == 1)
                    allowed_orientations.append(orientation_templates[0])
                else:
                    debug(f"Warning: ORIENTATION '{orientation_string}' drawn SHAPE {shape_replacement.template} doesn't exist.")
        return allowed_orientations

    next_shape_index = 0

    result = ""
    token_placeholder_count = 0
    for i, token in enumerate(template.tokens):
        if token.is_placeholder:
            if generator.print_tree:
                debug(f"{tabs_string}-> [{token_placeholder_count}] {token.string}")
            if token.string == "SHAPE":
                replacement = shape_replacements[next_shape_index]
                if isinstance(replacement, PlaceholderTemplate):
                    result += replacement.template
                else:
                    result += replacement
                next_shape_index += 1
            else:
                allowed_orientations = None
                # NOTE(kilian): Skip over purely whitespace literal tokens (e.g. the space in
                # "{ORIENTATION} {SHAPE}") so an ORIENTATION directly describing the following
                # SHAPE is still recognized as adjacent to it, restricting to orientations that
                # shape can physically have (e.g. never "cheesecake block" or "flat wedge").
                next_index = i + 1
                while next_index < len(template.tokens) and not template.tokens[next_index].is_placeholder \
                        and template.tokens[next_index].string.strip() == "":
                    next_index += 1
                if next_index < len(template.tokens):
                    next_token = template.tokens[next_index]
                    if next_token.is_placeholder and next_token.string == "SHAPE":
                        allowed_orientations = compute_allowed_orientations(next_shape_index)
                try:
                    replacement = random_placeholder_template(generator.rules, token.string, generator.two_random_steps,
                                                              generator.used_templates, allowed_orientations,
                                                              attribute_pick_fn=generator.attribute_pick_fn)
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


def random_rule(rules: Rules, starting_template: PlaceholderTemplate, two_random_steps: bool = False,
                print_tree: bool = False) -> str:
    """
    Generates a random rule as a string by recursively replacing placeholders in a starting template.

    :param rules: A Rules object containing placeholder definitions.
    :param starting_template: The initial PlaceholderTemplate from which the rule is generated.
    :param two_random_steps: If True, selects placeholder templates in two steps (category first, then template).
    :param print_tree: If True, prints the generation process as a tree structure.
    :return: A string representation of the randomly generated rule.
    """

    generator = TemplateGenerator(rules, depth=0, used_templates={}, two_random_steps=two_random_steps,
                                  print_tree=print_tree)
    string = template_to_string_random_recursive(generator, starting_template)
    return string


# MARK: Parsing

def parse_rule_text_match(parser: RuleParser, rule: str, template: PlaceholderTemplate) -> tuple[RuleNode, str] | str:
    """
    Matches a given rule text against a template recursively and constructs a corresponding RuleNode tree.

    :param parser: A RuleParser object that holds parsing settings and rules.
    :param rule: The rule text to be parsed.
    :param template: The PlaceholderTemplate against which the rule text is matched.
    :return: A tuple containing the parsed RuleNode and the remaining unmatched rule text,
             or an error message specifying why the template does not match.
    """

    remaining_rule = rule
    nodes = []
    for i, token in enumerate(template.tokens):
        if parser.print_tree:
            tabs_string = '\t' * parser.depth
            debug(f"{tabs_string}[{i}] '{token.string}': '{remaining_rule}'")
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


def parse_rule_text(rules: Rules, rule: str, starting_template: PlaceholderTemplate,
                    debug_print=False) -> RuleNode | None:
    """
    Parses the given rule text and generates a tree of RuleNodes based on a starting template.

    :param rules: A Rules object containing all placeholders and templates.
    :param rule: The rule text to parse.
    :param starting_template: The initial PlaceholderTemplate used for matching the rule text.
    :param debug_print: If True, prints debugging information during the parsing process.
    :return: The root RuleNode of the parsed rule tree, or None if parsing fails.
    """

    parser = RuleParser(rules, depth=0, print_tree=debug_print)
    parse_match = parse_rule_text_match(parser, rule, starting_template)
    if isinstance(parse_match, str):
        return None
    root, remaining_rule = parse_match
    return root


def print_rule_nodes(root: RuleNode):
    """
    Prints the structure of a rule node tree in a readable format.

    :param root: The root RuleNode of the rule tree.
    """

    nodes = [(0, root)]
    depth = 0
    while nodes:
        new_nodes = []
        i = 0
        for (num, node) in nodes:
            debug(f"{depth} {num} {node.template.template}")
            for c in node.children:
                new_nodes.append((i, c))
            i += 1
        nodes = new_nodes
        depth += 1


# MARK: Prolog conversion

def rule_to_prolog(root: RuleNode) -> tuple[str, str]:
    """
    Converts a parsed rule tree into corresponding Prolog queries.

    :param root: The root RuleNode representing the parsed rule.
    :return: A tuple containing Prolog queries for valid and invalid structure generation.
    """

    @dataclass
    class PrologCall:
        function_name: str | None = None
        quantity: list[str] = field(default_factory=lambda: [])
        number: list[str] = field(default_factory=lambda: [])
        interaction: list[str] = field(default_factory=lambda: [])
        interaction_name: str | None = None

    def nodes_dfs(ors: list[list[PrologCall]], node: RuleNode):
        """
        Recursively traverses the rule tree to construct Prolog calls.

        :param ors: A list of lists containing PrologCall objects for 'or' and 'and' logic.
        :param node: The current RuleNode being processed.
        """

        # NOTE(kilian): Generates an "or" list of "anded" PrologCall objects
        if node is None:
            return
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
                    debug(f"ERROR: Unexpected template identifier '{node.template.placeholder_id}' with no prolog data.")
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

    ored_call_string = f"or([{', '.join(anded_call_strings)}])" if len(anded_call_strings) != 1 else anded_call_strings[0]
    return f"generate_valid_structure([{ored_call_string}], Structure)", f"generate_invalid_structure([{ored_call_string}], Structure)"


# MARK: Combined

def rule_text_to_prolog(rules: Rules, rule: str, starting_template: PlaceholderTemplate, debug_print_parse=False,
                        debug_print_nodes=False) -> tuple[str, str]:
    """
    Converts a textual rule into its corresponding Prolog representation.

    :param rules: A Rules object containing placeholder definitions.
    :param rule: The rule text to be converted.
    :param starting_template: The initial PlaceholderTemplate used to parse the rule.
    :param debug_print_parse: If True, enables debug printing for the parsing process.
    :param debug_print_nodes: If True, prints the parsed rule node structure.
    :return: A tuple containing Prolog queries for valid and invalid structure generation.
    """

    root = parse_rule_text(rules, rule, starting_template, debug_print=debug_print_parse)
    if debug_print_nodes:
        print_rule_nodes(root)
    return rule_to_prolog(root)


# MARK: Validation

def call_prolog_subprocess(n, query, prolog_file, timeout=6):
    """
    Runs generate_prolog_structure for a single query in an isolated subprocess with a hard
    timeout. Some generated rules are logically impossible (e.g. comparing an attribute count
    against a strict superset of itself, such as "more upright pieces than vertical pieces",
    or "and"-ing together two attribute checks that contradict each other). Their Prolog query
    would otherwise search forever, since the underlying "repeat" loop never gives up on its
    own. The timeout turns that infinite loop into a clean "not realizable" result instead of
    a hang, mirroring the same guard already used when rendering scenes (see render.py).

    :param n: Number of structures to request.
    :param query: The Prolog query string to run (as produced by rule_to_prolog).
    :param prolog_file: Path to the Prolog rules file to consult.
    :param timeout: Maximum number of seconds to allow the subprocess to run.
    :return: A list of generated structures, or None if the query timed out or failed.
    """

    script_path = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                               "call_generate_prolog.py")
    try:
        result = subprocess.check_output(
            [sys.executable, script_path, str(n), query, prolog_file],
            timeout=timeout, stderr=subprocess.DEVNULL)
        return json.loads(result)
    except (subprocess.TimeoutExpired, subprocess.CalledProcessError, json.JSONDecodeError):
        return None


def is_rule_realizable(query, n_query, prolog_file='rules/rules.pl', timeout=6):
    """
    Empirically verifies that a generated rule is actually realizable: at least one structure
    that fulfills it (valid) and one that violates it (invalid) must exist. This catches rules
    that read fine as text but are logically impossible, such as comparing an attribute to a
    group that always contains it (e.g. "more upright pieces than vertical pieces" can never be
    true, since every upright piece already counts as vertical) or combining two attribute
    counts via "and"/"or" in a way that contradicts itself (e.g. "exactly 2 vertical pieces and
    exactly 3 upright pieces").

    :param query: Prolog query string generating a structure that fulfills the rule.
    :param n_query: Prolog query string generating a structure that violates the rule.
    :param prolog_file: Path to the Prolog rules file to consult.
    :param timeout: Maximum number of seconds to allow each check to run.
    :return: True if both a valid and an invalid structure could be generated, False otherwise.
    """

    if not call_prolog_subprocess(1, query, prolog_file, timeout=timeout):
        return False
    if not call_prolog_subprocess(1, n_query, prolog_file, timeout=timeout):
        return False
    return True


def generate_valid_rule(rules_json_file='rules/zendo_rules.json', prolog_file='rules/rules.pl',
                        max_attempts=30, timeout=6):
    """
    Generates a random rule and, if it turns out to be unrealizable, keeps drawing a fresh
    random rule (up to max_attempts times) instead of returning something that could never be
    satisfied by any structure. This is what lets a caller ask for N rules and actually get N
    usable rules back, rather than needing to discard a fraction of them afterwards.

    :param rules_json_file: The path to the JSON file containing placeholder rules.
    :param prolog_file: Path to the Prolog rules file used to check realizability.
    :param max_attempts: Maximum number of candidate rules to try before giving up.
    :param timeout: Maximum number of seconds to allow each realizability check to run.
    :return: A tuple (rule, query, n_query, attempts_used), or None if no realizable rule was
             found within max_attempts.
    """

    for attempt in range(1, max_attempts + 1):
        rule, query, n_query = generate_rule(rules_json_file)
        if "Error" in rule:
            continue
        if is_rule_realizable(query, n_query, prolog_file, timeout=timeout):
            return rule, query, n_query, attempt
    return None


# MARK: Curriculum generation
#
# Generates a fixed, structured set of rules rather than a purely random sample: a set
# number of single-predicate instances per predicate identity, plus a set of two-predicate
# "and"/"or" combinations covering every predicate identity exactly once.

_QUANTITY_ATTR_PLACEHOLDERS = {"COLOR", "ORIENTATION", "SHAPE"}

_RULE_PREFIX = "A structure must contain "
_RULE_SUFFIX = "."

# cheesecake/doorstop are only achievable by wedges (blocks/pyramids can't have those
# orientations), so weighting them the same as the other orientations would silently force
# wedges to be picked disproportionately often just to satisfy those draws.
DISCOUNTED_ATTRIBUTE_VALUES = {"cheesecake", "doorstop"}
DISCOUNTED_ATTRIBUTE_WEIGHT = 0.5


def make_balanced_attribute_picker(usage_counts: dict[str, int] | None = None):
    """
    Builds a weighted-pick function for COLOR/SHAPE/ORIENTATION candidates that favors
    whichever attribute value has been used least so far, so the aggregate distribution
    across a whole generation run stays roughly even. cheesecake/doorstop are discounted
    relative to the other orientations (see DISCOUNTED_ATTRIBUTE_WEIGHT) so satisfying them
    doesn't force wedges to be over-represented.

    :param usage_counts: An optional starting counts dict (attribute value text -> count),
        e.g. pre-seeded from already-hardcoded rules so the picker accounts for them from the
        start. Mutated in place as picks are made; pass the same dict to share balancing state
        across multiple generation calls.
    :return: A function (candidates: list[PlaceholderTemplate]) -> PlaceholderTemplate that
        also records the pick into usage_counts.
    """

    counts = usage_counts if usage_counts is not None else {}

    def weight_of(template: PlaceholderTemplate) -> float:
        value = template.template
        base = DISCOUNTED_ATTRIBUTE_WEIGHT if value in DISCOUNTED_ATTRIBUTE_VALUES else 1.0
        return base / (counts.get(value, 0) + 1)

    def pick(candidates: list[PlaceholderTemplate]) -> PlaceholderTemplate:
        weights = [weight_of(t) for t in candidates]
        chosen = random.choices(candidates, weights=weights, k=1)[0]
        counts[chosen.template] = counts.get(chosen.template, 0) + 1
        return chosen

    return pick
_VALID_QUERY_PREFIX = "generate_valid_structure(["
_VALID_QUERY_SUFFIX = "], Structure)"


def classify_quantity_template(template: PlaceholderTemplate) -> str:
    """
    Classifies a concrete QUANTITY template into one of the 22 canonical predicate identities
    (AT_LEAST_1, EXACTLY_2, MORE_THAN, EITHER_OR, ...) based on its prolog function name and
    which placeholders it contains (attribute count, presence of INTERACTION).

    :param template: A concrete PlaceholderTemplate from the QUANTITY placeholder's template pool.
    :return: The canonical predicate identity string.
    :raises Exception: If the template doesn't match any known predicate identity.
    """

    fn = template.prolog[0] if template.prolog else None
    attr_count = len(_QUANTITY_ATTR_PLACEHOLDERS & template.placeholders)
    has_interaction = "INTERACTION" in template.placeholders

    if fn == "zero":
        return "ZERO_1" if attr_count == 1 else "ZERO_2"
    if fn == "at_least":
        return "AT_LEAST_1" if attr_count == 1 else "AT_LEAST_2"
    if fn == "exactly":
        return "EXACTLY_1" if attr_count == 1 else "EXACTLY_2"
    if fn == "at_least_interaction":
        return "AT_LEAST_INTERACTION"
    if fn == "exactly_interaction":
        return "EXACTLY_INTERACTION"
    if fn == "more_than":
        return "MORE_THAN"
    if fn == "same_amount":
        return "SAME_AMOUNT"
    if fn == "odd_number_of":
        if has_interaction:
            return "ODD_INTERACTION"
        return "ODD" if attr_count == 0 else ("ODD_1" if attr_count == 1 else "ODD_2")
    if fn == "even_number_of":
        if has_interaction:
            return "EVEN_INTERACTION"
        return "EVEN" if attr_count == 0 else ("EVEN_1" if attr_count == 1 else "EVEN_2")
    if fn == "odd_number_of_interaction":
        return "ODD_INTERACTION"
    if fn == "even_number_of_interaction":
        return "EVEN_INTERACTION"
    if fn == "either_or":
        return "EITHER_OR"
    if fn == "all_three_shapes":
        return "ALL_THREE_SHAPES"
    if fn == "all_three_colors":
        return "ALL_THREE_COLORS"
    if fn == "exclusively":
        return "EXCLUSIVELY"
    raise Exception(
        f"Unrecognized QUANTITY template for curriculum classification: '{template.template}' (prolog={template.prolog}).")


def build_predicate_template_index(rules: Rules) -> dict[str, list[PlaceholderTemplate]]:
    """
    Groups all concrete QUANTITY templates by their canonical predicate identity.

    :param rules: A Rules object containing placeholder definitions.
    :return: A dict mapping each predicate identity to its concrete templates.
    """

    index: dict[str, list[PlaceholderTemplate]] = {}
    for template in rules.placeholders["QUANTITY"].all_templates:
        predicate = classify_quantity_template(template)
        index.setdefault(predicate, []).append(template)
    return index


def generate_predicate_instance(rules: Rules, predicate_templates: list[PlaceholderTemplate],
                                starting_template: PlaceholderTemplate, prolog_file: str,
                                max_attempts: int = 40, timeout: int = 6,
                                attribute_pick_fn: Callable[[list[PlaceholderTemplate]], PlaceholderTemplate] | None = None):
    """
    Generates one realizable, single-clause (no "and"/"or") rule instance drawn from a
    predicate's concrete templates.

    :param rules: A Rules object containing placeholder definitions.
    :param predicate_templates: The concrete templates belonging to one predicate identity.
    :param starting_template: The "A structure must contain QUANTITY." starting template,
        used to re-parse generated text back into a RuleNode tree.
    :param prolog_file: Path to the Prolog rules file used to check realizability.
    :param max_attempts: Maximum number of draws to try before giving up.
    :param timeout: Maximum number of seconds to allow each realizability check to run.
    :param attribute_pick_fn: Optional weighted picker for COLOR/SHAPE/ORIENTATION values
        (see make_balanced_attribute_picker), used to keep the aggregate attribute
        distribution balanced across a whole curriculum generation run.
    :return: A tuple (rule_text, query, n_query, inner_check), or None if no realizable
        single-clause instance was found within max_attempts.
    """

    for _ in range(max_attempts):
        chosen = random.choice(predicate_templates)
        generator = TemplateGenerator(rules, depth=0, used_templates={}, two_random_steps=False, print_tree=False,
                                      attribute_pick_fn=attribute_pick_fn)
        phrase = template_to_string_random_recursive(generator, chosen)
        if "Error" in phrase:
            continue
        rule_text = f"{_RULE_PREFIX}{phrase}{_RULE_SUFFIX}"
        root = parse_rule_text(rules, rule_text, starting_template)
        if root is None:
            continue
        query, n_query = rule_to_prolog(root)
        if "and([" in query or "or([" in query:
            continue  # NOTE(claude): OPERATION rolled a combinator, redraw for a clean single-clause instance
        if not is_rule_realizable(query, n_query, prolog_file, timeout=timeout):
            continue
        inner_check = query[len(_VALID_QUERY_PREFIX):-len(_VALID_QUERY_SUFFIX)]
        return rule_text, query, n_query, inner_check
    return None


def generate_predicate_instances(rules: Rules, predicate_templates: list[PlaceholderTemplate],
                                 starting_template: PlaceholderTemplate, prolog_file: str,
                                 count: int = 4, max_attempts: int = 80, timeout: int = 6,
                                 max_duplicate_streak: int = 15,
                                 attribute_pick_fn: Callable[[list[PlaceholderTemplate]], PlaceholderTemplate] | None = None,
                                 already_used_queries: set[str] | None = None):
    """
    Generates up to `count` realizable single-clause instances of a predicate, preferring
    textually distinct ones. Some predicates (e.g. "an odd number of total pieces",
    "pieces of all three shapes") have only one possible phrasing at all, so after
    max_duplicate_streak consecutive duplicate draws this falls back to accepting repeats
    rather than looping forever trying to find variety that doesn't exist.

    :param rules: A Rules object containing placeholder definitions.
    :param predicate_templates: The concrete templates belonging to one predicate identity.
    :param starting_template: The "A structure must contain QUANTITY." starting template.
    :param prolog_file: Path to the Prolog rules file used to check realizability.
    :param count: How many instances to collect.
    :param max_attempts: Maximum number of draws to try before giving up.
    :param timeout: Maximum number of seconds to allow each realizability check to run.
    :param max_duplicate_streak: Consecutive duplicate draws to tolerate before allowing repeats.
    :param attribute_pick_fn: Optional weighted picker for COLOR/SHAPE/ORIENTATION values
        (see make_balanced_attribute_picker).
    :param already_used_queries: Queries already used elsewhere (e.g. a hardcoded seed for this
        same predicate) that should count as "already seen" so freshly generated instances
        don't accidentally repeat them.
    :return: A list of up to `count` (rule_text, query, n_query) tuples.
    """

    instances = []
    seen_queries = set(already_used_queries) if already_used_queries else set()
    duplicate_streak = 0
    attempts = 0
    while len(instances) < count and attempts < max_attempts:
        attempts += 1
        result = generate_predicate_instance(rules, predicate_templates, starting_template, prolog_file,
                                             max_attempts=1, timeout=timeout, attribute_pick_fn=attribute_pick_fn)
        if result is None:
            continue
        rule_text, query, n_query, _ = result
        if query in seen_queries:
            duplicate_streak += 1
            if duplicate_streak < max_duplicate_streak:
                continue
        else:
            duplicate_streak = 0
        seen_queries.add(query)
        instances.append((rule_text, query, n_query))
    return instances


def generate_paired_instance(rules: Rules, predicate_templates_a: list[PlaceholderTemplate],
                             predicate_templates_b: list[PlaceholderTemplate], combiner: str,
                             starting_template: PlaceholderTemplate, prolog_file: str,
                             max_attempts: int = 60, timeout: int = 6,
                             attribute_pick_fn: Callable[[list[PlaceholderTemplate]], PlaceholderTemplate] | None = None):
    """
    Generates one realizable compound rule combining one instance from each of two predicates
    via "and" or "or". Individually-realizable instances can still be jointly unsatisfiable
    (e.g. two "exactly" checks that contradict each other), so the combined rule is
    re-verified as a whole and both sides are redrawn together on failure.

    :param rules: A Rules object containing placeholder definitions.
    :param predicate_templates_a: Concrete templates for the first predicate.
    :param predicate_templates_b: Concrete templates for the second predicate.
    :param combiner: Either "and" or "or".
    :param starting_template: The "A structure must contain QUANTITY." starting template.
    :param prolog_file: Path to the Prolog rules file used to check realizability.
    :param max_attempts: Maximum number of (redraw both sides) attempts before giving up.
    :param timeout: Maximum number of seconds to allow each realizability check to run.
    :param attribute_pick_fn: Optional weighted picker for COLOR/SHAPE/ORIENTATION values
        (see make_balanced_attribute_picker).
    :return: A tuple (rule_text, query, n_query), or None if no realizable combination was
        found within max_attempts.
    """

    already_failed = set()
    for _ in range(max_attempts):
        inst_a = generate_predicate_instance(rules, predicate_templates_a, starting_template, prolog_file,
                                            max_attempts=5, timeout=timeout, attribute_pick_fn=attribute_pick_fn)
        inst_b = generate_predicate_instance(rules, predicate_templates_b, starting_template, prolog_file,
                                            max_attempts=5, timeout=timeout, attribute_pick_fn=attribute_pick_fn)
        if inst_a is None or inst_b is None:
            continue
        rule_text_a, _, _, inner_a = inst_a
        rule_text_b, _, _, inner_b = inst_b
        phrase_a = rule_text_a[len(_RULE_PREFIX):-len(_RULE_SUFFIX)]
        phrase_b = rule_text_b[len(_RULE_PREFIX):-len(_RULE_SUFFIX)]
        rule_text = f"{_RULE_PREFIX}{phrase_a} {combiner} {phrase_b}{_RULE_SUFFIX}"
        combined_inner = f"{combiner}([{inner_a}, {inner_b}])"
        if combined_inner in already_failed:
            # NOTE(claude): This exact combination already failed once. If either side has no
            # attribute placeholders to vary (e.g. "an even number of total pieces" combined
            # with "an odd number of total pieces" - a mathematical contradiction, always
            # unsatisfiable), every future draw will keep landing on the identical combination
            # and time out identically. Retrying up to max_attempts times would just burn a
            # full timeout on the same guaranteed failure over and over, so stop here instead.
            break
        query = f"generate_valid_structure([{combined_inner}], Structure)"
        n_query = f"generate_invalid_structure([{combined_inner}], Structure)"
        if is_rule_realizable(query, n_query, prolog_file, timeout=timeout):
            return rule_text, query, n_query
        already_failed.add(combined_inner)
    return None


# Pre-existing rules pulled from zendo-model/data/combined_dataset_short.pkl (a sibling
# project's dataset), hardcoded here so this repo doesn't need a runtime dependency on that
# project or its pickle format. Each pickle entry's first element is the bare Prolog "Checks"
# expression (e.g. "at_least(blue, 3, Structure)"); the English rule text isn't stored there,
# so it's been composed here to match this repo's exact phrasing conventions. `attrs` records
# which COLOR/SHAPE/ORIENTATION values each rule consumes, so the balanced attribute picker
# (see make_balanced_attribute_picker) can be pre-seeded to account for them from the start.
#
# 20 of the 22 pickle rules are single-predicate and map 1:1 onto a predicate identity each
# (all but ZERO_2 and EXACTLY_INTERACTION, which have no pre-existing seed). The remaining 2
# are already "and"/"or" compounds; both happen to reuse AT_LEAST_1 (once each), so folding
# them in as 2 of the 11 pairs means AT_LEAST_1 ends up used twice in the paired section and
# exactly one of the other 21 predicates ends up with zero appearances there instead of one -
# an unavoidable consequence of the source data, not a bug.
HARDCODED_SINGLE_SEEDS: dict[str, tuple[str, str, list[str]]] = {
    "AT_LEAST_1": ("A structure must contain at least 3 blue pieces.",
                   "at_least(blue, 3, Structure)", ["blue"]),
    "EVEN_INTERACTION": ("A structure must contain an even number of red pieces pointing at a flat piece.",
                        "even_number_of_interaction(red, flat, pointing, Structure)", ["red", "flat"]),
    "MORE_THAN": ("A structure must contain more wedge pieces than cheesecake pieces.",
                 "more_than(wedge, cheesecake, Structure)", ["wedge", "cheesecake"]),
    "AT_LEAST_INTERACTION": ("A structure must contain at least 1 yellow pieces on top of another cheesecake piece.",
                            "at_least_interaction(yellow, cheesecake, on_top_of, 1, Structure)", ["yellow", "cheesecake"]),
    "EXACTLY_2": ("A structure must contain exactly 1 yellow flat pieces.",
                 "exactly(yellow, flat, 1, Structure)", ["yellow", "flat"]),
    "EVEN_1": ("A structure must contain an even number of block pieces.",
              "even_number_of(block, Structure)", ["block"]),
    "ODD_2": ("A structure must contain an odd number of flat wedge pieces.",
             "odd_number_of(flat, wedge, Structure)", ["flat", "wedge"]),
    "EITHER_OR": ("A structure must contain either 2 or 1 of total pieces.",
                 "either_or(2, 1, Structure)", []),
    "ODD_INTERACTION": ("A structure must contain an odd number of yellow pieces touching a wedge piece.",
                       "odd_number_of_interaction(yellow, wedge, touching, Structure)", ["yellow", "wedge"]),
    "EVEN_2": ("A structure must contain an even number of blue upright pieces.",
              "even_number_of(blue, upright, Structure)", ["blue", "upright"]),
    "AT_LEAST_2": ("A structure must contain at least 3 yellow block pieces.",
                  "at_least(yellow, block, 3, Structure)", ["yellow", "block"]),
    "EXACTLY_1": ("A structure must contain exactly 3 flat pieces.",
                 "exactly(flat, 3, Structure)", ["flat"]),
    "SAME_AMOUNT": ("A structure must contain same amount of blue pieces and red pieces.",
                   "same_amount(blue, red, Structure)", ["blue", "red"]),
    "ALL_THREE_SHAPES": ("A structure must contain pieces of all three shapes.",
                        "all_three_shapes(Structure)", []),
    "ALL_THREE_COLORS": ("A structure must contain pieces of all three colors.",
                        "all_three_colors(Structure)", []),
    "EXCLUSIVELY": ("A structure must contain exclusively upright pieces.",
                   "exclusively(upright, Structure)", ["upright"]),
    "ZERO_1": ("A structure must contain zero pyramid pieces.",
              "zero(pyramid, Structure)", ["pyramid"]),
    "ODD": ("A structure must contain an odd number of total pieces.",
           "odd_number_of(Structure)", []),
    "EVEN": ("A structure must contain an even number of total pieces.",
            "even_number_of(Structure)", []),
    "ODD_1": ("A structure must contain an odd number of doorstop pieces.",
             "odd_number_of(doorstop, Structure)", ["doorstop"]),
}

# (predicate_a, predicate_b, combiner, rule_text, checks_a, checks_b, attrs)
HARDCODED_PAIRED_SEEDS: list[tuple[str, str, str, str, str, str, list[str]]] = [
    ("AT_LEAST_1", "ODD_1", "or",
     "A structure must contain at least 3 block pieces or an odd number of flat pieces.",
     "at_least(block, 3, Structure)", "odd_number_of(flat, Structure)", ["block", "flat"]),
    ("AT_LEAST_1", "EXACTLY_1", "and",
     "A structure must contain at least 3 red pieces and exactly 1 pyramid pieces.",
     "at_least(red, 3, Structure)", "exactly(pyramid, 1, Structure)", ["red", "pyramid"]),
]


def generate_curriculum_rules(rules_json_file: str = 'rules/zendo_rules.json', prolog_file: str = 'rules/rules.pl',
                              per_predicate: int = 4, num_and: int = 6, num_or: int = 5,
                              seed: int | None = None, timeout: int = 6):
    """
    Generates the full curriculum rule set: `per_predicate` single-clause instances of each
    of the 22 predicate identities (seeded with the corresponding HARDCODED_SINGLE_SEEDS entry
    where one exists, topped up with freshly generated instances), plus `num_and` + `num_or`
    two-predicate "and"/"or" combinations (seeded with HARDCODED_PAIRED_SEEDS, topped up with a
    fresh random matching over whichever predicates aren't already spent by those seeds) so
    each identity appears (at least) once across the combinations.

    COLOR/SHAPE/ORIENTATION values are drawn via a usage-balanced picker shared across the
    whole run (see make_balanced_attribute_picker), pre-seeded with the hardcoded rules'
    attribute usage, so the aggregate distribution stays even instead of skewing toward
    whatever a purely uniform random choice happens to favor.

    :param rules_json_file: The path to the JSON file containing placeholder rules.
    :param prolog_file: Path to the Prolog rules file used to check realizability.
    :param per_predicate: How many single-clause instances to generate per predicate identity.
    :param num_and: How many predicate pairs to combine with "and" (including hardcoded ones).
    :param num_or: How many predicate pairs to combine with "or" (including hardcoded ones).
    :param seed: Optional random seed for reproducibility.
    :param timeout: Maximum number of seconds to allow each realizability check to run.
    :return: A tuple (single_entries, paired_entries).
        single_entries: list of (predicate_name, rule, query, n_query).
        paired_entries: list of (predicate_a, predicate_b, combiner, rule, query, n_query),
            where rule/query/n_query are None if no realizable combination was found.
    """

    if seed is not None:
        random.seed(seed)

    rules = load_json_rules(rules_json_file)
    starting_template = template_from_text(rules, "A structure must contain QUANTITY.")
    predicate_index = build_predicate_template_index(rules)
    predicate_names = list(predicate_index.keys())

    usage_counts: dict[str, int] = {}
    attribute_pick_fn = make_balanced_attribute_picker(usage_counts)

    def record_usage(attrs: list[str]):
        for attr in attrs:
            usage_counts[attr] = usage_counts.get(attr, 0) + 1

    # Seed the paired section first, so its predicates can be excluded from the fresh matching.
    paired_entries = []
    pairs_used_predicates = set()
    for predicate_a, predicate_b, combiner, rule_text, checks_a, checks_b, attrs in HARDCODED_PAIRED_SEEDS:
        record_usage(attrs)
        combined_inner = f"{combiner}([{checks_a}, {checks_b}])"
        query = f"generate_valid_structure([{combined_inner}], Structure)"
        n_query = f"generate_invalid_structure([{combined_inner}], Structure)"
        paired_entries.append((predicate_a, predicate_b, combiner, rule_text, query, n_query))
        pairs_used_predicates.add(predicate_a)
        pairs_used_predicates.add(predicate_b)

    # Seed the single-predicate buckets, topping each up to per_predicate with fresh instances.
    single_entries = []
    for name in predicate_names:
        instances = []
        if name in HARDCODED_SINGLE_SEEDS:
            rule_text, checks, attrs = HARDCODED_SINGLE_SEEDS[name]
            record_usage(attrs)
            query = f"generate_valid_structure([{checks}], Structure)"
            n_query = f"generate_invalid_structure([{checks}], Structure)"
            instances.append((rule_text, query, n_query))
        remaining = per_predicate - len(instances)
        if remaining > 0:
            already_used = {query for _, query, _ in instances}
            instances += generate_predicate_instances(rules, predicate_index[name], starting_template, prolog_file,
                                                       count=remaining, timeout=timeout,
                                                       attribute_pick_fn=attribute_pick_fn,
                                                       already_used_queries=already_used)
        for rule_text, query, n_query in instances[:per_predicate]:
            single_entries.append((name, rule_text, query, n_query))

    # Fresh perfect matching for the remaining pairs, excluding predicates already spent above.
    # An odd leftover count (from the hardcoded pairs reusing a predicate) means one predicate
    # is randomly left out of the paired section entirely - see the module docstring above.
    remaining_names = [n for n in predicate_names if n not in pairs_used_predicates]
    random.shuffle(remaining_names)
    if len(remaining_names) % 2 == 1:
        remaining_names = remaining_names[:-1]
    fresh_pairs = [(remaining_names[i], remaining_names[i + 1]) for i in range(0, len(remaining_names), 2)]
    # EVEN and ODD (the "total pieces" variants) are always mutually exclusive with no
    # attribute to vary on either side, unlike e.g. EVEN_1/ODD_1 (which can still succeed by
    # drawing different attributes) - so if the matching happens to pair them, reshuffle rather
    # than burning retries on a combination that can never succeed.
    reshuffle_attempts = 0
    while any({a, b} == {"EVEN", "ODD"} for a, b in fresh_pairs) and reshuffle_attempts < 20:
        reshuffle_attempts += 1
        random.shuffle(remaining_names)
        fresh_pairs = [(remaining_names[i], remaining_names[i + 1]) for i in range(0, len(remaining_names), 2)]
    random.shuffle(fresh_pairs)

    num_and_remaining = num_and - sum(1 for e in paired_entries if e[2] == "and")
    num_or_remaining = num_or - sum(1 for e in paired_entries if e[2] == "or")
    combiners = ["and"] * num_and_remaining + ["or"] * num_or_remaining
    random.shuffle(combiners)

    for (predicate_a, predicate_b), combiner in zip(fresh_pairs, combiners):
        result = generate_paired_instance(rules, predicate_index[predicate_a], predicate_index[predicate_b],
                                          combiner, starting_template, prolog_file, timeout=timeout,
                                          attribute_pick_fn=attribute_pick_fn)
        if result is None:
            paired_entries.append((predicate_a, predicate_b, combiner, None, None, None))
        else:
            rule_text, query, n_query = result
            paired_entries.append((predicate_a, predicate_b, combiner, rule_text, query, n_query))

    return single_entries, paired_entries


def generate_rule(rules_json_file='rules/zendo_rules.json'):
    """
    Generates a random rule from a JSON rules file and converts it into a Prolog query.

    :param rules_json_file: The path to the JSON file containing placeholder rules.
    :return: A tuple containing the generated rule text, its Prolog query, and its negative Prolog query.
    """

    rules = load_json_rules(rules_json_file)
    starting_template = template_from_text(rules, "A structure must contain QUANTITY.")

    # Generate random rule and parse it into a prolog query
    rule = random_rule(rules, starting_template, two_random_steps=False, print_tree=False)
    query, n_query = rule_text_to_prolog(rules, rule, starting_template, debug_print_parse=False,
                                         debug_print_nodes=False)
    return rule, query, n_query


def generate_prolog_structure(num_examples, query, prolog_file='rules/rules.pl'):
    """
    Executes a Prolog query to generate structural examples.

    :param num_examples: The number of examples to generate.
    :param query: The Prolog query to execute.
    :param prolog_file: The path to the Prolog file containing the rules.
    :return: A list of generated structures.
    """

    prolog = Prolog()
    prolog.consult(prolog_file)

    # Execute the random queries
    seen_structures = set()
    results = []

    def structure_to_hashable(structure):
        """Convert a Prolog structure to a JSON-serializable hashable string."""
        try:
            return sha256(json.dumps(structure, sort_keys=True).encode()).hexdigest()
        except Exception:
            return str(structure)  # fallback

    attempts = 0
    max_attempts = num_examples * 20  # to avoid infinite loops

    while len(results) < num_examples and attempts < max_attempts:
        attempts += 1
        try:
            prolog_query = prolog.query(query)
            for i, szene in enumerate(prolog_query):
                structure = szene["Structure"]
                hash_key = structure_to_hashable(structure)

                if hash_key not in seen_structures:
                    seen_structures.add(hash_key)
                    results.append(structure)

        except Exception as e:
            print(f"Prolog query failed: {e}")
            continue

    if len(results) < num_examples:
        print(f"⚠️ Warning: Only {len(results)} unique structures could be generated after {attempts} attempts.")

    return results


if __name__ == "__main__":
    """
    Main script execution to generate a rule and corresponding Prolog structures.
    """

    for _ in range(100):
        rule, query, n_query = generate_rule()
        r1 = generate_prolog_structure(1, query)
        r2 = generate_prolog_structure(1, n_query)

        print(len(r1[0]), r1)

        # print(f"{rule}")
        # print(f"{query} \n {r1}")
        # print(f"{n_query} \n {r2}")
