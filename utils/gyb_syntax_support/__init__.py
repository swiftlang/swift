from AttributeNodes import ATTRIBUTE_NODES
from CommonNodes import COMMON_NODES  # noqa: I201
from DeclNodes import DECL_NODES  # noqa: I201
from ExprNodes import EXPR_NODES  # noqa: I201
from GenericNodes import GENERIC_NODES  # noqa: I201
from PatternNodes import PATTERN_NODES  # noqa: I201
from StmtNodes import STMT_NODES  # noqa: I201
import Token
from TypeNodes import TYPE_NODES  # noqa: I201


# Re-export global constants
SYNTAX_NODES = COMMON_NODES + EXPR_NODES + DECL_NODES + ATTRIBUTE_NODES + \
    STMT_NODES + GENERIC_NODES + TYPE_NODES + PATTERN_NODES
SYNTAX_TOKENS = Token.SYNTAX_TOKENS
SYNTAX_TOKEN_MAP = Token.SYNTAX_TOKEN_MAP


def make_missing_child(child):
    """
    Generates a C++ call to make the raw syntax for a given Child object.
    """
    if child.is_token():
        token = child.main_token()
        tok_kind = "tok::" + token.kind if token else "tok::unknown"
        tok_text = token.text if token else ""
        return 'RawSyntax::missing(%s, "%s")' % (tok_kind, tok_text)
    else:
        missing_kind = "Unknown" if child.syntax_kind == "Syntax" \
                       else child.syntax_kind
        if child.node_choices:
            return make_missing_child(child.node_choices[0])
        return 'RawSyntax::missing(SyntaxKind::%s)' % missing_kind


def check_child_condition(child):
    """
    Generates a C++ closure to check whether a given syntax node S can satisfy
    the requirements of child.
    """
    result = '[](const Syntax &S) {\n'
    result += '  // check %s\n' % child.name
    if child.token_choices:
        # Check token kind choices are met.
        result += 'if (auto Tok = S.getAs<TokenSyntax>()) {\n'
        result += '  auto Kind = Tok->getTokenKind();\n'
        tok_checks = []
        for choice in child.token_choices:
            tok_checks.append("Kind == tok::%s" % choice.kind)
        all_checks = ' || '.join(tok_checks)
        result += '  return %s;\n' % all_checks
        result += '}\n'
        result += 'return false;\n'
    elif child.text_choices:
        # Check token text choices are met.
        result += 'if (auto Tok = S.getAs<TokenSyntax>()) {\n'
        result += '  auto Text = Tok->getText();\n'
        tok_checks = []
        for choice in child.text_choices:
            tok_checks.append("Text == \"%s\"" % choice)
        all_checks = ' || '.join(tok_checks)
        result += '  return %s;\n' % all_checks
        result += '}\n'
        result += 'return false;\n'
    elif child.node_choices:
        # Recursively, check one of the node choices' conditions are met.
        node_checks = []
        for choice in child.node_choices:
            node_checks.append(check_child_condition(choice) + '(S)')
        all_checks = ' || '.join(node_checks)
        result += 'return %s;\n' % all_checks
    else:
        # For remaining children, simply check the syntax kind.
        result += 'return S.getAs<%s>().hasValue();\n' % child.type_name
    result += '}'
    return result


def make_missing_swift_child(child):
    """
    Generates a Swift call to make the raw syntax for a given Child object.
    """
    if child.is_token():
        token = child.main_token()
        tok_kind = token.swift_kind() if token else "unknown"
        if token and not token.text:
            tok_kind += '("")'
        return 'RawSyntax.missingToken(.%s)' % tok_kind
    else:
        missing_kind = "unknown" if child.syntax_kind == "Syntax" \
                       else child.swift_syntax_kind
        return 'RawSyntax.missing(.%s)' % missing_kind


def create_node_map():
    """
    Creates a lookup table to find nodes by their kind.
    """
    return {node.syntax_kind: node for node in SYNTAX_NODES}


def is_visitable(node):
    return not node.is_base() and not node.collection_element
