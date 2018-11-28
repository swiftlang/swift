import textwrap
from AttributeNodes import ATTRIBUTE_NODES
from AvailabilityNodes import AVAILABILITY_NODES
import Classification
from CommonNodes import COMMON_NODES  # noqa: I201
from DeclNodes import DECL_NODES  # noqa: I201
from ExprNodes import EXPR_NODES  # noqa: I201
from GenericNodes import GENERIC_NODES  # noqa: I201
from NodeSerializationCodes import SYNTAX_NODE_SERIALIZATION_CODES, \
    verify_syntax_node_serialization_codes
from PatternNodes import PATTERN_NODES  # noqa: I201
from StmtNodes import STMT_NODES  # noqa: I201
import Token
from TypeNodes import TYPE_NODES  # noqa: I201


# Re-export global constants
SYNTAX_NODES = COMMON_NODES + EXPR_NODES + DECL_NODES + ATTRIBUTE_NODES + \
    STMT_NODES + GENERIC_NODES + TYPE_NODES + PATTERN_NODES + \
    AVAILABILITY_NODES
SYNTAX_TOKENS = Token.SYNTAX_TOKENS
SYNTAX_TOKEN_MAP = Token.SYNTAX_TOKEN_MAP
SYNTAX_CLASSIFICATIONS = Classification.SYNTAX_CLASSIFICATIONS

verify_syntax_node_serialization_codes(SYNTAX_NODES,
                                       SYNTAX_NODE_SERIALIZATION_CODES)


def make_missing_child(child):
    """
    Generates a C++ call to make the raw syntax for a given Child object.
    """
    if child.is_token():
        token = child.main_token()
        tok_kind = token.kind if token else "unknown"
        tok_text = token.text if token else ""
        return \
            'RawSyntax::missing(tok::%s, OwnedString::makeUnowned("%s"))' % \
            (tok_kind, tok_text)
    else:
        missing_kind = "Unknown" if child.syntax_kind == "Syntax" \
                       else child.syntax_kind
        if child.node_choices:
            return make_missing_child(child.node_choices[0])
        return 'RawSyntax::missing(SyntaxKind::%s)' % missing_kind


def check_child_condition_raw(child):
    """
    Generates a C++ closure to check whether a given raw syntax node can
    satisfy the requirements of child.
    """
    result = '[](const RC<RawSyntax> &Raw) {\n'
    result += ' // check %s\n' % child.name
    if child.token_choices:
        result += 'if (!Raw->isToken()) return false;\n'
        result += 'auto TokKind = Raw->getTokenKind();\n'
        tok_checks = []
        for choice in child.token_choices:
            tok_checks.append("TokKind == tok::%s" % choice.kind)
        result += 'return %s;\n' % (' || '.join(tok_checks))
    elif child.text_choices:
        result += 'if (!Raw->isToken()) return false;\n'
        result += 'auto Text = Raw->getTokenText();\n'
        tok_checks = []
        for choice in child.text_choices:
            tok_checks.append('Text == "%s"' % choice)
        result += 'return %s;\n' % (' || '.join(tok_checks))
    elif child.node_choices:
        node_checks = []
        for choice in child.node_choices:
            node_checks.append(check_child_condition_raw(choice) + '(Raw)')
        result += 'return %s;\n' % ((' || ').join(node_checks))
    else:
        result += 'return %s::kindof(Raw->getKind());' % child.type_name
    result += '}'
    return result


def make_missing_swift_child(child):
    """
    Generates a Swift call to make the raw syntax for a given Child object.
    """
    if child.is_token():
        token = child.main_token()
        tok_kind = token.swift_kind() if token else "unknown"
        if not token or not token.text:
            tok_kind += '("")'
        return 'RawSyntax.missingToken(TokenKind.%s)' % tok_kind
    else:
        missing_kind = "unknown" if child.syntax_kind == "Syntax" \
                       else child.swift_syntax_kind
        return 'RawSyntax.missing(SyntaxKind.%s)' % missing_kind


def create_node_map():
    """
    Creates a lookup table to find nodes by their kind.
    """
    return {node.syntax_kind: node for node in SYNTAX_NODES}


def is_visitable(node):
    return not node.is_base()


def dedented_lines(description):
    """
    Each line of the provided string with leading whitespace stripped.
    """
    if not description:
        return []
    return textwrap.dedent(description).split('\n')
