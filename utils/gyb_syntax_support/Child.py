# flake8: noqa I201
from Token import SYNTAX_TOKEN_MAP
from kinds import SYNTAX_BASE_KINDS, kind_to_type, lowercase_first_word


class Child(object):
    """
    A child of a node, that may be declared optional or a token with a
    restricted subset of acceptable kinds or texts.
    """
    def __init__(self, name, kind, description=None, is_optional=False,
                 token_choices=None, text_choices=None, node_choices=None,
                 classification=None, force_classification=False):
        """
        If a classification is passed, it specifies the color identifiers in 
        that subtree should inherit for syntax coloring. Must be a member of 
        SyntaxClassification in SyntaxClassifier.h.gyb
        If force_classification is also set to true, all child nodes (not only
        identifiers) inherit the syntax classification.
        """
        self.name = name
        self.swift_name = lowercase_first_word(name)
        self.syntax_kind = kind
        self.description = description
        self.swift_syntax_kind = lowercase_first_word(self.syntax_kind)
        self.type_name = kind_to_type(self.syntax_kind)
        self.classification = classification
        self.force_classification = force_classification

        # If the child has "token" anywhere in the kind, it's considered
        # a token node. Grab the existing reference to that token from the
        # global list.
        self.token_kind = \
            self.syntax_kind if "Token" in self.syntax_kind else None
        self.token = SYNTAX_TOKEN_MAP.get(self.token_kind)

        self.is_optional = is_optional

        # A restricted set of token kinds that will be accepted for this
        # child.
        self.token_choices = []
        if self.token:
            self.token_choices.append(self.token)
        for choice in token_choices or []:
            token = SYNTAX_TOKEN_MAP[choice]
            self.token_choices.append(token)

        # A list of valid text for tokens, if specified.
        # This will force validation logic to check the text passed into the
        # token against the choices.
        self.text_choices = text_choices or []

        # A list of valid choices for a child
        self.node_choices = node_choices or []

        # Check the choices are either empty or multiple
        assert len(self.node_choices) != 1

        # Check node choices are well-formed
        for choice in self.node_choices:
            assert not choice.is_optional, \
                "node choice %s cannot be optional" % choice.name
            assert not choice.node_choices, \
                "node choice %s cannot have further choices" % choice.name

    def is_token(self):
        """
        Returns true if this child has a token kind.
        """
        return self.token_kind is not None

    def main_token(self):
        """
        Returns the first choice from the token_choices if there are any,
        otherwise returns None.
        """
        if self.token_choices:
            return self.token_choices[0]
        return None
