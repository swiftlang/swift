# flake8: noqa I201
from Token import SYNTAX_TOKEN_MAP
from kinds import kind_to_type


class Child(object):
    """
    A child of a node, that may be declared optional or a token with a
    restricted subset of acceptable kinds or texts.
    """
    def __init__(self, name, kind, is_optional=False,
                 token_choices=None, text_choices=None):
        self.name = name
        self.syntax_kind = kind
        self.type_name = kind_to_type(self.syntax_kind)

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
