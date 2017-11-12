from Child import Child
from Node import Node  # noqa: I201

ATTRIBUTE_NODES = [
    # token-list -> token token-list?
    Node('TokenList', kind='SyntaxCollection',
         element='Token'),

    # attribute -> '@' identifier '('? token-list ')'?
    Node('Attribute', kind='Syntax',
         children=[
             Child('AtSignToken', kind='AtSignToken'),
             Child('Identifier', kind='IdentifierToken'),
             Child('LeftParen', kind='LeftParenToken',
                   is_optional=True),
             Child('BalancedTokens', kind='TokenList'),
             Child('RightParen', kind='RightParenToken',
                   is_optional=True),
         ]),

    # attribute-list -> attribute attribute-list?
    Node('AttributeList', kind='SyntaxCollection',
         element='Attribute'),
]
