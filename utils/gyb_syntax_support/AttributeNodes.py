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
             Child('AttributeName', kind='Token'),
             # FIXME: more structure
             Child('BalancedTokens', kind='TokenList'),
         ]),

    # attribute-list -> attribute attribute-list?
    Node('AttributeList', kind='SyntaxCollection',
         element='Attribute'),
]
