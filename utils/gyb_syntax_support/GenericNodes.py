from Child import Child
from Node import Node  # noqa: I201

GENERIC_NODES = [
    # generic-where-clause -> 'where' requirement-list
    Node('GenericWhereClause', kind='Syntax',
         children=[
             Child('WhereKeyword', kind='WhereToken'),
             Child('RequirementList', kind='GenericRequirementList'),
         ]),

    Node('GenericRequirementList', kind='SyntaxCollection',
         element='Syntax',
         element_name='GenericRequirement'),

    # same-type-requirement -> type-identifier == type
    Node('SameTypeRequirement', kind='Syntax',
         children=[
             Child('LeftTypeIdentifier', kind='TypeIdentifier'),
             Child('EqualityToken', kind='Token',
                   token_choices=[
                       'SpacedBinaryOperatorToken',
                       'UnspacedBinaryOperatorToken',
                   ]),
             Child('RightTypeIdentifier', kind='TypeIdentifier'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    Node('GenericParameterList', kind='SyntaxCollection',
         element='GenericParameter'),

    # generic-parameter -> type-name
    #                    | type-name : type-identifier
    #                    | type-name : protocol-composition-type
    Node('GenericParameter', kind='Syntax',
         children=[
             Child('TypeIdentifier', kind='TypeIdentifier'),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('InheritedType', kind='Type',
                   is_optional=True),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # generic-parameter-clause -> '<' generic-parameter-list '>'
    Node('GenericParameterClause', kind='Syntax',
         children=[
             Child('LeftAngleBracket', kind='LeftAngleToken'),
             Child('GenericParameterList', kind='GenericParameterList'),
             Child('RightAngleBracket', kind='RightAngleToken'),
         ]),

    # conformance-requirement -> type-identifier : type-identifier
    Node('ConformanceRequirement', kind='Syntax',
         children=[
             Child('LeftTypeIdentifier', kind='TypeIdentifier'),
             Child('Colon', kind='ColonToken'),
             Child('RightTypeIdentifier', kind='TypeIdentifier'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),
]
