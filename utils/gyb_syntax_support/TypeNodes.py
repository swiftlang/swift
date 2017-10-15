from Child import Child
from Node import Node  # noqa: I201

TYPE_NODES = [
    # metatype-type -> type '.' 'Type'
    #                | type '.' 'Protocol
    Node('MetatypeType', kind='Type',
         children=[
             Child('BaseType', kind='Type'),
             Child('Period', kind='PeriodToken'),
             Child('TypeOrProtocol', kind='IdentifierToken',
                   text_choices=[
                       'Type',
                       'Protocol',
                   ]),
         ]),

    # dictionary-type -> '[' type ':' type ']'
    Node('DictionaryType', kind='Type',
         children=[
             Child('LeftSquareBracket', kind='LeftSquareBracketToken'),
             Child('KeyType', kind='Type'),
             Child('Colon', kind='ColonToken'),
             Child('ValueType', kind='Type'),
             Child('RightSquareBracket', kind='RightSquareBracketToken'),
         ]),

    # throwing-specifier -> 'throws' | 'rethrows'
    # function-type -> attribute-list '(' function-type-argument-list ')'
    #   throwing-specifier? '->'? type?
    Node('FunctionType', kind='Type',
         children=[
             Child('TypeAttributes', kind='AttributeList'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('ArgumentList', kind='FunctionTypeArgumentList'),
             Child('RightParen', kind='RightParenToken'),
             Child('ThrowsOrRethrowsKeyword', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'ThrowsToken',
                       'RethrowsToken',
                   ]),
             Child('Arrow', kind='ArrowToken',
                   is_optional=True),
             Child('ReturnType', kind='Type',
                   is_optional=True),
         ]),

    # tuple-type -> '(' tuple-type-element-list ')'
    Node('TupleType', kind='Type',
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('Elements', kind='TupleTypeElementList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # tuple-type-element -> identifier? ':'? type-annotation ','?
    Node('TupleTypeElement', kind='Syntax',
         children=[
             Child('Label', kind='IdentifierToken',
                   is_optional=True),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('TypeAnnotation', kind='TypeAnnotation'),
             Child('Comma', kind='CommaToken',
                   is_optional=True),
         ]),

    # array-type -> '[' type ']'
    Node('ArrayType', kind='Type',
         children=[
             Child('LeftSquareBracket', kind='LeftSquareBracketToken'),
             Child('ElementType', kind='Type'),
             Child('RightSquareBracket', kind='RightSquareBracketToken'),
         ]),

    # type-annotation -> attribute-list 'inout'? type
    Node('TypeAnnotation', kind='Syntax',
         children=[
             Child('Attributes', kind='AttributeList'),
             Child('InOutKeyword', kind='InoutToken',
                   is_optional=True),
             Child('Type', kind='Type'),
         ]),

    # protocol-composition-element-list -> protocol-composition-element
    #   protocol-composition-element-list?
    Node('ProtocolCompositionElementList', kind='SyntaxCollection',
         element='ProtocolCompositionElement'),

    # tuple-type-element-list -> tuple-type-element tuple-type-element-list?
    Node('TupleTypeElementList', kind='SyntaxCollection',
         element='TupleTypeElement'),

    # implicitly-unwrapped-optional-type -> type '!'
    Node('ImplicitlyUnwrappedOptionalType', kind='Type',
         children=[
             Child('ValueType', kind='Type'),
             Child('ExclamationMark', kind='ExclamationMarkToken'),
         ]),

    # protocol-composition-element -> type-identifier '&'
    Node('ProtocolCompositionElement', kind='Syntax',
         children=[
             Child('ProtocolType', kind='TypeIdentifier'),
             Child('Ampersand', kind='AmpersandToken',
                   is_optional=True),
         ]),

    # generic-argument-list -> generic-argument generic-argument-list?
    Node('GenericArgumentList', kind='SyntaxCollection',
         element='GenericArgument'),

    # A generic argument.
    # Dictionary<Int, String>
    #            ^~~~ ^~~~~~
    Node('GenericArgument', kind='Syntax',
         children=[
             Child('ArgumentType', kind='Type'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # generic-argument-clause -> '<' generic-argument-list '>'
    Node('GenericArgumentClause', kind='Syntax',
         children=[
             Child('LeftAngleBracket', kind='LeftAngleToken'),
             Child('Arguments', kind='GenericArgumentList'),
             Child('RightAngleBracket', kind='RightAngleToken'),
         ]),

    # function-type-argument -> identifier? identifier? ':'
    #   type-annotation ','?
    Node('FunctionTypeArgument', kind='Syntax',
         children=[
             Child('ExternalName', kind='IdentifierToken',
                   is_optional=True),
             Child('LocalName', kind='IdentifierToken',
                   is_optional=True),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('TypeAnnotation', kind='TypeAnnotation'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # optional-type -> type '?'
    Node('OptionalType', kind='Type',
         children=[
             Child('ValueType', kind='Type'),
             Child('QuestionMark', kind='PostfixQuestionMarkToken'),
         ]),

    # type-identifier -> identifier generic-argument-clause? '.'?
    #   type-identifier?
    Node('TypeIdentifier', kind='Type',
         children=[
             Child('TypeName', kind='IdentifierToken'),
             Child('GenericArgumentClause', kind='GenericArgumentClause',
                   is_optional=True),
             Child('Period', kind='PeriodToken',
                   is_optional=True),
             Child('TypeIdentifier', kind='TypeIdentifier',
                   is_optional=True),
         ]),

    # function-type-argument-list -> function-type-argument
    #   function-type-argument-list?
    Node('FunctionTypeArgumentList', kind='SyntaxCollection',
         element='FunctionTypeArgument'),

    # protocol-composition-type -> protocol-composition-elements
    Node('ProtocolCompositionType', kind='Type',
         children=[
             Child('Elements', kind='ProtocolCompositionElementList'),
         ]),
]
