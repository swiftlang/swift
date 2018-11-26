from Child import Child
from Node import Node  # noqa: I201

TYPE_NODES = [
    # simple-type-identifier -> identifier generic-argument-clause?
    Node('SimpleTypeIdentifier', kind='Type',
         children=[
             Child('Name', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'CapitalSelfToken',
                       'AnyToken',
                   ]),
             Child('GenericArgumentClause', kind='GenericArgumentClause',
                   is_optional=True),
         ]),

    # member-type-identifier -> type '.' identifier generic-argument-clause?
    Node('MemberTypeIdentifier', kind='Type',
         children=[
             Child('BaseType', kind='Type'),
             Child('Period', kind='Token',
                   token_choices=[
                       'PeriodToken',
                       'PrefixPeriodToken',
                   ]),
             Child('Name', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'CapitalSelfToken',
                       'AnyToken',
                   ]),
             Child('GenericArgumentClause', kind='GenericArgumentClause',
                   is_optional=True),
         ]),

    # class-restriction-type -> 'class'
    Node('ClassRestrictionType', kind='Type',
         children=[
             Child('ClassKeyword', kind='ClassToken'),
         ]),
    # array-type -> '[' type ']'
    Node('ArrayType', kind='Type',
         children=[
             Child('LeftSquareBracket', kind='LeftSquareBracketToken'),
             Child('ElementType', kind='Type'),
             Child('RightSquareBracket', kind='RightSquareBracketToken'),
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

    # optional-type -> type '?'
    Node('OptionalType', kind='Type',
         children=[
             Child('WrappedType', kind='Type'),
             Child('QuestionMark', kind='PostfixQuestionMarkToken'),
         ]),

    # implicitly-unwrapped-optional-type -> type '!'
    Node('ImplicitlyUnwrappedOptionalType', kind='Type',
         children=[
             Child('WrappedType', kind='Type'),
             Child('ExclamationMark', kind='ExclamationMarkToken'),
         ]),

    # composition-type-element -> type '&'
    Node('CompositionTypeElement', kind='Syntax',
         children=[
             Child('Type', kind='Type'),
             Child('Ampersand', kind='Token',
                   text_choices=['&'],
                   is_optional=True),
         ]),

    # composition-typeelement-list -> composition-type-element
    #   composition-type-element-list?
    Node('CompositionTypeElementList', kind='SyntaxCollection',
         element='CompositionTypeElement'),

    # composition-type -> composition-type-element-list
    Node('CompositionType', kind='Type',
         children=[
             Child('Elements', kind='CompositionTypeElementList'),
         ]),

    # tuple-type-element -> identifier? ':'? type-annotation ','?
    Node('TupleTypeElement', kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
             Child('InOut', kind='InOutToken',
                   is_optional=True),
             Child('Name', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken'
                   ]),
             Child('SecondName', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken'
                   ]),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('Type', kind='Type'),
             Child('Ellipsis', kind='Token',
                   is_optional=True),
             Child('Initializer', kind='InitializerClause',
                   is_optional=True),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # tuple-type-element-list -> tuple-type-element tuple-type-element-list?
    Node('TupleTypeElementList', kind='SyntaxCollection',
         element='TupleTypeElement'),

    # tuple-type -> '(' tuple-type-element-list ')'
    Node('TupleType', kind='Type',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('Elements', kind='TupleTypeElementList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # throwing-specifier -> 'throws' | 'rethrows'
    # function-type -> attribute-list '(' function-type-argument-list ')'
    #   throwing-specifier? '->'? type?
    Node('FunctionType', kind='Type',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('Arguments', kind='TupleTypeElementList'),
             Child('RightParen', kind='RightParenToken'),
             Child('ThrowsOrRethrowsKeyword', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'ThrowsToken',
                       'RethrowsToken',
                       'ThrowToken',
                   ]),
             Child('Arrow', kind='ArrowToken'),
             Child('ReturnType', kind='Type'),
         ]),

    # attributed-type -> type-specifier? attribute-list? type
    # type-specifiyer -> 'inout' | '__owned' | '__unowned'
    Node('AttributedType', kind='Type',
         children=[
             Child('Specifier', kind='Token',
                   text_choices=['inout', '__shared', '__owned'],
                   is_optional=True),
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('BaseType', kind='Type'),
         ]),

    # generic-argument-list -> generic-argument generic-argument-list?
    Node('GenericArgumentList', kind='SyntaxCollection',
         element='GenericArgument'),

    # A generic argument.
    # Dictionary<Int, String>
    #            ^~~~ ^~~~~~
    Node('GenericArgument', kind='Syntax',
         traits=['WithTrailingComma'],
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
]
