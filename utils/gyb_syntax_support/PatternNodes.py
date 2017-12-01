from Child import Child
from Node import Node  # noqa: I201

PATTERN_NODES = [
    # enum-case-pattern -> type-identifier? '.' identifier tuple-pattern?
    Node('EnumCasePattern', kind='Pattern',
         children=[
             Child('Type', kind='Type',
                   is_optional=True),
             Child('Period', kind='PeriodToken'),
             Child('CaseName', kind='IdentifierToken'),
             Child('AssociatedTuple', kind='TuplePattern',
                   is_optional=True),
         ]),

    # is-type-pattern -> 'is' type
    Node('IsTypePattern', kind='Pattern',
         children=[
             Child('IsKeyword', kind='IsToken'),
             Child('Type', kind='Type'),
         ]),

    # optional-pattern -> identifier '?'
    Node('OptionalPattern', kind='Pattern',
         children=[
             Child('Identifier', kind='IdentifierToken'),
             Child('QuestionMark', kind='PostfixQuestionMarkToken'),
         ]),

    # identifier-pattern -> identifier type-annotation?
    Node('IdentifierPattern', kind='Pattern',
         children=[
             Child('Identifier', kind='IdentifierToken'),
             Child('TypeAnnotation', kind='TypeAnnotation',
                   is_optional=True),
         ]),

    # as-pattern -> pattern 'as' type
    Node('AsTypePattern', kind='Pattern',
         children=[
             Child('Pattern', kind='Pattern'),
             Child('AsKeyword', kind='AsToken'),
             Child('Type', kind='Type'),
         ]),

    # tuple-pattern -> '(' tuple-pattern-element-list ')' type-annotation?
    Node('TuplePattern', kind='Pattern',
         children=[
             Child('OpenParen', kind='LeftParenToken'),
             Child('Elements', kind='TuplePatternElementList'),
             Child('CloseParen', kind='RightParenToken'),
             Child('TypeAnnotation', kind='TypeAnnotation',
                   is_optional=True),
         ]),

    # wildcard-pattern -> '_' type-annotation?
    Node('WildcardPattern', kind='Pattern',
         children=[
             Child('Wildcard', kind='WildcardToken'),
             Child('TypeAnnotation', kind='TypeAnnotation',
                   is_optional=True),
         ]),

    # tuple-pattern-element -> identifier? ':' pattern ','?
    Node('TuplePatternElement', kind='Syntax',
         children=[
             Child('LabelName', kind='IdentifierToken',
                   is_optional=True),
             Child('LabelColon', kind='ColonToken',
                   is_optional=True),
             Child('Pattern', kind='Pattern'),
             Child('Comma', kind='CommaToken',
                   is_optional=True),
         ]),

    # expr-pattern -> expr
    Node('ExpressionPattern', kind='Pattern',
         children=[
             Child('Expression', kind='Expr'),
         ]),

    # tuple-pattern-element-list -> tuple-pattern-element
    #  tuple-pattern-element-list?
    Node('TuplePatternElementList', kind='SyntaxCollection',
         element='TuplePatternElement'),

    # value-binding-pattern -> 'let' pattern
    #                        | 'var' pattern
    Node('ValueBindingPattern', kind='Pattern',
         children=[
             Child('LetOrVarKeyword', kind='Token',
                   token_choices=[
                       'LetToken',
                       'VarToken',
                   ]),
             Child('ValuePattern', kind='Pattern'),
         ]),
]
