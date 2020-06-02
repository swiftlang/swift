from .Child import Child
from .Node import Node  # noqa: I201

PATTERN_NODES = [

    # type-annotation -> ':' type
    Node('TypeAnnotation', kind='Syntax',
         children=[
             Child('Colon', kind='ColonToken'),
             Child('Type', kind='Type'),
         ]),

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

    # optional-pattern -> pattern '?'
    Node('OptionalPattern', kind='Pattern',
         children=[
             Child('SubPattern', kind='Pattern'),
             Child('QuestionMark', kind='PostfixQuestionMarkToken'),
         ]),

    # identifier-pattern -> identifier
    Node('IdentifierPattern', kind='Pattern',
         children=[
             Child('Identifier', kind='Token',
                   token_choices=[
                       'SelfToken',
                       'IdentifierToken',
                   ]),
         ]),

    # as-pattern -> pattern 'as' type
    Node('AsTypePattern', kind='Pattern',
         children=[
             Child('Pattern', kind='Pattern'),
             Child('AsKeyword', kind='AsToken'),
             Child('Type', kind='Type'),
         ]),

    # tuple-pattern -> '(' tuple-pattern-element-list ')'
    Node('TuplePattern', kind='Pattern',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('Elements', kind='TuplePatternElementList',
                   collection_element_name='Element'),
             Child('RightParen', kind='RightParenToken'),
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
         traits=['WithTrailingComma', 'Labeled'],
         children=[
             Child('LabelName', kind='IdentifierToken',
                   is_optional=True),
             Child('LabelColon', kind='ColonToken',
                   is_optional=True),
             Child('Pattern', kind='Pattern'),
             Child('TrailingComma', kind='CommaToken',
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
