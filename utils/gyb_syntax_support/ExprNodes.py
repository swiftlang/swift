from Child import Child
from Node import Node  # noqa: I201

EXPR_NODES = [
    # An inout expression.
    # &x
    Node('InOutExpr', kind='Expr',
         children=[
             Child('Ampersand', kind='AmpersandToken'),
             Child('Identifier', kind='IdentifierToken'),
         ]),

    # A #column expression.
    Node('PoundColumnExpr', kind='Expr',
         children=[
             Child('PoundColumn', kind='PoundColumnToken'),
         ]),

    Node('FunctionCallArgumentList', kind='SyntaxCollection',
         element='FunctionCallArgument'),

    Node('TupleElementList', kind='SyntaxCollection',
         element='TupleElement'),

    Node('ArrayElementList', kind='SyntaxCollection',
         element='ArrayElement'),

    Node('DictionaryElementList', kind='SyntaxCollection',
         element='DictionaryElement'),

    # The try operator.
    # try foo()
    # try? foo()
    # try! foo()
    Node('TryOperator', kind='Syntax',
         children=[
             Child('TryKeyword', kind='TryToken'),
             Child('QuestionOrExclamationMark', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'PostfixQuestionMarkToken',
                       'ExclamationMarkToken',
                   ]),
         ]),

    # An identifier expression.
    Node('IdentifierExpr', kind='Expr',
         children=[
             Child('Identifier', kind='IdentifierToken'),
         ]),

    # A nil expression.
    Node('NilLiteralExpr', kind='Expr',
         children=[
             Child('NilKeyword', kind='NilToken'),
         ]),

    # A _ expression.
    Node('DiscardAssignmentExpr', kind='Expr',
         children=[
             Child('Wildcard', kind='WildcardToken'),
         ]),

    # An = expression.
    Node('AssignmentExpr', kind='Expr',
         children=[
             Child('AssignToken', kind='EqualToken'),
         ]),

    # A flat list of expressions before sequence folding, e.g. 1 + 2 + 3.
    Node('SequenceExpr', kind='Expr',
         children=[
             Child('Elements', kind='ExprList'),
         ]),

    # A #line expression.
    Node('PoundLineExpr', kind='Expr',
         children=[
             Child('PoundLine', kind='PoundLineToken'),
         ]),

    # A #file expression.
    Node('PoundFileExpr', kind='Expr',
         children=[
             Child('PoundFile', kind='PoundFileToken'),
         ]),

    # A #function expression.
    Node('PoundFunctionExpr', kind='Expr',
         children=[
             Child('PoundFunction', kind='PoundFunctionToken'),
         ]),

    # symbolic-reference-expression -> identifier generic-argument-clause?
    Node('SymbolicReferenceExpr', kind='Expr',
         children=[
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericArgumentClause', kind='GenericArgumentClause',
                   is_optional=True),
         ]),

    # A prefix operator expression.
    # -x
    # !true
    Node('PrefixOperatorExpr', kind='Expr',
         children=[
             Child('OperatorToken', kind='PrefixOperatorToken',
                   is_optional=True),
             Child('PostfixExpression', kind='Expr'),
         ]),

    # An operator like + or -.
    Node('BinaryOperatorExpr', kind='Expr',
         children=[
             Child('OperatorToken', kind='BinaryOperatorToken'),
         ]),

    # A floating-point literal
    # 4.0
    # -3.9
    # +4e20
    Node('FloatLiteralExpr', kind='Expr',
         children=[
             Child('FloatingDigits', kind='FloatingLiteralToken'),
         ]),

    Node('FunctionCallExpr', kind='Expr',
         children=[
             Child('CalledExpression', kind='Expr'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('ArgumentList', kind='FunctionCallArgumentList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    Node('TupleExpr', kind='Expr',
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('ElementList', kind='TupleElementList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # Array literal, e.g. [1, 2, 3]
    Node('ArrayExpr', kind='Expr',
         children=[
             Child('LeftSquare', kind='LeftSquareToken'),
             Child('Elements', kind='ArrayElementList'),
             Child('RightSquare', kind='RightSquareToken'),
         ]),

    # Dictionary literal, e.g. [1:1, 2:2, 3:3]
    Node('DictionaryExpr', kind='Expr',
         children=[
             Child('LeftSquare', kind='LeftSquareToken'),
             Child('Elements', kind='DictionaryElementList'),
             Child('RightSquare', kind='RightSquareToken'),
         ]),

    # function-call-argument -> label? ':'? expression ','?
    Node('FunctionCallArgument', kind='Syntax',
         children=[
             Child('Label', kind='IdentifierToken',
                   is_optional=True),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # An element inside a tuple element list
    Node('TupleElement', kind='Syntax',
         children=[
             Child('Label', kind='IdentifierToken',
                   is_optional=True),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # element inside an array expression: expression ','?
    Node('ArrayElement', kind='Syntax',
         children=[
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # element inside an array expression: expression ','?
    Node('DictionaryElement', kind='Syntax',
         children=[
             Child('KeyExpression', kind='Expr'),
             Child('Colon', kind='ColonToken'),
             Child('ValueExpression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # An integer literal.
    # 3
    # +3_400
    # +0x4f
    Node('IntegerLiteralExpr', kind='Expr',
         children=[
             Child('Digits', kind='IntegerLiteralToken'),
         ]),

    Node('StringLiteralExpr', kind='Expr',
         children=[
             Child("StringLiteral", kind='StringLiteralToken')
         ]),

    # true or false
    Node('BooleanLiteralExpr', kind='Expr',
         children=[
             Child("BooleanLiteral", kind='Token',
                   token_choices=[
                       'TrueToken',
                       'FalseToken',
                   ])
         ]),

    # a ? 1 : 0
    Node('TernaryExpr', kind='Expr',
         children=[
             Child("ConditionExpression", kind='Expr'),
             Child("QuestionMark", kind='InfixQuestionMarkToken'),
             Child("FirstChoice", kind='Expr'),
             Child("ColonMark", kind='ColonToken'),
             Child("SecondChoice", kind='Expr')
         ]),

    # a.b
    Node('MemberAccessExpr', kind='Expr',
         children=[
             Child("Base", kind='Expr'),
             Child("Dot", kind='PeriodToken'),
             Child("Name", kind='Token')
         ]),

    # is TypeName
    Node('IsExpr', kind='Expr',
         children=[
             Child("IsTok", kind='IsToken'),
             Child("TypeName", kind='Type')
         ]),

    # as TypeName
    Node('AsExpr', kind='Expr',
         children=[
             Child("AsTok", kind='AsToken'),
             Child("QuestionOrExclamationMark", kind='Token',
                   is_optional=True,
                   token_choices=[
                       'PostfixQuestionMarkToken',
                       'ExclamationMarkToken',
                   ]),
             Child("TypeName", kind='Type')
         ]),
]
