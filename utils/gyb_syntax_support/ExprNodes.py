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
             Child('Operator', kind='PrefixOperatorToken',
                   is_optional=True),
             Child('PostfixExpression', kind='Expr'),
         ]),

    # A floating-point literal
    # 4.0
    # -3.9
    # +4e20
    Node('FloatLiteralExpr', kind='Expr',
         children=[
             Child('Sign', kind='PrefixOperatorToken',
                   is_optional=True),
             Child('FloatingDigits', kind='FloatingLiteralToken'),
         ]),

    Node('FunctionCallExpr', kind='Expr',
         children=[
             Child('CalledExpression', kind='Expr'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('ArgumentList', kind='FunctionCallArgumentList'),
             Child('RightParen', kind='RightParenToken'),
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

    # An integer literal.
    # 3
    # +3_400
    # +0x4f
    Node('IntegerLiteralExpr', kind='Expr',
         children=[
             Child('Sign', kind='PrefixOperatorToken',
                   is_optional=True),
             Child('Digits', kind='IntegerLiteralToken'),
         ]),
]
