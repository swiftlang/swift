from Child import Child
from Node import Node  # noqa: I201

EXPR_NODES = [
    # An inout expression.
    # &x
    Node('InOutExpr', kind='Expr',
         children=[
             Child('Ampersand', kind='PrefixAmpersandToken'),
             Child('Expression', kind='Expr'),
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

    Node('StringInterpolationSegments', kind='SyntaxCollection',
         element='Syntax', element_name='Segment',
         element_choices=['StringSegment', 'ExpressionSegment']),

    # The try operator.
    # try foo()
    # try? foo()
    # try! foo()
    Node('TryExpr', kind='Expr',
         children=[
             Child('TryKeyword', kind='TryToken'),
             Child('QuestionOrExclamationMark', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'PostfixQuestionMarkToken',
                       'ExclamationMarkToken',
                   ]),
             Child('Expression', kind='Expr'),
         ]),

    # declname-arguments -> '(' declname-argument-list ')'
    # declname-argument-list -> declname-argument*
    # declname-argument -> identifier ':'
    Node('DeclNameArgument', kind='Syntax',
         children=[
             Child('Name', kind='Token'),
             Child('Colon', kind='ColonToken'),
         ]),
    Node('DeclNameArgumentList', kind='SyntaxCollection',
         element='DeclNameArgument'),
    Node('DeclNameArguments', kind='Syntax',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('Arguments', kind='DeclNameArgumentList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # An identifier expression.
    Node('IdentifierExpr', kind='Expr',
         children=[
             Child('Identifier', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'SelfToken',
                       'CapitalSelfToken',
                       'DollarIdentifierToken',
                       'SpacedBinaryOperatorToken',
                   ]),
             Child('DeclNameArguments', kind='DeclNameArguments',
                   is_optional=True),
         ]),

    # An 'super' expression.
    Node('SuperRefExpr', kind='Expr',
         children=[
             Child('SuperKeyword', kind='SuperToken'),
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

    Node('ExprList', kind='SyntaxCollection',
         element='Expr',
         element_name='Expression'),

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

    # A #dsohandle expression.
    Node('PoundDsohandleExpr', kind='Expr',
         children=[
             Child('PoundDsohandle', kind='PoundDsohandleToken'),
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
    # NOTE: This appears only in SequenceExpr.
    Node('BinaryOperatorExpr', kind='Expr',
         children=[
             Child('OperatorToken', kind='BinaryOperatorToken'),
         ]),

    # arrow-expr -> 'throws'? '->'
    # NOTE: This appears only in SequenceExpr.
    Node('ArrowExpr', kind='Expr',
         children=[
             Child('ThrowsToken', kind='ThrowsToken',
                   is_optional=True),
             Child('ArrowToken', kind='ArrowToken'),
         ]),

    # A floating-point literal
    # 4.0
    # -3.9
    # +4e20
    Node('FloatLiteralExpr', kind='Expr',
         children=[
             Child('FloatingDigits', kind='FloatingLiteralToken'),
         ]),

    Node('TupleExpr', kind='Expr',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('ElementList', kind='TupleElementList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # Array literal, e.g. [1, 2, 3]
    Node('ArrayExpr', kind='Expr',
         children=[
             Child('LeftSquare', kind='LeftSquareBracketToken'),
             Child('Elements', kind='ArrayElementList'),
             Child('RightSquare', kind='RightSquareBracketToken'),
         ]),

    # Dictionary literal, e.g. [1:1, 2:2, 3:3]
    Node('DictionaryExpr', kind='Expr',
         children=[
             Child('LeftSquare', kind='LeftSquareBracketToken'),
             Child('Content', kind='Syntax',
                   node_choices=[
                       Child('Colon', kind='ColonToken'),
                       Child('Elements', kind='DictionaryElementList'),
                   ]),
             Child('RightSquare', kind='RightSquareBracketToken'),
         ]),

    # .foo
    Node('ImplicitMemberExpr', kind='Expr',
         children=[
             Child("Dot", kind='PrefixPeriodToken'),
             Child("Name", kind='Token'),
             Child('DeclNameArguments', kind='DeclNameArguments',
                   is_optional=True),
         ]),

    # function-call-argument -> label? ':'? expression ','?
    Node('FunctionCallArgument', kind='Syntax',
         traits=['WithTrailingComma'],
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
         traits=['WithTrailingComma'],
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
         traits=['WithTrailingComma'],
         children=[
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # element inside an array expression: expression ','?
    Node('DictionaryElement', kind='Syntax',
         traits=['WithTrailingComma'],
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
             # The base needs to be optional to parse expressions in key paths
             # like \.a
             Child("Base", kind='Expr', is_optional=True),
             Child("Dot", kind='PeriodToken'),
             Child("Name", kind='Token'),
             Child('DeclNameArguments', kind='DeclNameArguments',
                   is_optional=True),
         ]),

    # dot-self-expr -> expr '.' 'self'
    Node('DotSelfExpr', kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('Dot', kind='Token',
                   token_choices=[
                       'PeriodToken', 'PrefixPeriodToken'
                   ]),
             Child('SelfKeyword', kind='SelfToken'),
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

    # Type
    Node('TypeExpr', kind='Expr',
         children=[
             Child('Type', kind='Type'),
         ]),

    Node('ClosureCaptureItem', kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
             Child("Specifier", kind='TokenList', is_optional=True),
             Child("Name", kind='IdentifierToken', is_optional=True),
             Child('AssignToken', kind='EqualToken', is_optional=True),
             Child("Expression", kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    Node('ClosureCaptureItemList', kind='SyntaxCollection',
         element='ClosureCaptureItem'),

    Node('ClosureCaptureSignature', kind='Syntax',
         children=[
             Child('LeftSquare', kind='LeftSquareBracketToken'),
             Child('Items', kind='ClosureCaptureItemList', is_optional=True),
             Child('RightSquare', kind='RightSquareBracketToken'),
         ]),

    Node('ClosureParam', kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
             Child('Name', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken',
                   ]),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # a, b, c
    Node('ClosureParamList', kind='SyntaxCollection', element='ClosureParam'),

    Node('ClosureSignature', kind='Syntax',
         children=[
             Child('Capture', kind='ClosureCaptureSignature',
                   is_optional=True),
             Child('Input', kind='Syntax', is_optional=True,
                   node_choices=[
                       Child('SimpleInput', kind='ClosureParamList'),
                       Child('Input', kind='ParameterClause'),
                   ]),
             Child('ThrowsTok', kind='ThrowsToken', is_optional=True),
             Child('Output', kind='ReturnClause', is_optional=True),
             Child('InTok', kind='InToken'),
         ]),

    Node('ClosureExpr', kind='Expr',
         traits=['Braced', 'WithStatements'],
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Signature', kind='ClosureSignature', is_optional=True),
             Child('Statements', kind='CodeBlockItemList'),
             Child('RightBrace', kind='RightBraceToken'),
         ]),

    # unresolved-pattern-expr -> pattern
    Node('UnresolvedPatternExpr', kind='Expr',
         children=[
             Child('Pattern', kind='Pattern'),
         ]),

    # call-expr -> expr '(' call-argument-list ')' closure-expr?
    #            | expr closure-expr
    Node('FunctionCallExpr', kind='Expr',
         children=[
             Child('CalledExpression', kind='Expr'),
             Child('LeftParen', kind='LeftParenToken',
                   is_optional=True),
             Child('ArgumentList', kind='FunctionCallArgumentList'),
             Child('RightParen', kind='RightParenToken',
                   is_optional=True),
             Child('TrailingClosure', kind='ClosureExpr',
                   is_optional=True),
         ]),

    # subscript-expr -> expr '[' call-argument-list ']' closure-expr?
    Node('SubscriptExpr', kind='Expr',
         children=[
             Child('CalledExpression', kind='Expr'),
             Child('LeftBracket', kind='LeftSquareBracketToken'),
             Child('ArgumentList', kind='FunctionCallArgumentList'),
             Child('RightBracket', kind='RightSquareBracketToken'),
             Child('TrailingClosure', kind='ClosureExpr',
                   is_optional=True),
         ]),

    # optional-chaining-expr -> expr '?'
    Node('OptionalChainingExpr', kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('QuestionMark', kind='PostfixQuestionMarkToken'),
         ]),

    # forced-value-expr -> expr '!'
    Node('ForcedValueExpr', kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('ExclamationMark', kind='ExclamationMarkToken'),
         ]),

    # postfix-unary-expr -> expr postfix-operator
    Node('PostfixUnaryExpr', kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('OperatorToken', kind='PostfixOperatorToken'),
         ]),

    # specialize-expr -> expr generic-argument-clause?
    Node('SpecializeExpr', kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('GenericArgumentClause', kind='GenericArgumentClause'),
         ]),

    # string literal segment in a string interpolation expression.
    Node('StringSegment', kind='Syntax',
         children=[
             Child('Content', kind='StringSegmentToken'),
         ]),

    # expression segment in a string interpolation expression.
    Node('ExpressionSegment', kind='Syntax',
         traits=['Parenthesized'],
         children=[
             Child('Backslash', kind='BackslashToken'),
             Child('LeftParen', kind='LeftParenToken',
                   classification='StringInterpolationAnchor',
                   force_classification=True),
             Child('Expression', kind='Expr'),
             Child('RightParen', kind='StringInterpolationAnchorToken'),
         ]),

    # e.g. "abc \(foo()) def"
    Node('StringInterpolationExpr', kind='Expr',
         children=[
             Child('OpenQuote', kind='Token',
                   token_choices=[
                       'StringQuoteToken',
                       'MultilineStringQuoteToken',
                   ]),
             Child('Segments', kind='StringInterpolationSegments'),
             Child('CloseQuote', kind='Token',
                   token_choices=[
                       'StringQuoteToken',
                       'MultilineStringQuoteToken',
                   ]),
         ]),

    # e.g. "\a.b[2].a"
    Node('KeyPathExpr', kind='Expr',
         children=[
             Child('Backslash', kind='BackslashToken'),
             Child('RootExpr', kind='Expr', is_optional=True,
                   node_choices=[
                       Child('IdentifierExpr', kind='IdentifierExpr'),
                       Child('SpecializeExpr', kind='SpecializeExpr')
                   ]),
             Child('Expression', kind='Expr'),
         ]),

    # The period in the key path serves as the base on which the
    # right-hand-side of the key path is evaluated
    Node('KeyPathBaseExpr', kind='Expr',
         children=[
             Child('Period', kind='PeriodToken'),
         ]),

    # e.g. "a." or "a"
    Node('ObjcNamePiece', kind='Syntax',
         children=[
             Child('Name', kind='IdentifierToken'),
             Child('Dot', kind='PeriodToken', is_optional=True),
         ]),

    # e.g. "a.b.c"
    Node('ObjcName', kind='SyntaxCollection', element='ObjcNamePiece'),

    # e.g. "#keyPath(a.b.c)"
    Node('ObjcKeyPathExpr', kind='Expr',
         traits=['Parenthesized'],
         children=[
             Child('KeyPath', kind='PoundKeyPathToken'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('Name', kind='ObjcName'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # e.g. "#selector(getter:Foo.bar)"
    Node('ObjcSelectorExpr', kind='Expr',
         traits=['Parenthesized'],
         children=[
             Child('PoundSelector', kind='PoundSelectorToken'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('Kind', kind='ContextualKeywordToken',
                   text_choices=['getter', 'setter'],
                   is_optional=True),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('Name', kind='Expr'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # <#content#>
    Node('EditorPlaceholderExpr', kind='Expr',
         children=[
             Child('Identifier', kind='IdentifierToken'),
         ]),
    # #fileLiteral(a, b, c)
    Node('ObjectLiteralExpr', kind='Expr',
         traits=['Parenthesized'],
         children=[
             Child('Identifier', kind='Token',
                   token_choices=[
                       'PoundColorLiteralToken',
                       'PoundFileLiteralToken',
                       'PoundImageLiteralToken',
                   ]),
             Child('LeftParen', kind='LeftParenToken'),
             Child('Arguments', kind='FunctionCallArgumentList'),
             Child('RightParen', kind='RightParenToken'),
         ]),
]
