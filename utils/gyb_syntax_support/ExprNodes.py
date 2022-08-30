from .Child import Child
from .Node import Node  # noqa: I201

EXPR_NODES = [
    # An inout expression.
    # &x
    Node('InOutExpr', name_for_diagnostics='inout expression', kind='Expr',
         children=[
             Child('Ampersand', kind='PrefixAmpersandToken'),
             Child('Expression', kind='Expr'),
         ]),

    # A #column expression.
    Node('PoundColumnExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('PoundColumn', kind='PoundColumnToken'),
         ]),

    Node('TupleExprElementList', name_for_diagnostics=None, kind='SyntaxCollection',
         element='TupleExprElement'),

    Node('ArrayElementList', name_for_diagnostics=None, kind='SyntaxCollection',
         element='ArrayElement'),

    Node('DictionaryElementList', name_for_diagnostics=None, kind='SyntaxCollection',
         element='DictionaryElement'),

    Node('StringLiteralSegments', name_for_diagnostics=None, kind='SyntaxCollection',
         element='Syntax', element_name='Segment',
         element_choices=['StringSegment', 'ExpressionSegment']),

    # The try operator.
    # try foo()
    # try? foo()
    # try! foo()
    Node('TryExpr', name_for_diagnostics="'try' expression", kind='Expr',
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

    # The await operator.
    # await foo()
    Node('AwaitExpr', name_for_diagnostics="'await' expression", kind='Expr',
         children=[
             Child('AwaitKeyword', kind='ContextualKeywordToken',
                   text_choices=['await']),
             Child('Expression', kind='Expr'),
         ]),

    # The move expr
    Node('MoveExpr', name_for_diagnostics="'_move' expression", kind='Expr',
         children=[
             Child('MoveKeyword', kind='ContextualKeywordToken',
                   text_choices=['_move']),
             Child('Expression', kind='Expr'),
         ]),

    # declname-arguments -> '(' declname-argument-list ')'
    # declname-argument-list -> declname-argument*
    # declname-argument -> identifier ':'
    Node('DeclNameArgument', name_for_diagnostics=None, kind='Syntax',
         children=[
             Child('Name', kind='Token'),
             Child('Colon', kind='ColonToken'),
         ]),
    Node('DeclNameArgumentList', name_for_diagnostics=None, kind='SyntaxCollection',
         element='DeclNameArgument'),
    Node('DeclNameArguments', name_for_diagnostics=None, kind='Syntax',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('Arguments', kind='DeclNameArgumentList',
                   collection_element_name='Argument'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # An identifier expression.
    Node('IdentifierExpr', name_for_diagnostics=None, kind='Expr',
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
    Node('SuperRefExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('SuperKeyword', kind='SuperToken'),
         ]),

    # A nil expression.
    Node('NilLiteralExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('NilKeyword', kind='NilToken'),
         ]),

    # A _ expression.
    Node('DiscardAssignmentExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('Wildcard', kind='WildcardToken'),
         ]),

    # An = expression.
    Node('AssignmentExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('AssignToken', kind='EqualToken'),
         ]),

    # A flat list of expressions before sequence folding, e.g. 1 + 2 + 3.
    Node('SequenceExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('Elements', kind='ExprList',
                   collection_element_name='Element'),
         ]),

    Node('ExprList', name_for_diagnostics=None, kind='SyntaxCollection',
         element='Expr',
         element_name='Expression',
         description='''
         A list of expressions connected by operators. This list is contained
         by a `SequenceExprSyntax`.
         '''),

    # A #line expression.
    Node('PoundLineExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('PoundLine', kind='PoundLineToken'),
         ]),

    # A #file expression.
    Node('PoundFileExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('PoundFile', kind='PoundFileToken'),
         ]),

    # A #fileID expression.
    Node('PoundFileIDExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('PoundFileID', kind='PoundFileIDToken'),
         ]),

    # A #filePath expression.
    Node('PoundFilePathExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('PoundFilePath', kind='PoundFilePathToken'),
         ]),

    # A #function expression.
    Node('PoundFunctionExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('PoundFunction', kind='PoundFunctionToken'),
         ]),

    # A #dsohandle expression.
    Node('PoundDsohandleExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('PoundDsohandle', kind='PoundDsohandleToken'),
         ]),

    # symbolic-reference-expression -> identifier generic-argument-clause?
    Node('SymbolicReferenceExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericArgumentClause', kind='GenericArgumentClause',
                   is_optional=True),
         ]),

    # A prefix operator expression.
    # -x
    # !true
    Node('PrefixOperatorExpr', name_for_diagnostics='prefix operator expression',
         kind='Expr',
         children=[
             Child('OperatorToken', kind='PrefixOperatorToken',
                   is_optional=True),
             Child('PostfixExpression', kind='Expr'),
         ]),

    # An operator like + or -.
    # NOTE: This appears only in SequenceExpr.
    Node('BinaryOperatorExpr', name_for_diagnostics=None,
         kind='Expr',
         children=[
             Child('OperatorToken', kind='BinaryOperatorToken'),
         ]),

    # arrow-expr -> 'async'? 'throws'? '->'
    # NOTE: This appears only in SequenceExpr.
    Node('ArrowExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('AsyncKeyword', kind='ContextualKeywordToken',
                   text_choices=['async'], is_optional=True),
             Child('ThrowsToken', kind='ThrowsToken',
                   is_optional=True),
             Child('ArrowToken', kind='ArrowToken'),
         ]),

    # An infix binary expression like x + y.
    # NOTE: This won't come directly out of the parser. Rather, it is the
    # result of "folding" a SequenceExpr based on knowing the precedence
    # relationships amongst the different infix operators.
    Node('InfixOperatorExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('LeftOperand', kind='Expr'),
             Child('OperatorOperand', kind='Expr'),
             Child('RightOperand', kind='Expr'),
         ]),

    # A floating-point literal
    # 4.0
    # -3.9
    # +4e20
    Node('FloatLiteralExpr', name_for_diagnostics='floating literal', kind='Expr',
         children=[
             Child('FloatingDigits', kind='FloatingLiteralToken'),
         ]),

    Node('TupleExpr', name_for_diagnostics='tuple', kind='Expr',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('ElementList', kind='TupleExprElementList',
                   collection_element_name='Element'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # Array literal, e.g. [1, 2, 3]
    Node('ArrayExpr', name_for_diagnostics='array', kind='Expr',
         children=[
             Child('LeftSquare', kind='LeftSquareBracketToken'),
             Child('Elements', kind='ArrayElementList',
                   collection_element_name='Element'),
             Child('RightSquare', kind='RightSquareBracketToken'),
         ]),

    # Dictionary literal, e.g. [1:1, 2:2, 3:3]
    Node('DictionaryExpr', name_for_diagnostics='dictionary', kind='Expr',
         children=[
             Child('LeftSquare', kind='LeftSquareBracketToken'),
             Child('Content', kind='Syntax',
                   node_choices=[
                       Child('Colon', kind='ColonToken'),
                       Child('Elements', kind='DictionaryElementList'),
                   ]),
             Child('RightSquare', kind='RightSquareBracketToken'),
         ]),

    # An element inside a tuple element list
    Node('TupleExprElement', name_for_diagnostics=None, kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
             Child('Label', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken'
                   ]),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # element inside an array expression: expression ','?
    Node('ArrayElement', name_for_diagnostics=None, kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # element inside an array expression: expression ','?
    Node('DictionaryElement', name_for_diagnostics=None, kind='Syntax',
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
    Node('IntegerLiteralExpr', name_for_diagnostics='integer literal', kind='Expr',
         children=[
             Child('Digits', kind='IntegerLiteralToken'),
         ]),

    # true or false
    Node('BooleanLiteralExpr', name_for_diagnostics='bool literal', kind='Expr',
         children=[
             Child("BooleanLiteral", kind='Token',
                   token_choices=[
                       'TrueToken',
                       'FalseToken',
                   ])
         ]),

    # ? expr :
    # Ternary expression without the condition and the second choice.
    # NOTE: This appears only in SequenceExpr.
    Node('UnresolvedTernaryExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child("QuestionMark", kind='InfixQuestionMarkToken'),
             Child("FirstChoice", kind='Expr'),
             Child("ColonMark", kind='ColonToken'),
         ]),


    # a ? 1 : 0
    # NOTE: This won't come directly out of the parser. Rather, it is the
    # result of "folding" a SequenceExpr based on knowing the precedence
    # relationships amongst the different infix operators.
    Node('TernaryExpr', name_for_diagnostics='ternay expression', kind='Expr',
         children=[
             Child("ConditionExpression", kind='Expr'),
             Child("QuestionMark", kind='InfixQuestionMarkToken'),
             Child("FirstChoice", kind='Expr'),
             Child("ColonMark", kind='ColonToken'),
             Child("SecondChoice", kind='Expr')
         ]),

    # expr?.name
    Node('MemberAccessExpr', name_for_diagnostics='member access', kind='Expr',
         children=[
             # The base needs to be optional to parse expressions in key paths
             # like \.a
             Child("Base", kind='Expr', is_optional=True),
             Child("Dot", kind='Token',
                   token_choices=[
                       'PeriodToken', 'PrefixPeriodToken'
                   ]),
             # Name could be 'self'
             Child("Name", kind='Token'),
             Child('DeclNameArguments', kind='DeclNameArguments',
                   is_optional=True),
         ]),

    # 'is'
    # "is" type casting ooperator without operands.
    # NOTE: This appears only in SequenceExpr.
    Node('UnresolvedIsExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child("IsTok", kind='IsToken'),
         ]),

    # expression is TypeName
    # NOTE: This won't come directly out of the parser. Rather, it is the
    # result of "folding" a SequenceExpr based on knowing the precedence
    # relationships amongst the different infix operators.
    Node('IsExpr', name_for_diagnostics="'is' expression", kind='Expr',
         children=[
             Child("Expression", kind="Expr"),
             Child("IsTok", kind='IsToken'),
             Child("TypeName", kind='Type')
         ]),

    # 'as' ('?'|'!')
    # "as" type casting ooperator without operands.
    # NOTE: This appears only in SequenceExpr.
    Node('UnresolvedAsExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child("AsTok", kind='AsToken'),
             Child("QuestionOrExclamationMark", kind='Token',
                   is_optional=True,
                   token_choices=[
                       'PostfixQuestionMarkToken',
                       'ExclamationMarkToken',
                   ]),
         ]),

    # expression as TypeName
    # NOTE: This won't come directly out of the parser. Rather, it is the
    # result of "folding" a SequenceExpr based on knowing the precedence
    # relationships amongst the different infix operators.
    Node('AsExpr', name_for_diagnostics="'as' expression", kind='Expr',
         children=[
             Child("Expression", kind="Expr"),
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
    Node('TypeExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('Type', kind='Type'),
         ]),

    Node('ClosureCaptureItem', name_for_diagnostics='closure capture item',
         kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
             # FIXME: Add a 'CaptureSpecifier' node kind for `Specifier`.
             Child("Specifier", kind='TokenList',
                   collection_element_name='SpecifierToken', is_optional=True),
             Child("Name", kind='IdentifierToken', is_optional=True),
             Child('AssignToken', kind='EqualToken', is_optional=True),
             Child("Expression", kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    Node('ClosureCaptureItemList', name_for_diagnostics=None, kind='SyntaxCollection',
         element='ClosureCaptureItem'),

    Node('ClosureCaptureSignature', name_for_diagnostics='closure capture signature',
         kind='Syntax',
         children=[
             Child('LeftSquare', kind='LeftSquareBracketToken'),
             Child('Items', kind='ClosureCaptureItemList',
                   collection_element_name='Item', is_optional=True),
             Child('RightSquare', kind='RightSquareBracketToken'),
         ]),

    Node('ClosureParam', name_for_diagnostics='closure parameter', kind='Syntax',
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
    Node('ClosureParamList', name_for_diagnostics=None, kind='SyntaxCollection',
         element='ClosureParam'),

    Node('ClosureSignature', name_for_diagnostics='closure signature', kind='Syntax',
         children=[
             Child('Attributes', kind='AttributeList',
                   collection_element_name='Attribute', is_optional=True),
             Child('Capture', kind='ClosureCaptureSignature',
                   is_optional=True),
             Child('Input', kind='Syntax', is_optional=True,
                   node_choices=[
                       Child('SimpleInput', kind='ClosureParamList'),
                       Child('Input', kind='ParameterClause'),
                   ]),
             Child('AsyncKeyword', kind='ContextualKeywordToken',
                   text_choices=['async'], is_optional=True),
             Child('ThrowsTok', kind='ThrowsToken', is_optional=True),
             Child('Output', kind='ReturnClause', is_optional=True),
             Child('InTok', kind='InToken'),
         ]),

    Node('ClosureExpr', name_for_diagnostics='closure', kind='Expr',
         traits=['Braced', 'WithStatements'],
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Signature', kind='ClosureSignature', is_optional=True),
             Child('Statements', kind='CodeBlockItemList',
                   collection_element_name='Statement', is_indented=True),
             Child('RightBrace', kind='RightBraceToken',
                   requires_leading_newline=True),
         ]),

    # unresolved-pattern-expr -> pattern
    Node('UnresolvedPatternExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('Pattern', kind='Pattern'),
         ]),

    # trailing-closure-element -> identifier ':' closure-expression
    Node('MultipleTrailingClosureElement', name_for_diagnostics='trailing closure',
         kind='Syntax',
         children=[
             Child('Label', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken'
                   ]),
             Child('Colon', kind='ColonToken'),
             Child('Closure', kind='ClosureExpr'),
         ]),

    Node('MultipleTrailingClosureElementList', name_for_diagnostics=None,
         kind='SyntaxCollection', element='MultipleTrailingClosureElement'),

    # call-expr -> expr '(' call-argument-list ')' closure-expr?
    #            | expr closure-expr
    Node('FunctionCallExpr', name_for_diagnostics='function call', kind='Expr',
         children=[
             Child('CalledExpression', kind='Expr'),
             Child('LeftParen', kind='LeftParenToken',
                   is_optional=True),
             Child('ArgumentList', kind='TupleExprElementList',
                   collection_element_name='Argument'),
             Child('RightParen', kind='RightParenToken',
                   is_optional=True),
             Child('TrailingClosure', kind='ClosureExpr',
                   is_optional=True),
             Child('AdditionalTrailingClosures',
                   kind='MultipleTrailingClosureElementList',
                   collection_element_name='AdditionalTrailingClosure',
                   is_optional=True),
         ]),

    # subscript-expr -> expr '[' call-argument-list ']' closure-expr?
    Node('SubscriptExpr', name_for_diagnostics='subscript', kind='Expr',
         children=[
             Child('CalledExpression', kind='Expr'),
             Child('LeftBracket', kind='LeftSquareBracketToken'),
             Child('ArgumentList', kind='TupleExprElementList',
                   collection_element_name='Argument'),
             Child('RightBracket', kind='RightSquareBracketToken'),
             Child('TrailingClosure', kind='ClosureExpr',
                   is_optional=True),
             Child('AdditionalTrailingClosures',
                   kind='MultipleTrailingClosureElementList',
                   collection_element_name='AdditionalTrailingClosure',
                   is_optional=True),
         ]),

    # optional-chaining-expr -> expr '?'
    Node('OptionalChainingExpr', name_for_diagnostics='optional chaining', kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('QuestionMark', kind='PostfixQuestionMarkToken'),
         ]),

    # forced-value-expr -> expr '!'
    Node('ForcedValueExpr', name_for_diagnostics='force unwrap', kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('ExclamationMark', kind='ExclamationMarkToken'),
         ]),

    # postfix-unary-expr -> expr postfix-operator
    Node('PostfixUnaryExpr', name_for_diagnostics='postfix expression', kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('OperatorToken', kind='PostfixOperatorToken'),
         ]),

    # specialize-expr -> expr generic-argument-clause?
    Node('SpecializeExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('Expression', kind='Expr'),
             Child('GenericArgumentClause', kind='GenericArgumentClause'),
         ]),

    # string literal segment in a string interpolation expression.
    Node('StringSegment', name_for_diagnostics=None, kind='Syntax',
         children=[
             Child('Content', kind='StringSegmentToken'),
         ]),

    # expression segment in a string interpolation expression.
    Node('ExpressionSegment', name_for_diagnostics=None, kind='Syntax',
         traits=['Parenthesized'],
         children=[
             Child('Backslash', kind='BackslashToken'),
             Child('Delimiter', kind='RawStringDelimiterToken',
                   is_optional=True),
             Child('LeftParen', kind='LeftParenToken',
                   classification='StringInterpolationAnchor',
                   force_classification=True),
             Child('Expressions', kind='TupleExprElementList',
                   collection_element_name='Expression'),
             Child('RightParen', kind='StringInterpolationAnchorToken'),
         ]),

    # e.g. "abc \(foo()) def"
    Node('StringLiteralExpr', name_for_diagnostics='string literal', kind='Expr',
         children=[
             Child('OpenDelimiter', kind='RawStringDelimiterToken',
                   is_optional=True),
             Child('OpenQuote', kind='Token',
                   token_choices=[
                       'StringQuoteToken',
                       'MultilineStringQuoteToken',
                   ]),
             Child('Segments', kind='StringLiteralSegments',
                   collection_element_name='Segment'),
             Child('CloseQuote', kind='Token',
                   token_choices=[
                       'StringQuoteToken',
                       'MultilineStringQuoteToken',
                   ]),
             Child('CloseDelimiter', kind='RawStringDelimiterToken',
                   is_optional=True),
         ]),

    # e.g '(a|c)*', the contents of the literal is opaque to the C++ Swift
    # parser though.
    Node('RegexLiteralExpr', name_for_diagnostics='regex literal', kind='Expr',
         children=[
             Child('Regex', kind='RegexLiteralToken'),
         ]),

    # e.g. "\a.b[2].a"
    Node('KeyPathExpr', name_for_diagnostics='key path', kind='Expr',
         children=[
             Child('Backslash', kind='BackslashToken'),
             Child('RootExpr', kind='Expr', is_optional=True,
                   node_choices=[
                       Child('IdentifierExpr', kind='IdentifierExpr'),
                       Child('SpecializeExpr', kind='SpecializeExpr'),
                       Child('OptionalChainingExpr', kind='OptionalChainingExpr'),
                   ]),
             Child('Expression', kind='Expr'),
         ]),

    # The period in the key path serves as the base on which the
    # right-hand-side of the key path is evaluated
    Node('KeyPathBaseExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('Period', kind='PeriodToken'),
         ]),

    # e.g. "a." or "a"
    Node('ObjcNamePiece', name_for_diagnostics=None, kind='Syntax',
         children=[
             Child('Name', kind='IdentifierToken'),
             Child('Dot', kind='PeriodToken', is_optional=True),
         ]),

    # e.g. "a.b.c"
    Node('ObjcName', name_for_diagnostics=None, kind='SyntaxCollection',
         element='ObjcNamePiece'),

    # e.g. "#keyPath(a.b.c)"
    Node('ObjcKeyPathExpr', name_for_diagnostics="'#keyPath' expression", kind='Expr',
         traits=['Parenthesized'],
         children=[
             Child('KeyPath', kind='PoundKeyPathToken'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('Name', kind='ObjcName',
                   collection_element_name='NamePiece'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # e.g. "#selector(getter:Foo.bar)"
    Node('ObjcSelectorExpr', name_for_diagnostics="'#selector' expression", kind='Expr',
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

    # postfix '#if' expression
    Node('PostfixIfConfigExpr', name_for_diagnostics=None, kind='Expr',
         children=[
             Child('Base', kind='Expr', is_optional=True),
             Child('Config', kind='IfConfigDecl'),
         ]),

    # <#content#>
    Node('EditorPlaceholderExpr', name_for_diagnostics='editor placeholder',
         kind='Expr',
         children=[
             Child('Identifier', kind='IdentifierToken'),
         ]),
    # #fileLiteral(a, b, c)
    Node('ObjectLiteralExpr', name_for_diagnostics='object literal', kind='Expr',
         traits=['Parenthesized'],
         children=[
             Child('Identifier', kind='Token',
                   token_choices=[
                       'PoundColorLiteralToken',
                       'PoundFileLiteralToken',
                       'PoundImageLiteralToken',
                   ]),
             Child('LeftParen', kind='LeftParenToken'),
             Child('Arguments', kind='TupleExprElementList',
                   collection_element_name='Argument'),
             Child('RightParen', kind='RightParenToken'),
         ]),
]
