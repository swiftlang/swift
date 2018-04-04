# flake8: noqa I201
from Child import Child
from Node import Node


DECL_NODES = [
    # type-assignment -> '=' type
    Node('TypeInitializerClause', kind='Syntax',
         children=[
             Child('Equal', kind='EqualToken'),
             Child('Value', kind='Type'),
         ]),

    # typealias-declaration -> attributes? access-level-modifier? 'typealias'
    #                            typealias-name generic-parameter-clause?
    #                            type-assignment
    # typealias-name -> identifier
    Node('TypealiasDecl', kind='Decl', traits=['IdentifiedDecl'],
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList', is_optional=True),
             Child('TypealiasKeyword', kind='TypealiasToken'),
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('Initializer', kind='TypeInitializerClause',
                   is_optional=True),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
         ]),

    # associatedtype-declaration -> attributes? access-level-modifier?
    #                                 'associatedtype' associatedtype-name
    #                                 inheritance-clause? type-assignment?
    #                                 generic-where-clause?
    # associatedtype-name -> identifier
    Node('AssociatedtypeDecl', kind='Decl', traits=['IdentifiedDecl'],
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList', is_optional=True),
             Child('AssociatedtypeKeyword', kind='AssociatedtypeToken'),
             Child('Identifier', kind='IdentifierToken'),
             Child('InheritanceClause', kind='TypeInheritanceClause',
                   is_optional=True),
             Child('Initializer', kind='TypeInitializerClause',
                   is_optional=True),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
         ]),

    Node('FunctionParameterList', kind='SyntaxCollection',
         element='FunctionParameter'),

    Node('ParameterClause', kind='Syntax',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('ParameterList', kind='FunctionParameterList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # -> Type
    Node('ReturnClause', kind='Syntax',
         children=[
             Child('Arrow', kind='ArrowToken'),
             Child('ReturnType', kind='Type'),
         ]),

    # function-signature ->
    #   '(' parameter-list? ')' (throws | rethrows)? '->'? type?
    Node('FunctionSignature', kind='Syntax',
         children=[
             Child('Input', kind='ParameterClause'),
             Child('ThrowsOrRethrowsKeyword', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'ThrowsToken',
                       'RethrowsToken',
                   ]),
             Child('Output', kind='ReturnClause', is_optional=True),
         ]),

    # if-config-clause ->
    #    ('#if' | '#elseif' | '#else') expr? (stmt-list | switch-case-list)
    Node('IfConfigClause', kind='Syntax',
         children=[
             Child('PoundKeyword', kind='Token',
                   token_choices=[
                       'PoundIfToken',
                       'PoundElseifToken',
                       'PoundElseToken',
                   ]),
             Child('Condition', kind='Expr',
                   is_optional=True),
             Child('Elements', kind='Syntax',
                   node_choices=[
                      Child('Statements', kind='CodeBlockItemList'),
                      Child('SwitchCases', kind='SwitchCaseList')]),
         ]),

    Node('IfConfigClauseList', kind='SyntaxCollection',
         element='IfConfigClause'),

    # if-config-decl -> '#if' expr stmt-list else-if-directive-clause-list
    #   else-clause? '#endif'
    Node('IfConfigDecl', kind='Decl',
         children=[
             Child('Clauses', kind='IfConfigClauseList'),
             Child('PoundEndif', kind='PoundEndifToken'),
         ]),

    Node('PoundErrorDecl', kind='Decl',
         traits=['Parenthesized'],
         children=[
             Child('PoundError', kind='PoundErrorToken'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('Message', kind='StringLiteralExpr'),
             Child('RightParen', kind='RightParenToken')
         ]),

    Node('PoundWarningDecl', kind='Decl',
         traits=['Parenthesized'],
         children=[
             Child('PoundWarning', kind='PoundWarningToken'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('Message', kind='StringLiteralExpr'),
             Child('RightParen', kind='RightParenToken')
         ]),

    Node('DeclModifier', kind='Syntax',
         children=[
             Child('Name', kind='Token',
                   text_choices=[
                       'class', 'convenience', 'dynamic', 'final', 'infix',
                       'lazy', 'optional', 'override', 'postfix', 'prefix',
                       'required', 'static', 'unowned', 'weak', 'private',
                       'fileprivate', 'internal', 'public', 'open',
                       'mutating', 'nonmutating', 'indirect',
                   ]),
             Child('Detail', kind='TokenList', is_optional=True),
         ]),

    Node('InheritedType', kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
            Child('TypeName', kind='Type'),
            Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    Node('InheritedTypeList', kind='SyntaxCollection',
         element='InheritedType'),

    # type-inheritance-clause -> ':' type
    Node('TypeInheritanceClause', kind='Syntax',
         children=[
             Child('Colon', kind='ColonToken'),
             Child('InheritedTypeCollection', kind='InheritedTypeList'),
         ]),

    # class-declaration -> attributes? access-level-modifier?
    #                      'class' class-name
    #                      generic-parameter-clause?
    #                      type-inheritance-clause?
    #                      generic-where-clause?
    #                     '{' class-members '}'
    # class-name -> identifier
    Node('ClassDecl', kind='Decl',
         traits=['DeclGroup', 'IdentifiedDecl'],
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList', is_optional=True),
             Child('ClassKeyword', kind='ClassToken'),
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('InheritanceClause', kind='TypeInheritanceClause',
                   is_optional=True),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             Child('Members', kind='MemberDeclBlock'),
         ]),

    # struct-declaration -> attributes? access-level-modifier?
    #                         'struct' struct-name
    #                         generic-parameter-clause?
    #                           type-inheritance-clause?
    #                         generic-where-clause?
    #                         '{' struct-members '}'
    # struct-name -> identifier
    Node('StructDecl', kind='Decl',
         traits=['DeclGroup', 'IdentifiedDecl'],
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList', is_optional=True),
             Child('StructKeyword', kind='StructToken'),
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('InheritanceClause', kind='TypeInheritanceClause',
                   is_optional=True),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             Child('Members', kind='MemberDeclBlock'),
         ]),

    Node('ProtocolDecl', kind='Decl',
         traits=['DeclGroup', 'IdentifiedDecl'],
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList', is_optional=True),
             Child('ProtocolKeyword', kind='ProtocolToken'),
             Child('Identifier', kind='IdentifierToken'),
             Child('InheritanceClause', kind='TypeInheritanceClause',
                   is_optional=True),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             Child('Members', kind='MemberDeclBlock'),
         ]),

    # extension-declaration -> attributes? access-level-modifier?
    #                            'extension' extended-type
    #                              type-inheritance-clause?
    #                            generic-where-clause?
    #                            '{' extension-members '}'
    # extension-name -> identifier
    Node('ExtensionDecl', kind='Decl', traits=['DeclGroup'],
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList', is_optional=True),
             Child('ExtensionKeyword', kind='ExtensionToken'),
             Child('ExtendedType', kind='Type'),
             Child('InheritanceClause', kind='TypeInheritanceClause',
                   is_optional=True),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             Child('Members', kind='MemberDeclBlock'),
         ]),

    Node('MemberDeclBlock', kind='Syntax', traits=['Braced'],
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Members', kind='DeclList'),
             Child('RightBrace', kind='RightBraceToken'),
         ]),

    # decl-list = decl decl-list?
    Node('DeclList', kind='SyntaxCollection',
         element='Decl'),

    # source-file = code-block-item-list eof
    Node('SourceFile', kind='Syntax',
         traits=['WithStatements'],
         children=[
             Child('Statements', kind='CodeBlockItemList'),
             Child('EOFToken', kind='EOFToken')
         ]),

    # initializer -> '=' expr
    Node('InitializerClause', kind='Syntax',
         children=[
             Child('Equal', kind='EqualToken'),
             Child('Value', kind='Expr'),
         ]),

    # parameter ->
    # external-parameter-name? local-parameter-name ':'
    #   type '...'? '='? expression? ','?
    Node('FunctionParameter', kind='Syntax',
         traits=['WithTrailingComma'],
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('FirstName', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken',
                   ],
                   is_optional=True),
             # One of these two names needs be optional, we choose the second
             # name to avoid backtracking.
             Child('SecondName', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken',
                   ],
                   is_optional=True),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('Type', kind='Type',
                   is_optional=True),
             Child('Ellipsis', kind='Token',
                   is_optional=True),
             Child('DefaultArgument', kind='InitializerClause',
                   is_optional=True),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # declaration-modifier -> access-level-modifier
    #                       | mutation-modifier
    #                       | 'class'
    #                       | 'convenience'
    #                       | 'dynamic'
    #                       | 'final'
    #                       | 'infix'
    #                       | 'lazy'
    #                       | 'optional'
    #                       | 'override'
    #                       | 'postfix'
    #                       | 'prefix'
    #                       | 'required'
    #                       | 'static'
    #                       | 'unowned'
    #                       | 'unowned(safe)'
    #                       | 'unowned(unsafe)'
    #                       | 'weak'
    # mutation-modifier -> 'mutating' | 'nonmutating'
    Node('ModifierList', kind='SyntaxCollection',
         element='DeclModifier',
         element_name='Modifier'),

    Node('FunctionDecl', kind='Decl', traits=['IdentifiedDecl'],
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList',
                   is_optional=True),
             Child('FuncKeyword', kind='FuncToken'),
             Child('Identifier', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'OperatorToken',
                       'UnspacedBinaryOperatorToken',
                       'SpacedBinaryOperatorToken',
                       'PrefixOperatorToken',
                       'PostfixOperatorToken',
                   ]),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('Signature', kind='FunctionSignature'),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             # the body is not necessary inside a protocol definition
             Child('Body', kind='CodeBlock', is_optional=True),
         ]),

    Node('InitializerDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList',
                   is_optional=True),
             Child('InitKeyword', kind='InitToken'),
             Child('OptionalMark', kind='Token',
                   token_choices=[
                       'PostfixQuestionMarkToken',
                       'InfixQuestionMarkToken',
                       'ExclamationMarkToken',
                   ],
                   is_optional=True),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('Parameters', kind='ParameterClause'),
             Child('ThrowsOrRethrowsKeyword', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'ThrowsToken',
                       'RethrowsToken',
                   ]),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             # the body is not necessary inside a protocol definition
             Child('Body', kind='CodeBlock', is_optional=True),
         ]),

    Node('DeinitializerDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList',
                   is_optional=True),
             Child('DeinitKeyword', kind='DeinitToken'),
             Child('Body', kind='CodeBlock'),
         ]),

    Node('SubscriptDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('Modifiers', kind='ModifierList',
                   is_optional=True),
             Child('SubscriptKeyword', kind='SubscriptToken'),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('Indices', kind='ParameterClause'),
             Child('Result', kind='ReturnClause'),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             # the body is not necessary inside a protocol definition
             Child('Accessor', kind='AccessorBlock', is_optional=True),
         ]),

    # access-level-modifier -> 'private' | 'private' '(' 'set' ')'
    #                        | 'fileprivate' | 'fileprivate' '(' 'set' ')'
    #                        | 'internal' | 'internal' '(' 'set' ')'
    #                        | 'public' | 'public' '(' 'set' ')'
    #                        | 'open' | 'open' '(' 'set' ')'
    Node('AccessLevelModifier', kind='Syntax',
         children=[
             Child('Name', kind='IdentifierToken'),
             Child('LeftParen', kind='LeftParenToken',
                   is_optional=True),
             Child('Modifier', kind='IdentifierToken',
                   is_optional=True),
             Child('RightParen', kind='RightParenToken',
                   is_optional=True),
         ]),

    Node('AccessPathComponent', kind='Syntax',
         children=[
            Child('Name', kind='IdentifierToken'),
            Child('TrailingDot', kind='PeriodToken', is_optional=True),
         ]),

    Node('AccessPath', kind='SyntaxCollection', element='AccessPathComponent'),

    Node('ImportDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList', is_optional=True),
             Child('Modifiers', kind='ModifierList',
                   is_optional=True),
             Child('ImportTok', kind='ImportToken'),
             Child('ImportKind', kind='Token', is_optional=True,
                   token_choices=[
                      'TypealiasToken', 'StructToken', 'ClassToken',
                      'EnumToken', 'ProtocolToken', 'VarToken', 'LetToken',
                      'FuncToken',
                   ]),
             Child('Path', kind='AccessPath'),
         ]),

    # (value)
    Node('AccessorParameter', kind='Syntax',
         traits=['Parenthesized'],
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('Name', kind='IdentifierToken'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    Node('AccessorDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList', is_optional=True),
             Child('Modifier', kind='DeclModifier', is_optional=True),
             Child('AccessorKind', kind='Token',
                   text_choices=[
                      'get', 'set', 'didSet', 'willSet',
                   ]),
             Child('Parameter', kind='AccessorParameter', is_optional=True),
             Child('Body', kind='CodeBlock', is_optional=True),
         ]),

    Node('AccessorList', kind="SyntaxCollection", element='AccessorDecl'),

    Node('AccessorBlock', kind="Syntax", traits=['Braced'],
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('AccessorListOrStmtList', kind='Syntax',
                   node_choices=[
                      Child('Accessors', kind='AccessorList'),
                      Child('Statements', kind='CodeBlockItemList')]),
             Child('RightBrace', kind='RightBraceToken'),
         ]),

    # Pattern: Type = Value { get {} },
    Node('PatternBinding', kind="Syntax",
         traits=['WithTrailingComma'],
         children=[
             Child('Pattern', kind='Pattern'),
             Child('TypeAnnotation', kind='TypeAnnotation', is_optional=True),
             Child('Initializer', kind='InitializerClause', is_optional=True),
             Child('Accessor', kind='AccessorBlock', is_optional=True),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    Node('PatternBindingList', kind="SyntaxCollection",
         element='PatternBinding'),

    Node('VariableDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList', is_optional=True),
             Child('Modifiers', kind='ModifierList', is_optional=True),
             Child('LetOrVarKeyword', kind='Token',
                   token_choices=[
                       'LetToken', 'VarToken',
                   ]),
             Child('Bindings', kind='PatternBindingList'),
         ]),

    Node('EnumCaseElement', kind='Syntax',
         description='''
         An element of an enum case, containing the name of the case and, \
         optionally, either associated values or an assignment to a raw value.
         ''',
         traits=['WithTrailingComma'],
         children=[
             Child('Identifier', kind='IdentifierToken',
                   description='The name of this case.'),
             Child('AssociatedValue', kind='ParameterClause', is_optional=True,
                   description='The set of associated values of the case.'),
             Child('RawValue', kind='InitializerClause', is_optional=True,
                   description='''
                   The raw value of this enum element, if present.
                   '''),
             Child('TrailingComma', kind='CommaToken', is_optional=True,
                   description='''
                   The trailing comma of this element, if the case has \
                   multiple elements.
                   '''),
         ]),

    Node('EnumCaseElementList', kind='SyntaxCollection',
         description='A collection of 0 or more `EnumCaseElement`s.',
         element='EnumCaseElement'),

    Node('EnumCaseDecl', kind='Decl',
         description='''
         A `case` declaration of a Swift `enum`. It can have 1 or more \
         `EnumCaseElement`s inside, each declaring a different case of the
         enum.
         ''',
         children=[
             Child('Attributes', kind='AttributeList', is_optional=True,
                   description='''
                   The attributes applied to the case declaration.
                   '''),
             Child('Modifiers', kind='ModifierList', is_optional=True,
                   description='''
                   The declaration modifiers applied to the case declaration.
                   '''),
             Child('CaseKeyword', kind='CaseToken',
                   description='The `case` keyword for this case.'),
             Child('Elements', kind='EnumCaseElementList',
                   description='The elements this case declares.')
         ]),

    Node('EnumDecl', kind='Decl', traits=['IdentifiedDecl'],
         description='A Swift `enum` declaration.',
         children=[
             Child('Attributes', kind='AttributeList', is_optional=True,
                   description='''
                   The attributes applied to the enum declaration.
                   '''),
             Child('Modifiers', kind='ModifierList', is_optional=True,
                   description='''
                   The declaration modifiers applied to the enum declaration.
                   '''),
             Child('EnumKeyword', kind='EnumToken',
                   description='''
                   The `enum` keyword for this declaration.
                   '''),
             Child('Identifier', kind='IdentifierToken',
                   description='''
                   The name of this enum.
                   '''),
             Child('GenericParameters', kind='GenericParameterClause',
                   is_optional=True,
                   description='''
                   The generic parameters, if any, for this enum.
                   '''),
             Child('InheritanceClause', kind='TypeInheritanceClause',
                   is_optional=True,
                   description='''
                   The inheritance clause describing conformances or raw \
                   values for this enum.
                   '''),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True,
                   description='''
                   The `where` clause that applies to the generic parameters of \
                   this enum.
                   '''),
             Child('Members', kind='MemberDeclBlock',
                   description='''
                   The cases and other members of this enum.
                   ''')
         ]),
]
