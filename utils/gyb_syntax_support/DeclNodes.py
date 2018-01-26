# flake8: noqa I201
from Child import Child
from Node import Node


DECL_NODES = [
    # initializer -> '=' type
    Node('TypeInitializerClause', kind='Syntax',
         children=[
             Child('Equal', kind='EqualToken'),
             Child('Value', kind='Type'),
         ]),

    # typealias-declaration -> attributes? access-level-modifier? 'typealias'
    #                            typealias-name generic-parameter-clause?
    #                            typealias-assignment
    # typealias-name -> identifier
    # typealias-assignment -> = type
    Node('TypealiasDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('AccessLevelModifier', kind='DeclModifier',
                   is_optional=True),
             Child('TypealiasKeyword', kind='TypealiasToken'),
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('Initializer', kind='TypeInitializerClause',
                   is_optional=True)
         ]),

    Node('FunctionParameterList', kind='SyntaxCollection',
         element='FunctionParameter'),

    Node('ParameterClause', kind='Syntax',
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

    # else-if-directive-clause -> '#elseif' expr stmt-list
    Node('ElseifDirectiveClause', kind='Syntax',
         children=[
             Child('PoundElseif', kind='PoundElseifToken'),
             Child('Condition', kind='Expr'),
             Child('Body', kind='StmtList'),
         ]),

    # if-config-decl -> '#if' expr stmt-list else-if-directive-clause-list
    #   else-clause? '#endif'
    Node('IfConfigDecl', kind='Decl',
         children=[
             Child('PoundIf', kind='PoundIfToken'),
             Child('Condition', kind='Expr'),
             Child('Body', kind='StmtList'),
             Child('ElseifDirectiveClauses', kind='ElseifDirectiveClauseList',
                   is_optional=True),
             Child('ElseClause', kind='ElseDirectiveClause',
                   is_optional=True),
             Child('PoundEndif', kind='PoundEndifToken'),
         ]),

    Node('DeclModifier', kind='Syntax',
         children=[
             Child('Name', kind='Token',
                   text_choices=[
                       'class', 'convenience', 'dynamic', 'final', 'infix',
                       'lazy', 'optional', 'override', 'postfix', 'prefix',
                       'required', 'static', 'unowned', 'weak', 'private',
                       'fileprivate', 'internal', 'public', 'open',
                       'mutating', 'nonmutating',
                   ]),
             Child('Detail', kind='TokenList', is_optional=True),
         ]),

    Node('InheritedType', kind='Syntax',
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
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('AccessLevelModifier', kind='DeclModifier',
                   is_optional=True),
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
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('AccessLevelModifier', kind='DeclModifier',
                   is_optional=True),
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
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('AccessLevelModifier', kind='DeclModifier',
                   is_optional=True),
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
    Node('ExtensionDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('AccessLevelModifier', kind='DeclModifier',
                   is_optional=True),
             Child('ExtensionKeyword', kind='ExtensionToken'),
             Child('ExtendedType', kind='Type'),
             Child('InheritanceClause', kind='TypeInheritanceClause',
                   is_optional=True),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             Child('Members', kind='MemberDeclBlock'),
         ]),

    Node('MemberDeclBlock', kind='Syntax',
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Members', kind='DeclList'),
             Child('RightBrace', kind='RightBraceToken'),
         ]),

    # decl-list = decl decl-list?
    Node('DeclList', kind='SyntaxCollection',
         element='Decl'),

    # source-file = decl-list eof
    Node('SourceFile', kind='Syntax',
         children=[
             Child('TopLevelDecls', kind='DeclList'),
             Child('EOFToken', kind='EOFToken')
         ]),

    # top-level-code-decl = stmt-list
    Node('TopLevelCodeDecl', kind='Decl',
         children=[
             Child('Body', kind='StmtList')
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
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('FirstName', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken',
                   ]),
             # One of these two names needs be optional, we choose the second
             # name to avoid backtracking.
             Child('SecondName', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken',
                   ],
                   is_optional=True),
             Child('Colon', kind='ColonToken'),
             Child('TypeAnnotation', kind='Type'),
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
         element='Syntax',
         element_name='Modifier'),

    Node('FunctionDecl', kind='Decl',
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

    # else-if-directive-clause-list -> else-if-directive-clause
    #   else-if-directive-clause-list?
    Node('ElseifDirectiveClauseList', kind='SyntaxCollection',
         element='ElseifDirectiveClause'),

    # else-directive-clause -> '#else' stmt-list
    Node('ElseDirectiveClause', kind='Syntax',
         children=[
             Child('PoundElse', kind='PoundElseToken'),
             Child('Body', kind='StmtList'),
         ]),

    # access-level-modifier -> 'private' | 'private' '(' 'set' ')'
    #                        | 'fileprivate' | 'fileprivate' '(' 'set' ')'
    #                        | 'internal' | 'internal' '(' 'set' ')'
    #                        | 'public' | 'public' '(' 'set' ')'
    #                        | 'open' | 'open' '(' 'set' ')'
    Node('AccessLevelModifier', kind='Syntax',
         children=[
             Child('Name', kind='IdentifierToken'),
             Child('OpenParen', kind='LeftParenToken',
                   is_optional=True),
             Child('Modifier', kind='IdentifierToken',
                   is_optional=True),
             Child('CloseParen', kind='RightParenToken',
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

    Node('AccessorBlock', kind="Syntax",
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('AccessorListOrStmtList', kind='Syntax',
                   node_choices=[
                      Child('Accessors', kind='AccessorList'),
                      Child('Statements', kind='StmtList')]),
             Child('RightBrace', kind='RightBraceToken'),
         ]),

    # Pattern: Type = Value { get {} },
    Node('PatternBinding', kind="Syntax",
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
]
