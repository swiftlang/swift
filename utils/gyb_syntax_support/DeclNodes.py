# flake8: noqa I201
from Child import Child
from Node import Node


DECL_NODES = [
    # typealias-declaration -> attributes? access-level-modifier? 'typealias'
    #                            typealias-name generic-parameter-clause?
    #                            typealias-assignment
    # typealias-name -> identifier
    # typealias-assignment -> = type
    Node('TypealiasDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('AccessLevelModifier', kind='AccessLevelModifier',
                   is_optional=True),
             Child('TypealiasKeyword', kind='TypealiasToken'),
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('Equals', kind='EqualToken'),
             Child('Type', kind='Type'),
         ]),

    Node('FunctionParameterList', kind='SyntaxCollection',
         element='FunctionParameter'),

    # function-signature ->
    #   '(' parameter-list? ')' (throws | rethrows)? '->'? attributes? type?
    Node('FunctionSignature', kind='Syntax',
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('ParameterList', kind='FunctionParameterList'),
             Child('RightParen', kind='RightParenToken'),
             Child('ThrowsOrRethrowsKeyword', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'ThrowsToken',
                       'RethrowsToken',
                   ]),
             Child('Arrow', kind='ArrowToken',
                   is_optional=True),
             Child('ReturnTypeAttributes', kind='AttributeList',
                   is_optional=True),
             Child('ReturnType', kind='Type',
                   is_optional=True),
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
             Child('ElseifDirectiveClauses', kind='ElseifDirectiveClauseList'),
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
             Child('Detail', kind='TokenList'),
         ]),

    # type-inheritance-clause -> ':' type
    Node('TypeInheritanceClause', kind='Syntax',
         children=[
             Child('Colon', kind='ColonToken'),
             Child('InheritedType', kind='Type'),
         ]),

    # struct-declaration -> attributes? access-level-modifier?
    #                         'struct' struct-name
    #                         generic-parameter-clause?
    #                           type-inheritance-clause?
    #                         generic-where-clause?
    #                         '{' struct-members ''
    # struct-name -> identifier
    Node('StructDecl', kind='Decl',
         children=[
             Child('Attributes', kind='AttributeList',
                   is_optional=True),
             Child('AccessLevelModifier', kind='AccessLevelModifier',
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

    # parameter ->
    # external-parameter-name? local-parameter-name ':'
    #   type '...'? '='? expression? ','?
    Node('FunctionParameter', kind='Syntax',
         children=[
             Child('ExternalName', kind='IdentifierToken',
                   is_optional=True),
             Child('LocalName', kind='IdentifierToken'),
             Child('Colon', kind='ColonToken'),
             Child('TypeAnnotation', kind='TypeAnnotation'),
             Child('Ellipsis', kind='Token',
                   is_optional=True),
             Child('DefaultEquals', kind='EqualToken',
                   is_optional=True),
             Child('DefaultValue', kind='Expr',
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
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericParameterClause', kind='GenericParameterClause',
                   is_optional=True),
             Child('Signature', kind='FunctionSignature'),
             Child('GenericWhereClause', kind='GenericWhereClause',
                   is_optional=True),
             Child('Body', kind='CodeBlock'),
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
]
