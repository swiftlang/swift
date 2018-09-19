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
                   classification='BuildConfigId',
                   token_choices=[
                       'PoundIfToken',
                       'PoundElseifToken',
                       'PoundElseToken',
                   ]),
             Child('Condition', kind='Expr', classification='BuildConfigId',
                   is_optional=True),
             Child('Elements', kind='Syntax',
                   node_choices=[
                       Child('Statements', kind='CodeBlockItemList'),
                       Child('SwitchCases', kind='SwitchCaseList'),
                       Child('Decls', kind='MemberDeclList'),
                       Child('ArrayElements', kind='ArrayElementList'),
                       Child('DictionaryElements', kind='DictionaryElementList'),
                   ]),
         ]),

    Node('IfConfigClauseList', kind='SyntaxCollection',
         element='IfConfigClause'),

    # if-config-decl -> '#if' expr stmt-list else-if-directive-clause-list
    #   else-clause? '#endif'
    Node('IfConfigDecl', kind='Decl',
         children=[
             Child('Clauses', kind='IfConfigClauseList'),
             Child('PoundEndif', kind='PoundEndifToken', 
                   classification='BuildConfigId'),
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

    Node('PoundSourceLocation', kind='Decl', 
         traits=['Parenthesized'],
         children=[
             Child('PoundSourceLocation', kind='PoundSourceLocationToken'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('Args', kind='PoundSourceLocationArgs', is_optional=True),
             Child('RightParen', kind='RightParenToken')
         ]),

    Node('PoundSourceLocationArgs', kind='Syntax',
         children=[
             Child('FileArgLabel', kind='IdentifierToken', 
                   text_choices=['file']),
             Child('FileArgColon', kind='ColonToken'),
             Child('FileName', kind='StringLiteralToken'),
             Child('Comma', kind='CommaToken'),
             Child('LineArgLabel', kind='IdentifierToken', 
                   text_choices=['line']),
             Child('LineArgColon', kind='ColonToken'),
             Child('LineNumber', kind='IntegerLiteralToken'),
         ]),

    Node('DeclModifier', kind='Syntax',
         children=[
             Child('Name', kind='Token', classification='Attribute',
                   text_choices=[
                       'class', 'convenience', 'dynamic', 'final', 'infix',
                       'lazy', 'optional', 'override', 'postfix', 'prefix',
                       'required', 'static', 'unowned', 'weak', 'private',
                       'fileprivate', 'internal', 'public', 'open',
                       'mutating', 'nonmutating', 'indirect', '__consuming'
                   ]),
             Child('DetailLeftParen', kind='LeftParenToken', is_optional=True),
             Child('Detail', kind='IdentifierToken', is_optional=True),
             Child('DetailRightParen', kind='RightParenToken', is_optional=True),
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
             Child('Members', kind='MemberDeclList'),
             Child('RightBrace', kind='RightBraceToken'),
         ]),

    # member-decl-list = member-decl member-decl-list?
    Node('MemberDeclList', kind='SyntaxCollection',
         element='MemberDeclListItem'),

    # member-decl = decl ';'?
    Node('MemberDeclListItem', kind='Syntax',
         description='''
         A member declaration of a type consisting of a declaration and an \
         optional semicolon;
         ''',
         children=[
             Child('Decl', kind='Decl', 
                   description='The declaration of the type member.'),
             Child('Semicolon', kind='SemicolonToken', is_optional=True,
                   description='An optional trailing semicolon.'),
         ]),

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
             Child('Accessor', kind='Syntax', is_optional=True,
                   node_choices=[
                      Child('Accessors', kind='AccessorBlock'),
                      Child('Getter', kind='CodeBlock')]),
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
                      'get', 'set', 'didSet', 'willSet', 'unsafeAddress', 
                      'addressWithOwner', 'addressWithNativeOwner', 
                      'unsafeMutableAddress', 
                      'mutableAddressWithOwner', 
                      'mutableAddressWithNativeOwner', 
                      '_read', '_modify'
                   ]),
             Child('Parameter', kind='AccessorParameter', is_optional=True),
             Child('Body', kind='CodeBlock', is_optional=True),
         ]),

    Node('AccessorList', kind="SyntaxCollection", element='AccessorDecl'),

    Node('AccessorBlock', kind="Syntax", traits=['Braced'],
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Accessors', kind='AccessorList'),
             Child('RightBrace', kind='RightBraceToken'),
         ]),

    # Pattern: Type = Value { get {} },
    Node('PatternBinding', kind="Syntax",
         traits=['WithTrailingComma'],
         children=[
             Child('Pattern', kind='Pattern'),
             Child('TypeAnnotation', kind='TypeAnnotation', is_optional=True),
             Child('Initializer', kind='InitializerClause', is_optional=True),
             Child('Accessor', kind='Syntax', is_optional=True,
                   node_choices=[
                      Child('Accessors', kind='AccessorBlock'),
                      Child('Getter', kind='CodeBlock')]),
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

    # operator-decl -> attribute? modifiers? 'operator' operator 
    Node('OperatorDecl', kind='Decl', traits=['IdentifiedDecl'],
         description='A Swift `operator` declaration.',
         children=[
             Child('Attributes', kind='AttributeList', is_optional=True,
                   description='''
                   The attributes applied to the 'operator' declaration.
                   '''),
             Child('Modifiers', kind='ModifierList', is_optional=True,
                   classification='Attribute',
                   description='''
                   The declaration modifiers applied to the 'operator'
                   declaration.
                   '''),
             Child('OperatorKeyword', kind='OperatorToken'),
             Child('Identifier', kind='Token',
                   token_choices=[
                       'UnspacedBinaryOperatorToken',
                       'SpacedBinaryOperatorToken',
                       'PrefixOperatorToken',
                       'PostfixOperatorToken',
                   ]),
             Child('OperatorPrecedenceAndTypes', kind='OperatorPrecedenceAndTypes',
                   description='''
                   Optionally specify a precedence group and designated types.
                   ''',
                   is_optional=True),
         ]),

    Node('IdentifierList', kind='SyntaxCollection',
         element='IdentifierToken'),

    # infix-operator-group -> ':' identifier ','? identifier?
    Node('OperatorPrecedenceAndTypes', kind='Syntax',
         description='''
         A clause to specify precedence group in infix operator declarations, and designated types in any operator declaration.
         ''',
         children=[
             Child('Colon', kind='ColonToken'),
             Child('PrecedenceGroupAndDesignatedTypes', kind='IdentifierList',
                   description='''
                   The precedence group and designated types for this operator
                   '''),
         ]),

    # precedence-group-decl -> attributes? modifiers? 'precedencegroup'
    #                            identifier '{' precedence-group-attribute-list
    #                            '}'
    Node('PrecedenceGroupDecl', kind='Decl', traits=['IdentifiedDecl'],
         description='A Swift `precedencegroup` declaration.',
         children=[
             Child('Attributes', kind='AttributeList', is_optional=True,
                   description='''
                   The attributes applied to the 'precedencegroup' declaration.
                   '''),
             Child('Modifiers', kind='ModifierList', is_optional=True,
                   description='''
                   The declaration modifiers applied to the 'precedencegroup'
                   declaration.
                   '''),
             Child('PrecedencegroupKeyword', kind='PrecedencegroupToken'),
             Child('Identifier', kind='IdentifierToken',
                   description='''
                   The name of this precedence group.
                   '''),
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('GroupAttributes', kind='PrecedenceGroupAttributeList',
                   description='''
                   The characteristics of this precedence group.
                   '''),
             Child('RightBrace', kind='RightBraceToken'),
         ]),

    # precedence-group-attribute-list ->
    #     (precedence-group-relation | precedence-group-assignment |
    #      precedence-group-associativity )*
    Node('PrecedenceGroupAttributeList', kind='SyntaxCollection',
         element='Syntax',
         element_choices=[
             'PrecedenceGroupRelation',
             'PrecedenceGroupAssignment',
             'PrecedenceGroupAssociativity'
         ]),

    # precedence-group-relation ->
    #     ('higherThan' | 'lowerThan') ':' precedence-group-name-list
    Node('PrecedenceGroupRelation', kind='Syntax',
         description='''
         Specify the new precedence group's relation to existing precedence
         groups.
         ''',
         children=[
             Child('HigherThanOrLowerThan', kind='IdentifierToken', 
                   classification='Keyword',
                   text_choices=[
                      'higherThan', 'lowerThan',
                   ],
                   description='''
                   The relation to specified other precedence groups.
                   '''),
             Child('Colon', kind='ColonToken'),
             Child('OtherNames', kind='PrecedenceGroupNameList',
                   description='''
                   The name of other precedence group to which this precedence
                   group relates.
                   '''),
         ]),

    # precedence-group-name-list ->
    #    identifier (',' identifier)*
    Node('PrecedenceGroupNameList', kind='SyntaxCollection',
         element='PrecedenceGroupNameElement'),
    Node('PrecedenceGroupNameElement', kind='Syntax',
         children=[
             Child('Name', kind='IdentifierToken'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # precedence-group-assignment ->
    #     'assignment' ':' ('true' | 'false')
    Node('PrecedenceGroupAssignment', kind='Syntax',
         description='''
         Specifies the precedence of an operator when used in an operation
         that includes optional chaining.
         ''',
         children=[
             Child('AssignmentKeyword', kind='IdentifierToken',
                   text_choices=['assignment']),
             Child('Colon', kind='ColonToken'),
             Child('Flag', kind='Token',
                   token_choices=[
                       'TrueToken',
                       'FalseToken',
                   ],
                   description='''
                   When true, an operator in the corresponding precedence group
                   uses the same grouping rules during optional chaining as the
                   assignment operators from the standard library. Otherwise,
                   operators in the precedence group follows the same optional
                   chaining rules as operators that don't perform assignment.
                   '''),
         ]),

    # precedence-group-associativity ->
    #     'associativity' ':' ('left' | 'right' | 'none')
    Node('PrecedenceGroupAssociativity', kind='Syntax',
         description='''
         Specifies how a sequence of operators with the same precedence level
         are grouped together in the absence of grouping parentheses.
         ''',
         children=[
             Child('AssociativityKeyword', kind='IdentifierToken', 
                   classification='Keyword', text_choices=['associativity']),
             Child('Colon', kind='ColonToken'),
             Child('Value', kind='IdentifierToken',
                   text_choices=['left', 'right', 'none'],
                   description='''
                   Operators that are `left`-associative group left-to-right.
                   Operators that are `right`-associative group right-to-left.
                   Operators that are specified with an associativity of `none`
                   don't associate at all
                   '''),
         ]),
]
