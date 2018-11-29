from Child import Child
from Node import Node  # noqa: I201

ATTRIBUTE_NODES = [
    # token-list -> token? token-list?
    Node('TokenList', kind='SyntaxCollection',
         element='Token'),

    # token-list -> token token-list?
    Node('NonEmptyTokenList', kind='SyntaxCollection',
         element='Token', omit_when_empty=True),

    # attribute -> '@' identifier '('? 
    #              ( identifier 
    #                | string-literal 
    #                | integer-literal
    #                | availability-spec-list
    #                | specialize-attr-spec-list
    #                | implements-attr-arguments
    #                | differentiable-attr-arguments
    #                | named-attribute-string-argument
    #              )? ')'?
    Node('Attribute', kind='Syntax',
         description='''
         An `@` attribute.
         ''',
         children=[
             Child('AtSignToken', kind='AtSignToken', 
                   description='The `@` sign.'),
             Child('AttributeName', kind='Token', classification='Attribute',
                   description='The name of the attribute.'),
             Child('LeftParen', kind='LeftParenToken', is_optional=True,
                   description='''
                   If the attribute takes arguments, the opening parenthesis.
                   '''),
             Child('Argument', kind='Syntax', is_optional=True,
                   node_choices=[
                       Child('Identifier', kind='IdentifierToken'),
                       Child('String', kind='StringLiteralToken'),
                       Child('Integer', kind='IntegerLiteralToken'),
                       Child('Availability', kind='AvailabilitySpecList'),
                       Child('SpecializeArguments', 
                             kind='SpecializeAttributeSpecList'),
                       Child('ObjCName', kind='ObjCSelector'),
                       Child('ImplementsArguments', 
                             kind='ImplementsAttributeArguments'),
                       Child('DifferentiableArguments',
                             kind='DifferentiableAttributeArguments'),
                       Child('NamedAttributeString',
                             kind='NamedAttributeStringArgument'),
                   ], description='''
                   The arguments of the attribute. In case the attribute  \
                   takes multiple arguments, they are gather in the \
                   appropriate takes first.
                   '''),
             Child('RightParen', kind='RightParenToken', is_optional=True,
                   description='''
                   If the attribute takes arguments, the closing parenthesis.
                   '''),
             # TokenList to gather remaining tokens of invalid attributes
             # FIXME: Remove this recovery option entirely
             Child('TokenList', kind='TokenList', is_optional=True),
         ]),

    # attribute-list -> attribute attribute-list?
    Node('AttributeList', kind='SyntaxCollection',
         element='Attribute'),

    # The argument of '@_specialize(...)'
    # specialize-attr-spec-list -> labeled-specialize-entry 
    #                                  specialize-spec-attr-list?
    #                            | generic-where-clause 
    #                                  specialize-spec-attr-list?
    Node('SpecializeAttributeSpecList', kind='SyntaxCollection',
         description='''
         A collection of arguments for the `@_specialize` attribute
         ''',
         element='Syntax',
         element_choices=[
             'LabeledSpecializeEntry',
             'GenericWhereClause',
         ]),

    # Representation of e.g. 'exported: true,'
    # labeled-specialize-entry -> identifier ':' token ','?
    Node('LabeledSpecializeEntry', kind='Syntax',
         description='''
         A labeled argument for the `@_specialize` attribute like \
         `exported: true`
         ''',
         traits=['WithTrailingComma'],
         children=[
             Child('Label', kind='IdentifierToken', 
                   description='The label of the argument'),
             Child('Colon', kind='ColonToken', 
                   description='The colon separating the label and the value'),
             Child('Value', kind='Token', 
                   description='The value for this argument'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True, description='''
                   A trailing comma if this argument is followed by another one
                   '''),
         ]),
    # The argument of '@_dynamic_replacement(for:)' or '@_private(sourceFile:)'
    # named-attribute-string-arg -> 'name': string-literal
    Node('NamedAttributeStringArgument', kind='Syntax',
         description='''
         The argument for the `@_dynamic_replacement` or `@_private` \
         attribute of the form `for: "function()"` or `sourceFile: \
         "Src.swift"`
         ''',
         children=[
             Child('NameTok', kind='Token',
                   description='The label of the argument'),
             Child('Colon', kind='ColonToken',
                   description='The colon separating the label and the value'),
             Child('StringOrDeclname', kind='Syntax', node_choices=[
                 Child('String', kind='StringLiteralToken'),
                 Child('Declname', kind='DeclName'),
             ]),
         ]),
    Node('DeclName', kind='Syntax', children=[
         Child('DeclBaseName', kind='Syntax', description='''
               The base name of the protocol\'s requirement.
               ''',
               node_choices=[
                   Child('Identifier', kind='IdentifierToken'),
                   Child('Operator', kind='PrefixOperatorToken'),
               ]),
         Child('DeclNameArguments', kind='DeclNameArguments',
               is_optional=True, description='''
               The argument labels of the protocol\'s requirement if it \
               is a function requirement.
               '''),
         ]),
    # The argument of '@_implements(...)'
    # implements-attr-arguments -> simple-type-identifier ',' 
    #                              (identifier | operator) decl-name-arguments
    Node('ImplementsAttributeArguments', kind='Syntax',
         description='''
         The arguments for the `@_implements` attribute of the form \
         `Type, methodName(arg1Label:arg2Label:)`
         ''',
         children=[
             Child('Type', kind='SimpleTypeIdentifier', description='''
                   The type for which the method with this attribute \
                   implements a requirement.
                   '''),
             Child('Comma', kind='CommaToken', 
                   description='''
                   The comma separating the type and method name
                   '''),
             Child('DeclBaseName', kind='Syntax', description='''
                   The base name of the protocol\'s requirement.
                   ''',
                   node_choices=[
                       Child('Identifier', kind='IdentifierToken'),
                       Child('Operator', kind='PrefixOperatorToken'),
                   ]),
             Child('DeclNameArguments', kind='DeclNameArguments', 
                   is_optional=True, description='''
                   The argument labels of the protocol\'s requirement if it \
                   is a function requirement.
                   '''),
         ]),

    # SWIFT_ENABLE_TENSORFLOW
    # The argument of '@differentiable(...)'.
    # differentiable-attr-arguments ->
    #     ('forward' | 'reverse')
    #     (',' differentiable-attr-parameters)?
    #     (',' 'primal' ':' differentiable-attr-func-specifier)?
    #     (',' 'adjoint' ':' differentiable-attr-func-specifier)?
    #     generic-where-clause?
    Node('DifferentiableAttributeArguments', kind='Syntax',
         description='''
         The arguments for the `@differentiable` attribute: differentiation \
         mode ('forward' or 'reverse'), an optional differentiation parameter \
         list, and functions associated with the differentiation mode.
         ''',
         children=[
             Child('AutoDiffMode', kind='IdentifierToken',
                   text_choices=['forward', 'reverse'],
                   description='The mode of automatic differentiation.'),
             Child('DiffParams', kind='DifferentiableAttributeDiffParams',
                   is_optional=True),
             Child('Primal', kind='DifferentiableAttributeFuncSpecifier',
                   is_optional=True),
             Child('AdjointOrTangent',
                   kind='DifferentiableAttributeFuncSpecifier',
                   is_optional=True),
             Child('WhereClause', kind='GenericWhereClause', is_optional=True),
         ]),

    # differentiable-attr-parameters ->
    #     'wrt' ':' '(' differentiation-parameter-list ')'
    Node('DifferentiableAttributeDiffParams', kind='Syntax',
         description='The parameters to differentiate with respect to.',
         children=[
             Child('LeadingComma', kind='CommaToken', description='''
                   The comma separating the differentiation mode and the \
                   differentiation parameter list.
                   '''),
             Child('WrtLabel', kind='IdentifierToken',
                   text_choices=['wrt'], description='The "wrt" label.'),
             Child('Colon', kind='ColonToken', description='''
                   The colon separating "wrt" and the parameter list.
                   '''),
             Child('LeftParen', kind='LeftParenToken'),
             Child('DiffParams', kind='DifferentiableAttributeDiffParamList',
                   description='The parameters for differentiation.'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # differentiable-attr-diff-param-list ->
    #     differentiable-attr-diff-param differentiable-attr-diff-param-list?
    Node('DifferentiableAttributeDiffParamList', kind='SyntaxCollection',
         element='DifferentiableAttributeDiffParam'),

    # differentiable-attr-diff-param ->
    #     ('self' | differentiation-index-parameter) ','?
    Node('DifferentiableAttributeDiffParam', kind='Syntax',
         description='''
         A differentiation parameter: either the "self" identifier or a period \
         followed by an unsigned integer (e.g. `.0`).
         ''',
         traits=['WithTrailingComma'],
         children=[
             Child('Parameter', kind='Syntax',
                   node_choices=[
                       Child('Self', kind='SelfToken'),
                       Child('Index', kind='DifferentiationIndexParam'),
                   ]),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # differentiation-index-param -> '.' integer-literal
    Node('DifferentiationIndexParam', kind='Syntax',
         description='''
         A differentiation index parameter: a period followed by an unsigned \
         integer (e.g. `.0`)
         ''',
         children=[
             Child('PrefixPeriod', kind='PrefixPeriodToken'),
             Child('IntegerLiteral', kind='IntegerLiteralToken'),
         ]),

    # differentiation-func-specifier ->
    #     ('primal' | 'adjoint' | 'tangent') ':' decl-name
    # decl-name -> (identifier | operator) decl-name-arguments?
    Node('DifferentiableAttributeFuncSpecifier', kind='Syntax',
         description='''
         A function specifier, consisting of an identifier, colon, and a \
         function declaration name (e.g. `adjoint: foo(_:_:)`.
         ''',
         children=[
             Child('LeadingComma', kind='CommaToken',
                   description='The leading comma of a function specifier.'),
             Child('Label', kind='IdentifierToken',
                   text_choices=['primal', 'adjoint', 'tangent']),
             Child('Colon', kind='ColonToken'),
             Child('DeclBaseName', kind='Syntax', description='''
                   The base name of the referenced function.
                   ''',
                   node_choices=[
                       Child('Identifier', kind='IdentifierToken'),
                       Child('Operator', kind='PrefixOperatorToken'),
                   ]),
             Child('DeclNameArguments', kind='DeclNameArguments',
                   is_optional=True, description='''
                   The argument labels of the referenced function, optionally \
                   specified.
                   '''),
         ]),

    # objc-selector-piece -> identifier? ':'?
    Node('ObjCSelectorPiece', kind='Syntax',
         description='''
         A piece of an Objective-C selector. Either consisiting of just an \
         identifier for a nullary selector, an identifier and a colon for a \
         labeled argument or just a colon for an unlabeled argument
         ''',
         children=[
             Child('Name', kind='IdentifierToken', is_optional=True),
             Child('Colon', kind='ColonToken', is_optional=True),
         ]),

    # objc-selector -> objc-selector-piece objc-selector?
    Node('ObjCSelector', kind='SyntaxCollection', element='ObjCSelectorPiece')
]
