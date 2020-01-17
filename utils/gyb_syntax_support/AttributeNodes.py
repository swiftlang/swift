from Child import Child
from Node import Node  # noqa: I201

ATTRIBUTE_NODES = [
    # token-list -> token? token-list?
    Node('TokenList', kind='SyntaxCollection',
         element='Token'),

    # token-list -> token token-list?
    Node('NonEmptyTokenList', kind='SyntaxCollection',
         element='Token', omit_when_empty=True),

    Node('CustomAttribute', kind='Syntax',
         description='''
         A custom `@` attribute.
         ''',
         children=[
             Child('AtSignToken', kind='AtSignToken',
                   description='The `@` sign.'),
             Child('AttributeName', kind='Type', classification='Attribute',
                   description='The name of the attribute.'),
             Child('LeftParen', kind='LeftParenToken',
                   is_optional=True),
             Child('ArgumentList', kind='TupleExprElementList',
                   collection_element_name='Argument', is_optional=True),
             Child('RightParen', kind='RightParenToken',
                   is_optional=True),
         ]),

    # attribute -> '@' identifier '('?
    #              ( identifier
    #                | string-literal
    #                | integer-literal
    #                | availability-spec-list
    #                | specialize-attr-spec-list
    #                | implements-attr-arguments
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
                       Child('DerivativeRegistrationArguments',
                             kind='DerivativeRegistrationAttributeArguments'),
                       Child('NamedAttributeString',
                             kind='NamedAttributeStringArgument'),
                   ], description='''
                   The arguments of the attribute. In case the attribute
                   takes multiple arguments, they are gather in the
                   appropriate takes first.
                   '''),
             Child('RightParen', kind='RightParenToken', is_optional=True,
                   description='''
                   If the attribute takes arguments, the closing parenthesis.
                   '''),
             # TokenList to gather remaining tokens of invalid attributes
             # FIXME: Remove this recovery option entirely
             Child('TokenList', kind='TokenList',
                   collection_element_name='Token', is_optional=True),
         ]),

    # attribute-list -> attribute attribute-list?
    Node('AttributeList', kind='SyntaxCollection',
         omit_when_empty=True,
         element='Syntax', element_name='Attribute',
         element_choices=[
             'Attribute',
             'CustomAttribute',
         ]),

    # The argument of '@_specialize(...)'
    # specialize-attr-spec-list -> labeled-specialize-entry
    #                                  specialize-spec-attr-list?
    #                            | generic-where-clause
    #                                  specialize-spec-attr-list?
    Node('SpecializeAttributeSpecList', kind='SyntaxCollection',
         description='''
         A collection of arguments for the `@_specialize` attribute
         ''',
         element='Syntax', element_name='SpecializeAttribute',
         element_choices=[
             'LabeledSpecializeEntry',
             'GenericWhereClause',
         ]),

    # Representation of e.g. 'exported: true,'
    # labeled-specialize-entry -> identifier ':' token ','?
    Node('LabeledSpecializeEntry', kind='Syntax',
         description='''
         A labeled argument for the `@_specialize` attribute like
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
         The argument for the `@_dynamic_replacement` or `@_private`
         attribute of the form `for: "function()"` or `sourceFile:
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
               The argument labels of the protocol\'s requirement if it
               is a function requirement.
               '''),
         ]),
    # The argument of '@_implements(...)'
    # implements-attr-arguments -> simple-type-identifier ','
    #                              (identifier | operator) decl-name-arguments
    Node('ImplementsAttributeArguments', kind='Syntax',
         description='''
         The arguments for the `@_implements` attribute of the form
         `Type, methodName(arg1Label:arg2Label:)`
         ''',
         children=[
             Child('Type', kind='SimpleTypeIdentifier', description='''
                   The type for which the method with this attribute
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
                   The argument labels of the protocol\'s requirement if it
                   is a function requirement.
                   '''),
         ]),

    # objc-selector-piece -> identifier? ':'?
    Node('ObjCSelectorPiece', kind='Syntax',
         description='''
         A piece of an Objective-C selector. Either consisiting of just an
         identifier for a nullary selector, an identifier and a colon for a
         labeled argument or just a colon for an unlabeled argument
         ''',
         children=[
             Child('Name', kind='IdentifierToken', is_optional=True),
             Child('Colon', kind='ColonToken', is_optional=True),
         ]),

    # objc-selector -> objc-selector-piece objc-selector?
    Node('ObjCSelector', kind='SyntaxCollection', element='ObjCSelectorPiece'),

    # The argument of '@differentiable(...)'.
    # differentiable-attr-arguments ->
    #     differentiation-params-clause? ','?
    #     differentiable-attr-func-specifier? # jvp
    #     differentiable-attr-func-specifier? # vjp
    #     generic-where-clause?
    Node('DifferentiableAttributeArguments', kind='Syntax',
         description='''
         The arguments for the `@differentiable` attribute: an optional
         differentiation parameter list and associated functions.
         ''',
         children=[
             Child('DiffParams', kind='DifferentiationParamsClause',
                   is_optional=True),
             Child('DiffParamsComma', kind='CommaToken', description='''
                   The comma following the differentiation parameters clause,
                   if it exists.
                   ''', is_optional=True),
             Child('MaybeJVP', kind='DifferentiableAttributeFuncSpecifier',
                   is_optional=True),
             Child('MaybeVJP', kind='DifferentiableAttributeFuncSpecifier',
                   is_optional=True),
             Child('WhereClause', kind='GenericWhereClause', is_optional=True),
         ]),

    # differentiation-params-clause ->
    #     'wrt' ':' (differentiation-param | differentiation-params)
    Node('DifferentiationParamsClause', kind='Syntax',
         description='A clause containing differentiation parameters.',
         children=[
             Child('WrtLabel', kind='IdentifierToken',
                   text_choices=['wrt'], description='The "wrt" label.'),
             Child('Colon', kind='ColonToken', description='''
                   The colon separating "wrt" and the parameter list.
                   '''),
             Child('Parameters', kind='Syntax',
                   node_choices=[
                       Child('Parameter', kind='DifferentiationParam'),
                       Child('ParameterList', kind='DifferentiationParams'),
                   ]),
         ]),

    # differentiation-params -> '(' differentiation-param-list ')'
    Node('DifferentiationParams', kind='Syntax',
         description='The differentiation parameters.',
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('DiffParams', kind='DifferentiationParamList',
                   collection_element_name='DifferentiationParam',
                   description='The parameters for differentiation.'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # differentiation-param-list ->
    #     differentiation-param differentiation-param-list?
    Node('DifferentiationParamList', kind='SyntaxCollection',
         element='DifferentiationParam'),

    # differentiation-param -> ('self' | identifer | integer-literal) ','?
    Node('DifferentiationParam', kind='Syntax',
         description='''
         A differentiation parameter: either the "self" identifier, a function
         parameter name, or a function parameter index.
         ''',
         traits=['WithTrailingComma'],
         children=[
             Child('Parameter', kind='Syntax',
                   node_choices=[
                       Child('Self', kind='SelfToken'),
                       Child('Name', kind='IdentifierToken'),
                       Child('Index', kind='IntegerLiteralToken'),
                   ]),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # differentiable-attr-func-specifier ->
    #     ('jvp' | 'vjp') ':' func-decl-name ','?
    Node('DifferentiableAttributeFuncSpecifier', kind='Syntax',
         description='''
         A function specifier, consisting of an identifier, colon, and a
         function declaration name (e.g. `vjp: foo(_:_:)`).
         ''',
         traits=['WithTrailingComma'],
         children=[
             Child('Label', kind='IdentifierToken',
                   text_choices=['jvp', 'vjp']),
             Child('Colon', kind='ColonToken'),
             Child('FunctionDeclName', kind='FunctionDeclName',
                   description='The referenced function name.'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # The argument of the derivative registration attribute
    # '@derivative(of: ...)' and the transpose registration attribute
    # '@transpose(of: ...)'.
    #
    # derivative-registration-attr-arguments ->
    #     'of' ':' func-decl-name ','? differentiation-params-clause?
    Node('DerivativeRegistrationAttributeArguments', kind='Syntax',
         description='''
         The arguments for the '@derivative(of:)' and '@transpose(of:)'
         attributes: the 'of:' label, the original declaration name, and an
         optional differentiation parameter list.
         ''',
         children=[
             Child('OfLabel', kind='IdentifierToken', text_choices=['of'],
                   description='The "of" label.'),
             Child('Colon', kind='ColonToken', description='''
                   The colon separating the "of" label and the original
                   declaration name.
                   '''),
             Child('OriginalDeclName', kind='QualifiedDeclName',
                   description='The referenced original declaration name.'),
             Child('Comma', kind='CommaToken', is_optional=True),
             Child('DiffParams', kind='DifferentiationParamsClause',
                   is_optional=True),
         ]),

    # An optionally qualified declaration name.
    # Currently used only for `@derivative` and `@transpose` attribute.
    # TODO(TF-1066): Use module qualified name syntax/parsing instead of custom
    # qualified name syntax/parsing.
    #
    # qualified-decl-name ->
    #     base-type? '.'? (identifier | operator) decl-name-arguments?
    # base-type ->
    #     member-type-identifier | base-type-identifier
    Node('QualifiedDeclName', kind='Syntax',
         description='''
         An optionally qualified function declaration name (e.g. `+(_:_:)`,
         `A.B.C.foo(_:_:)`).
         ''',
         children=[
             Child('BaseType', kind='Type', description='''
                   The base type of the qualified name, optionally specified.
                   ''', is_optional=True),
             Child('Dot', kind='Token',
                   token_choices=[
                       'PeriodToken', 'PrefixPeriodToken'
                   ], is_optional=True),
             Child('Name', kind='Token', description='''
                   The base name of the referenced function.
                   ''',
                   token_choices=[
                       'IdentifierToken',
                       'UnspacedBinaryOperatorToken',
                       'SpacedBinaryOperatorToken',
                       'PrefixOperatorToken',
                       'PostfixOperatorToken',
                   ]),
             Child('Arguments', kind='DeclNameArguments',
                   is_optional=True, description='''
                   The argument labels of the referenced function, optionally
                   specified.
                   '''),
         ]),

    # func-decl-name -> (identifier | operator) decl-name-arguments?
    # NOTE: This is duplicated with `DeclName` above. Change `DeclName`
    # description and use it if possible.
    Node('FunctionDeclName', kind='Syntax',
         description='A function declaration name (e.g. `foo(_:_:)`).',
         children=[
             Child('Name', kind='Syntax', description='''
                   The base name of the referenced function.
                   ''',
                   node_choices=[
                       Child('Identifier', kind='IdentifierToken'),
                       Child('PrefixOperator', kind='PrefixOperatorToken'),
                       Child('SpacedBinaryOperator',
                             kind='SpacedBinaryOperatorToken'),
                   ]),
             Child('Arguments', kind='DeclNameArguments',
                   is_optional=True, description='''
                   The argument labels of the referenced function, optionally
                   specified.
                   '''),
         ]),
]
