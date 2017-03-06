# Swift Syntax Library Implementation Status

## Represented Grammar Productions

Include the following in each entry:

- Grammar production(s)
  - See [Summary of the Swift Grammar](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/zzSummaryOfTheGrammar.html)
- C++ types representing the productions
- Testing status
  - With APIs
  - Make APIs
  - Builder APIs (if applicable)
- SR links
  - Related lib/AST changes

### Declarations

- declaration-modifiers
  - `DeclModifierListSyntax`

- declaration-modifier
  - `DeclModifierSyntax`

- struct-declaration
  - `StructDeclSyntax`

- typealias-assignment  
  typealias-declaration
  - `TypeAliasDeclSyntax`

- class-body  
  class-members

  - `DeclMembersSyntax`
  - `ClassDeclSyntax` used for the `{` `}` braces.

- extension-body
  - `DeclMembersSyntax`

- protocol-body
  - `DeclMembersSyntax`

- struct-body  
  struct-members
  - `DeclMembersSyntax`
  - `StructDeclSyntax` used for the `{` `}` braces.

- function-declaration
  - `FunctionDeclSyntax`

- function-body
  - `CodeBlockSyntax`

- function-result
  - `TypeSyntax`

- function-signature
  - `FunctionSignatureSyntax`

- parameter-clause
  - `FunctionParameterClauseSyntax`

- parameter-list
  - `FunctionParameterListSyntax`

- parameter
  - `FunctionParameterSyntax`

### Statements

- statement
  - `StmtSyntax` (Abstract base class)

- statements
  - `StmtListSyntax`

- code-block
  - `CodeBlockSyntax`

- fallthrough-statement
  - `FallthroughStmtSyntax`

- break-statement
  - `BreakStmtSyntax`

- continue-statement
  - `ContinueStmtSyntax`

- return-statement
  - `ReturnStmtSyntax`

### Expressions

- binary-literal
- decimal-literal
- hexadecimal-literal
- integer-literal
- octal-literal
  - `IntegerLiteralExprSyntax`

- function-call-argument
  - `FunctionCallArgumentSyntax`

- function-call-argument-list
  - `FunctionCallArgumentListSyntax`

- function-call-expression
- function-call-argument-clause
  - `FunctionCallExprSyntax`

### Types

- type
- type-annotation
  - `TypeSyntax` (Abstract base class)

- protocol-identifier
- type-identifier
  - `TypeIdentifierSyntax`

- optional-type
  - `OptionalTypeSyntax`

- implicitly-unwrapped-optional-type
  - `ImplicitlyUnwrappedOptionalTypeSyntax`

- array-type
  - `ArrayTypeSyntax`

- dictionary-type
  - `DictionaryTypeSyntax`

- function-type
- function-type-argument
- function-type-argument-clause
  - `FunctionTypeSyntax`

- function-type-argument-list
  - `TypeArgumentListSyntax`

- metatype-type
  - `MetatypeTypeSyntax`

- tuple-type
  - `TupleTypeSyntax`

- tuple-type-element
  - `TupleTypeElementSyntax`

- tuple-type-element-list
  - `TupleTypeElementListSyntax`

### Type Attributes

- attribute
- attribute-argument-clause
  - `TypeAttributeSyntax`

- attributes
  - `TypeAttributesSyntax`

- balanced-token
  - `BalancedTokenSyntax`

- balanced-tokens
  - `BalancedTokensSyntax`

### Generics

- generic-argument-clause
  - `GenericArgumentClauseSyntax`

- generic-argument-list
  - `GenericArgumentListSyntax`

- generic-parameter-clause
  - `GenericParameterClauseSyntax`

- generic-parameter
  - `GenericParameterSyntax`

- generic-parameter-list
  - `GenericParameterListSyntax`

- conformance-requirement
  - `ConformanceRequirementSyntax`

- same-type-requirement
  - `SameTypeRequirementSyntax`

- generic-where-clause
  - `GenericWhereClauseSyntax`

- requirement-list
  - `GenericRequirementListSyntax`

### Identifiers and Terminal Tokens

- access-level-modifier
- argument-label
- attribute-name
- boolean-literal
- class-name
- closure-parameter-name
- element-name
- enum-case-name
- enum-name
- external-parameter-name
- function-name
- identifier-pattern
- import-kind
- import-path-identifier
- label-name
- local-parameter-name
- mutation-modifier
- platform-name
- precedence-group-name
- protocol-name
- sign
- struct-name
- type-name
- typealias-name
- variable-name
  - identifier
    - `TokenSyntax`

## Unrepresented Grammar Productions

These are categorized somewhat by difficulty and priority.

### Easy

- array-literal
- array-literal-items
- as-pattern
- case-condition
- case-label
- dynamic-type-expression
- floating-point-literal
- forced-value-expression
- identifier-list
- implicit-member-expression
- import-path
- in-out-expression
- interpolated-text
- interpolated-text-item
- is-pattern
- key-path-expression
- line-control-statement
- optional-chaining-expression
- optional-pattern
- parenthesized-expression
- platform-condition
- platform-version
- postfix-operator-declaration
- precedence-group-assignment
- precedence-group-associativity
- precedence-group-names
- statement-label
- static-string-literal
- swift-version
- throw-statement
- value-binding-pattern
- where-clause
- dictionary-literal
  - dictionary-literal-items
    - dictionary-literal-item
- capture-list
    - capture-list-items
      - capture-list-item
- defer-statement

### Medium

- else-directive-clause
- elseif-directive-clauses
  - elseif-directive-clause
- precedence-group-declaration
- precedence-group-relation
- expression-list
- availability-condition
  - availability-arguments
    - availability-argument
- switch-cases
  - switch-case
- constant-declaration
- catch-clauses
  - catch-clause
- variable-declaration
- do-statement
- for-in-statement
- guard-statement
- case-item-list
- import-declaration
- if-directive-clause
- if-statement
  - else-clause
- protocol-associated-type-declaration
- repeat-while-statement
- while-statement
- tuple-expression
  - tuple-element-list
    - tuple-element
- tuple-pattern
  - tuple-pattern-element-list
    - tuple-pattern-element
- switch-statement
- explicit-member-expression
- optional-binding-condition
- operator-declaration
- selector-expression
- protocol-composition-type
- conditional-operator
- deinitializer-declaration
- didSet-clause
- willSet-clause
- pattern-initializer-list
  - pattern-initializer
- prefix-expression
- prefix-operator-declaration
- infix-operator-declaration
  - infix-operator-group
- binary-expression

### Hard

- protocol-declaration
- closure-expression
  - closure-signature
    - closure-parameter-clause
      - closure-parameter-list
        - closure-parameter
- extension-declaration
- enum-declaration
- class-declaration
- getter-setter-block
  - getter-setter-keyword-block
  - getter-keyword-clause
    - getter-clause
  - setter-keyword-clause
    - setter-clause
      - setter-name
- subscript-declaration
- enum-case-pattern
- initializer-declaration
  - initializer-head
- interpolated-string-literal
- conditional-compilation-block

## Trivial and Intermediate Grammar Productions

- binary-expressions
- binary-operator
- compilation-condition
- capture-specifier
- precedence-group-attributes
- precedence-group-attribute
- prefix-operator
- type-casting-operator
- willSet-didSet-block
- architecture
- string-literal
- argument-names
- array-literal-item
- type-casting-pattern
- assignment-operator
- expression-pattern
- binary-digit
- binary-literal-character
- binary-literal-characters
- branch-statement
- class-member
- class-requirement
- condition
- condition-list
- compiler-control-statement
- control-transfer-statement
- decimal-digit
- decimal-digits
- decimal-exponent
- decimal-fraction
- decimal-literal-character
- decimal-literal-characters
- default-argument-clause
- default-label
- dot-operator-character
- dot-operator-characters
- dot-operator-head
- else-directive
- elseif-directive
- endif-directive
- escaped-character
- expression
- extension-member
- file-name
- floating-point-e
- floating-point-p
- function-head
- hexadecimal-digit
- hexadecimal-exponent
- hexadecimal-fraction
- identifier-character
- identifier-characters
- identifier-head
- if-directive
- where-expression
- implicit-parameter-name
- initializer
- initializer-body
- initializer-expression
- labeled-statement (TODO: Put in loop-, if-, switch-, do-statement layout)
- line-number
- literal
- literal-expression
- loop-statement
- nil-literal
- numeric-literal
- octal-digit
- octal-literal-character
- octal-literal-characters
- operating-system
- operator
- operator-character
- operator-characters
- operator-head
- pattern
- postfix-expression
- postfix-operator
- postfix-self-expression
- protocol-composition-continuation
- protocol-initializer-declaration
- protocol-member
- protocol-member-declaration
- protocol-members
- extension-members
- protocol-method-declaration
- protocol-property-declaration
- protocol-subscript-declaration
- quoted-text
- quoted-text-item
- raw-value-assignment
- raw-value-literal
- raw-value-style-enum
- raw-value-style-enum-case
- raw-value-style-enum-case-clause
- raw-value-style-enum-case-list
- playground-literal
- raw-value-style-enum-member
- raw-value-style-enum-members
- requirement
- self-expression
- self-initializer-expression
- self-method-expression
- self-subscript-expression
- struct-member
- subscript-expression
- subscript-head
- subscript-result
- superclass-expression
- superclass-initializer-expression
- superclass-method-expression
- superclass-subscript-expression
- top-level-declaration
- try-operator
- type-inheritance-clause
- type-inheritance-list
- unicode-scalar-digits
- union-style-enum
- union-style-enum-case
- union-style-enum-case-clause
- union-style-enum-case-list
- union-style-enum-member
- union-style-enum-members
- variable-declaration-head
- wildcard-expression
- wildcard-pattern
- primary-expression
- generic-argument
