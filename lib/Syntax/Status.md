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

- argument-names
- array-literal
- array-literal-items
- as-pattern
- availability-argument
- availability-arguments
- availability-condition
- binary-expression
- binary-expressions
- binary-operator
- branch-statement
- capture-list
- capture-list-item
- capture-list-items
- capture-specifier
- case-condition
- case-item-list
- case-label
- catch-clause
- catch-clauses
- class-declaration
- closure-expression
- closure-parameter
- closure-parameter-clause
- closure-parameter-list
- closure-signature
- compilation-condition
- compiler-control-statement
- condition
- condition-list
- conditional-compilation-block
- conditional-operator
- constant-declaration
- declaration-modifier
- declaration-modifiers
- defer-statement
- deinitializer-declaration
- dictionary-literal
- dictionary-literal-item
- dictionary-literal-items
- didSet-clause
- do-statement
- dynamic-type-expression
- else-clause
- else-directive-clause
- elseif-directive-clause
- elseif-directive-clauses
- enum-case-pattern
- enum-declaration
- explicit-member-expression
- expression-list
- expression-pattern
- extension-declaration
- extension-members
- floating-point-literal
- for-in-statement
- forced-value-expression
- function-body
- function-call-argument
- function-call-argument-clause
- function-call-argument-list
- function-call-expression
- function-declaration
- function-result
- function-signature
- getter-clause
- getter-keyword-clause
- getter-setter-block
- getter-setter-keyword-block
- guard-statement
- identifier-list
- if-directive-clause
- if-statement
- implicit-member-expression
- import-declaration
- import-path
- in-out-expression
- infix-operator-declaration
- infix-operator-group
- initializer-declaration
- initializer-head
- interpolated-string-literal
- interpolated-text
- interpolated-text-item
- is-pattern
- key-path-expression
- line-control-statement
- operator-declaration
- optional-binding-condition
- optional-chaining-expression
- optional-pattern
- parameter
- parameter-clause
- parameter-list
- parenthesized-expression
- pattern
- pattern-initializer
- pattern-initializer-list
- platform-condition
- platform-version
- playground-literal
- postfix-operator-declaration
- precedence-group-assignment
- precedence-group-associativity
- precedence-group-attribute
- precedence-group-attributes
- precedence-group-declaration
- precedence-group-names
- precedence-group-relation
- prefix-expression
- prefix-operator
- prefix-operator-declaration
- primary-expression
- protocol-associated-type-declaration
- protocol-composition-type
- protocol-declaration
- repeat-while-statement
- selector-expression
- setter-clause
- setter-keyword-clause
- setter-name
- statement-label
- static-string-literal
- string-literal
- subscript-declaration
- swift-version
- switch-case
- switch-cases
- switch-statement
- throw-statement
- tuple-element
- tuple-element-list
- tuple-expression
- tuple-pattern
- tuple-pattern-element
- tuple-pattern-element-list
- tuple-type
- tuple-type-element
- tuple-type-element-list
- type-casting-operator
- type-casting-pattern
- value-binding-pattern
- variable-declaration
- where-clause
- where-expression
- while-statement
- willSet-clause
- willSet-didSet-block

## Trivial and Intermediate Grammar Productions

- architecture
- array-literal-item
- assignment-operator
- binary-digit
- binary-literal-character
- binary-literal-characters
- class-member
- class-requirement
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
- postfix-expression
- postfix-operator
- postfix-self-expression
- protocol-composition-continuation
- protocol-initializer-declaration
- protocol-member
- protocol-member-declaration
- protocol-members
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

## Intermediate Grammar Productions

These productions don't need to be represented directly in a class hierarchy.

- generic-argument
