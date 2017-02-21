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

- integer-literal
  - `IntegerLiteralExprSyntax`

### Types

- type
  - `TypeSyntax` (Abstract base class)

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
  - `FunctionTypeSyntax`

- metatype-type
  - `MetatypeTypeSyntax`

### Type Attributes

- attribute
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

- same-type-requirement
  - `SameTypeRequirementSyntax`

- generic-where-clause
  - `GenericWhereClauseSyntax`

### Identifiers and Terminal Tokens

- access-level-modifier
- attribute-name
- class-name
- closure-parameter-name
- element-name
- enum-case-name
- enum-name
- external-parameter-name
- import-kind
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

- argument-label
- argument-name
- argument-names
- array-literal
- array-literal-item
- array-literal-items
- as-pattern
- assignment-operator
- attribute-argument-clause
- availability-argument
- availability-arguments
- availability-condition
- binary-expression
- binary-expressions
- binary-literal
- binary-operator
- boolean-literal
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
- class-member
- class-requirement
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
- conformance-requirement
- constant-declaration
- control-transfer-statement
- decimal-exponent
- decimal-fraction
- decimal-literal
- declaration-modifier
- declaration-modifiers
- default-argument-clause
- default-label
- defer-statement
- deinitializer-declaration
- dictionary-literal
- dictionary-literal-item
- dictionary-literal-items
- didSet-clause
- do-statement
- dynamic-type-expression
- else-clause
- else-directive
- else-directive-clause
- elseif-directive
- elseif-directive-clause
- elseif-directive-clauses
- endif-directive
- enum-case-pattern
- enum-declaration
- escaped-character
- explicit-member-expression
- expression
- expression-list
- expression-pattern
- extension-declaration
- extension-member
- extension-members
- floating-point-e
- floating-point-literal
- floating-point-p
- for-in-statement
- forced-value-expression
- function-body
- function-call-argument
- function-call-argument-clause
- function-call-argument-list
- function-call-expression
- function-declaration
- function-head
- function-name
- function-result
- function-signature
- function-type-argument
- function-type-argument-clause
- function-type-argument-list
- getter-clause
- getter-keyword-clause
- getter-setter-block
- getter-setter-keyword-block
- guard-statement
- hexadecimal-exponent
- hexadecimal-fraction
- hexadecimal-literal
- hexadecimal-literal-character
- hexadecimal-literal-characters
- identifier-list
- identifier-pattern
- if-directive
- if-directive-clause
- if-statement
- implicit-member-expression
- import-declaration
- import-path
- import-path-identifier
- in-out-expression
- infix-operator-declaration
- infix-operator-group
- initializer
- initializer-body
- initializer-declaration
- initializer-expression
- initializer-head
- interpolated-string-literal
- interpolated-text
- interpolated-text-item
- is-pattern
- key-path-expression
- labeled-statement
- line-control-statement
- line-number
- literal
- literal-expression
- loop-statement
- nil-literal
- numeric-literal
- octal-literal
- operator
- operator-character
- operator-characters
- operator-declaration
- operator-head
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
- postfix-expression
- postfix-operator
- postfix-operator-declaration
- postfix-self-expression
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
- protocol-composition-continuation
- protocol-composition-type
- protocol-declaration
- protocol-identifier
- protocol-initializer-declaration
- protocol-member
- protocol-member-declaration
- protocol-members
- protocol-method-declaration
- protocol-property-declaration
- protocol-subscript-declaration
- raw-value-assignment
- raw-value-literal
- raw-value-style-enum
- raw-value-style-enum-case
- raw-value-style-enum-case-clause
- raw-value-style-enum-case-list
- raw-value-style-enum-member
- raw-value-style-enum-members
- repeat-while-statement
- requirement
- requirement-list
- selector-expression
- self-expression
- self-initializer-expression
- self-method-expression
- self-subscript-expression
- setter-clause
- setter-keyword-clause
- setter-name
- statement-label
- static-string-literal
- string-literal
- struct-member
- subscript-declaration
- subscript-expression
- subscript-head
- subscript-result
- superclass-expression
- superclass-initializer-expression
- superclass-method-expression
- superclass-subscript-expression
- swift-version
- switch-case
- switch-cases
- switch-statement
- throw-statement
- top-level-declaration
- try-operator
- tuple-element
- tuple-element-list
- tuple-expression
- tuple-pattern
- tuple-pattern-element
- tuple-pattern-element-list
- tuple-type
- tuple-type-element
- tuple-type-element-list
- type-annotation
- type-casting-operator
- type-casting-pattern
- type-inheritance-clause
- type-inheritance-list
- unicode-scalar-digits
- union-style-enum
- union-style-enum-case
- union-style-enum-case-clause
- union-style-enum-case-list
- union-style-enum-member
- union-style-enum-members
- value-binding-pattern
- variable-declaration
- variable-declaration-head
- where-clause
- where-expression
- while-statement
- wildcard-expression
- wildcard-pattern
- willSet-clause
- willSet-didSet-block

## Trivial Grammar Productions

- architecture
- binary-digit
- binary-literal-character
- binary-literal-characters
- decimal-digit
- decimal-digits
- decimal-literal-character
- decimal-literal-characters
- dot-operator-character
- dot-operator-characters
- dot-operator-head
- hexadecimal-digit
- identifier-character
- identifier-characters
- identifier-head
- implicit-parameter-name
- file-name
- octal-digit
- octal-literal-character
- octal-literal-characters
- operating-system
- quoted-text
- quoted-text-item

## Intermediate Grammar Productions

These productions don't need to be represented directly in a class hierarchy.

- generic-argument
