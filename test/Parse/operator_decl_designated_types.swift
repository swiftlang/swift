// RUN: %target-typecheck-verify-swift -enable-operator-designated-types

precedencegroup LowPrecedence {
  associativity: right
}

precedencegroup MediumPrecedence {
  associativity: left
  higherThan: LowPrecedence
}

protocol PrefixMagicOperatorProtocol {
}

protocol PostfixMagicOperatorProtocol {
}

protocol InfixMagicOperatorProtocol {
}

prefix operator ^^ : PrefixMagicOperatorProtocol
infix operator  <*< : MediumPrecedence, InfixMagicOperatorProtocol
postfix operator ^^ : PostfixMagicOperatorProtocol

infix operator ^*^
prefix operator *^^
postfix operator ^^*

infix operator **>> : UndeclaredPrecedence
// expected-error@-1 {{unknown precedence group 'UndeclaredPrecedence'}}
// expected-error@-2 {{use of undeclared type 'UndeclaredPrecedence'}}

infix operator **+> : MediumPrecedence, UndeclaredProtocol
// expected-error@-1 {{use of undeclared type 'UndeclaredProtocol'}}

prefix operator *+*> : MediumPrecedence
// expected-error@-1 {{use of undeclared type 'MediumPrecedence'}}

postfix operator ++*> : MediumPrecedence
// expected-error@-1 {{use of undeclared type 'MediumPrecedence'}}

prefix operator *++> : UndeclaredProtocol
// expected-error@-1 {{use of undeclared type 'UndeclaredProtocol'}}
postfix operator +*+> : UndeclaredProtocol
// expected-error@-1 {{use of undeclared type 'UndeclaredProtocol'}}

struct Struct {}
class Class {}
infix operator *>*> : Struct
infix operator >**> : Class

prefix operator **>> : Struct
prefix operator *>*> : Class

postfix operator >*>* : Struct
postfix operator >>** : Class

infix operator  <*<<< : MediumPrecedence, &
// expected-error@-1 {{expected designated type in operator declaration}}

infix operator **^^ : MediumPrecedence // expected-note {{previous operator declaration here}}
infix operator **^^ : InfixMagicOperatorProtocol // expected-error {{operator redeclared}}
