// RUN: %target-typecheck-verify-swift -enable-operator-designated-protocols

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
// expected-error@-1 {{type 'Struct' unexpected; expected a protocol type}}

infix operator >**> : Class
// expected-error@-1 {{type 'Class' unexpected; expected a protocol type}}

prefix operator **>> : Struct
// expected-error@-1 {{type 'Struct' unexpected; expected a protocol type}}
prefix operator *>*> : Class
// expected-error@-1 {{type 'Class' unexpected; expected a protocol type}}

postfix operator >*>* : Struct
// expected-error@-1 {{type 'Struct' unexpected; expected a protocol type}}
postfix operator >>** : Class
// expected-error@-1 {{type 'Class' unexpected; expected a protocol type}}

infix operator  <*<<< : MediumPrecedence, &
// expected-error@-1 {{expected designated protocol in operator declaration}}

infix operator **^^ : MediumPrecedence // expected-note {{previous operator declaration here}}
infix operator **^^ : InfixMagicOperatorProtocol // expected-error {{operator redeclared}}
