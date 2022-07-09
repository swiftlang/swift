// RUN: %target-typecheck-verify-swift -enable-operator-designated-types -verify-syntax-tree

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
// expected-warning@-1 {{designated types are no longer used by the compiler; please remove the designated type list from this operator declaration}} {{20-49=}}
infix operator  <*< : MediumPrecedence, InfixMagicOperatorProtocol
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{39-67=}}
postfix operator ^^ : PostfixMagicOperatorProtocol
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{21-51=}}

infix operator ^*^
prefix operator *^^
postfix operator ^^*

infix operator **>> : UndeclaredPrecedence
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{21-43=}}

infix operator **+> : MediumPrecedence, UndeclaredProtocol
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{39-59=}}

prefix operator *+*> : MediumPrecedence
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{22-40=}}

postfix operator ++*> : MediumPrecedence
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{23-41=}}

prefix operator *++> : UndeclaredProtocol
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{22-42=}}
postfix operator +*+> : UndeclaredProtocol
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{23-43=}}

struct Struct {}
class Class {}
infix operator *>*> : Struct
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{21-29=}}
infix operator >**> : Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{21-28=}}

prefix operator **>> : Struct
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{22-30=}}
prefix operator *>*> : Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{22-29=}}

postfix operator >*>* : Struct
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{23-31=}}
postfix operator >>** : Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{23-30=}}

infix operator  <*<<< : MediumPrecedence, &
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{41-44=}}

infix operator **^^ : MediumPrecedence // expected-note {{previous operator declaration here}}
infix operator **^^ : InfixMagicOperatorProtocol // expected-error {{operator redeclared}}
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{21-50=}}

infix operator ^%*%^ : MediumPrecedence, Struct, Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{40-55=}}
infix operator ^%*%% : Struct, Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{30-37=}}
// expected-warning@-2 {{designated types are no longer used by the compiler}} {{22-30=}}
prefix operator %^*^^ : Struct, Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{23-38=}}
postfix operator ^^*^% : Struct, Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{24-39=}}
prefix operator %%*^^ : LowPrecedence, Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{23-45=}}
postfix operator ^^*%% : MediumPrecedence, Class
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{24-49=}}

infix operator <*<>*> : AdditionPrecedence,
// expected-warning@-1 {{designated types are no longer used by the compiler}} {{43-44=}}
