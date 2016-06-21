// RUN: %target-parse-verify-swift

class A { }
protocol P { }

// Duplicate inheritance
class B : A, A { } // expected-error{{duplicate inheritance from 'A'}}{{12-15=}}

// Duplicate inheritance from protocol
class B2 : P, P { } // expected-error{{duplicate inheritance from 'P'}}{{13-16=}}

// Multiple inheritance
class C : B, A { } // expected-error{{multiple inheritance from classes 'B' and 'A'}}

// Superclass in the wrong position
class D : P, A { } // expected-error{{superclass 'A' must appear first in the inheritance clause}}{{12-15=}}{{11-11=A, }}

// Struct inheriting a class
struct S : A { } // expected-error{{non-class type 'S' cannot inherit from class 'A'}}

// Protocol inheriting a class
protocol Q : A { } // expected-error{{non-class type 'Q' cannot inherit from class 'A'}}

// Extension inheriting a class
extension C : A { } // expected-error{{extension of type 'C' cannot inherit from class 'A'}}

// Keywords in inheritance clauses
struct S2 : struct { } // expected-error{{expected identifier for type name}}

// Protocol composition in inheritance clauses
struct S3 : P, protocol<P> { } // expected-error{{duplicate inheritance from 'P'}}
                               // expected-error@-1{{protocol composition is neither allowed nor needed here}}{{16-25=}}{{26-27=}}
struct S4 : protocol< { } // expected-error{{expected identifier for type name}}
                          // expected-error@-1{{protocol composition is neither allowed nor needed here}}{{13-23=}}

class GenericBase<T> {}

class GenericSub<T> : GenericBase<T> {} // okay

class InheritGenericParam<T> : T {} // expected-error {{inheritance from non-protocol, non-class type 'T'}}
class InheritBody : T { // expected-error {{use of undeclared type 'T'}}
	typealias T = A
}
class InheritBodyBad : fn { // expected-error {{use of undeclared type 'fn'}}
	func fn() {}
}
