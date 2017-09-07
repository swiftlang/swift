// RUN: %target-typecheck-verify-swift

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
struct S2 : struct { } // expected-error{{expected type}}

// Protocol composition in inheritance clauses
struct S3 : P, P & Q { } // expected-error {{redundant conformance of 'S3' to protocol 'P'}}
                         // expected-error @-1 {{'Q' requires that 'S3' inherit from 'A'}}
                         // expected-note @-2 {{requirement specified as 'Self' : 'A' [with Self = S3]}}
                         // expected-note @-3 {{'S3' declares conformance to protocol 'P' here}}
struct S4 : P, P { }     // expected-error {{duplicate inheritance from 'P'}}
struct S6 : P & { }      // expected-error {{expected identifier for type name}}
struct S7 : protocol<P, Q> { }  // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}}
                                // expected-error @-1 {{'Q' requires that 'S7' inherit from 'A'}}
                                // expected-note @-2 {{requirement specified as 'Self' : 'A' [with Self = S7]}}

class GenericBase<T> {}

class GenericSub<T> : GenericBase<T> {} // okay

class InheritGenericParam<T> : T {} // expected-error {{inheritance from non-protocol, non-class type 'T'}}
class InheritBody : T { // expected-error {{use of undeclared type 'T'}}
  typealias T = A
}
class InheritBodyBad : fn { // expected-error {{use of undeclared type 'fn'}}
  func fn() {}
}
