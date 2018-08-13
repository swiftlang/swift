// RUN: %target-typecheck-verify-swift -swift-version 5

public class A { }
public protocol P { }
public protocol P1 { }

// Duplicate inheritance
class B : A, A { } // expected-error{{duplicate inheritance from 'A'}}{{12-15=}}

// Duplicate inheritance from protocol
class B2 : P, P { } // expected-error{{duplicate inheritance from 'P'}}{{13-16=}}
// FIXME: These are unnecessary
// expected-note@-2 {{'B2' declares conformance to protocol 'P' here}}
// expected-error@-3 {{redundant conformance of 'B2' to protocol 'P'}}

// Multiple inheritance
class C : B, A { } // expected-error{{multiple inheritance from classes 'B' and 'A'}}

// Superclass in the wrong position
class D : P, A { } // expected-error{{superclass 'A' must appear first in the inheritance clause}}{{12-15=}}{{11-11=A, }}

// SR-8160
class D1 : Any, A { } // expected-error{{superclass 'A' must appear first in the inheritance clause}} // We don't emit a fix-it in this case in order to avoid overlapping removal fix-it ranges. However, we emit a removal fix-it for the redundant 'Any', which has the same effect.
// expected-warning@-1 {{redundant conformance of 'D1' to 'Any'}} {{12-17=}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}

class D2 : P & P1, A { } // expected-error{{superclass 'A' must appear first in the inheritance clause}}{{18-21=}}{{12-12=A, }}

@usableFromInline
class D3 : Any, A { } // expected-error{{superclass 'A' must appear first in the inheritance clause}} // We don't emit a fix-it in this case in order to avoid overlapping removal fix-it ranges. However, we emit a removal fix-it for the redundant 'Any', which has the same effect.
// expected-warning@-1 {{redundant conformance of 'D3' to 'Any'}} {{12-17=}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}

@usableFromInline
class D4 : P & P1, A { } // expected-error{{superclass 'A' must appear first in the inheritance clause}}{{18-21=}}{{12-12=A, }}

// Struct inheriting a class
struct S : A { } // expected-error{{non-class type 'S' cannot inherit from class 'A'}}

// Protocol inheriting a class
protocol Q : A { }

// Extension inheriting a class
extension C : A { } // expected-error{{extension of type 'C' cannot inherit from class 'A'}}

// Keywords in inheritance clauses
struct S2 : struct { } // expected-error{{expected type}}

// Protocol composition in inheritance clauses
struct S3 : P, P & Q { } // expected-error {{redundant conformance of 'S3' to protocol 'P'}}
                         // expected-error @-1 {{non-class type 'S3' cannot conform to class protocol 'Q'}}
                         // expected-note @-2 {{'S3' declares conformance to protocol 'P' here}}
struct S4 : P, P { }     // FIXME: expected-error {{duplicate inheritance from 'P'}}
// expected-error@-1{{redundant conformance of 'S4' to protocol 'P'}}
// expected-note@-2{{'S4' declares conformance to protocol 'P' here}}
struct S6 : P & { }      // expected-error {{expected identifier for type name}}
struct S7 : protocol<P, Q> { }  // expected-error {{'protocol<...>' composition syntax has been removed; join the protocols using '&'}}
                                // expected-error @-1 {{non-class type 'S7' cannot conform to class protocol 'Q'}}

class GenericBase<T> {}

class GenericSub<T> : GenericBase<T> {} // okay

class InheritGenericParam<T> : T {} // expected-error {{inheritance from non-protocol, non-class type 'T'}}
class InheritBody : T { // expected-error {{use of undeclared type 'T'}}
  typealias T = A
}
class InheritBodyBad : fn { // expected-error {{use of undeclared type 'fn'}}
  func fn() {}
}
