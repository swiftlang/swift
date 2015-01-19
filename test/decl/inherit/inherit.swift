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
