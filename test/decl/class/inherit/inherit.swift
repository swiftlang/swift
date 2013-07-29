// RUN: %swift -parse %s -verify

class A { }

// Duplicate inheritance
class B : A, A { } // expected-error{{duplicate inheritance from class 'A'}}{{12-15=}}

// Multiple inheritance
class C : B, A { } // expected-error{{multiple inheritance from classes 'B' and 'A'}}

// Struct inheriting a class
struct S : A { } // expected-error{{non-class type 'S' cannot inherit from class 'A'}}

// Protocol inheriting a class
protocol Q : A { } // expected-error{{non-class type 'Q' cannot inherit from class 'A'}}

// Extension inheriting a class
extension C : A { } // expected-error{{extension of type 'C' cannot inherit from class 'A'}}

