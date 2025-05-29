// RUN: %target-typecheck-verify-swift

protocol P {
    associatedtype A : P where A.X == Self
    associatedtype X : P where P.A == Self
    // expected-error@-1{{cannot access associated type 'A' from 'P'; use a concrete type or generic parameter base instead}}
}
