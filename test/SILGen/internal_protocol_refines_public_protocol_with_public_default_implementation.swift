// RUN: %target-swift-emit-silgen -verify %s

public protocol A {
    @_borrowed
    subscript() -> Int { get }
}

protocol B: A { }

extension B {
    public subscript() -> Int { return 0 } // expected-note {{mark the subscript as 'public' to satisfy the requirement}}
}

public struct S: B { // expected-warning {{subscript must be as accessible as its enclosing type because it matches a requirement in protocol 'A'}}
}
