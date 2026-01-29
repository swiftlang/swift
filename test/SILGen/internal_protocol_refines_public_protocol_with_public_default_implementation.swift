// RUN: %target-swift-emit-silgen -verify %s

public protocol A {
    @_borrowed
    subscript() -> Int { get }
}

protocol B: A { }

extension B {
    public subscript() -> Int { return 0 }
}

public struct S: B {
}
