// RUN: %target-swift-frontend -typecheck -verify %s

// rdar://177641107
public func withThing(_: (consuming Thing) -> Thing.CompletionToken) { }

public struct Thing: ~Copyable {
    public mutating func doStuff() {} 

    public consuming func finalize() -> CompletionToken {
        CompletionToken()
    }

    public struct CompletionToken: ~Copyable {}
}

withThing { thing in
    // TODO: This ought to be allowed, by `thing` inferring the `consuming`
    // ownership modifier from the context of `withThing`'s parameter type.
    thing.doStuff() // expected-error{{cannot use mutating member on immutable value}}
    return thing.finalize()
}
