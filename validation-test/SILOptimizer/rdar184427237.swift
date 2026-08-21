// RUN: %target-swift-frontend %s -enable-experimental-feature Lifetimes -O -emit-sil

// REQUIRES: swift_feature_Lifetimes

// Ensure we don't crash

protocol Carrier: ~Escapable {
    associatedtype Underlying: ~Copyable & ~Escapable
    var underlying: Underlying {
        @_lifetime(borrow self)
        borrowing get
    }
}

protocol Value {
    func read() -> Int
}

struct Payload: Value {
    var n: Int
    func read() -> Int { n }
}

struct Box: Carrier {
    var underlying: Payload
}

func readUnderlying<C: Carrier>(_ value: C) -> Int where C.Underlying: Value {
    value.underlying.read()
}

public func entryPoint(_ x: Int) -> Int {
    readUnderlying(Box(underlying: Payload(n: x)))
}
