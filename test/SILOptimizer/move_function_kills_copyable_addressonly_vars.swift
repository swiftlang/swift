// RUN: %target-swift-frontend -enable-experimental-move-only -verify %s -parse-stdlib -emit-sil -o /dev/null

// REQUIRES: optimized_stdlib

import Swift

public class Klass {}

//////////////////
// Declarations //
//////////////////

func consumingUse<T>(_ k: __owned T) {}
var booleanValue: Bool { false }
func nonConsumingUse<T>(_ k: T) {}
func exchangeUse<T>(_ k: __owned T) -> T { k }

///////////
// Tests //
///////////

public func performMoveOnVarSingleBlock<T>(_ p: T) {
    var x = p
    let _ = _move(x)
    x = p
    nonConsumingUse(x)
}

public func performMoveOnVarSingleBlockError<T>(_ p: T) {
    var x = p // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    nonConsumingUse(x) // expected-note {{use here}}
    x = p
    nonConsumingUse(x)
}

public func performMoveOnVarMultiBlock<T>(_ p: T) {
    var x = p
    let _ = _move(x)

    while booleanValue {
        print("true")
    }

    while booleanValue {
        print("true")
    }

    x = p
    nonConsumingUse(x)
}

public func performMoveOnVarMultiBlockError1<T>(_ p: T) {
    var x = p // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}

    nonConsumingUse(x) // expected-note {{use here}}

    while booleanValue {
        print("true")
    }

    // We only emit an error on the first one.
    nonConsumingUse(x)

    while booleanValue {
        print("true")
    }

    // We only emit an error on the first one.
    nonConsumingUse(x)

    x = p
    nonConsumingUse(x)
}

public func performMoveOnVarMultiBlockError2<T>(_ p: T) {
    var x = p // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}

    while booleanValue {
        print("true")
    }

    nonConsumingUse(x) // expected-note {{use here}}

    while booleanValue {
        print("true")
    }

    // We only error on the first one.
    nonConsumingUse(x)

    x = p
    nonConsumingUse(x)
}

public func performMoveOnInOut<T>(_ p: inout T) { // expected-error {{'p' used after being moved}}
    let buf = _move(p) // expected-note {{move here}}
    let _ = buf
} // expected-note {{use here}}

public func performMoveOnInOut2<T>(_ p: inout T, _ p2: T) {
    let buf = _move(p)
    let _ = buf
    p = p2
}

struct S<T> {
    var buffer: T?

    mutating func appendNoError() {
        let b = _move(self).buffer
        let maybeNewB = exchangeUse(b)
        self = .init(buffer: maybeNewB)
    }

    mutating func appendError() { // expected-error {{'self' used after being moved}}
        let b = _move(self).buffer // expected-note {{move here}}
        let _ = b
    } // expected-note {{use here}}

    mutating func appendThrowingNoError1(_ f: () throws -> ()) throws {
        let b = _move(self).buffer
        let maybeNewB = exchangeUse(b)
        // We have to initialize self before we call try since otherwise we will
        // not initialize self along the throws path.
        self = .init(buffer: maybeNewB)
        try f()
    }

    mutating func appendThrowingNoError2(_ f: () throws -> ()) {
        do {
            let b = _move(self).buffer
            try f()
            let maybeNewB = exchangeUse(b)
            self = .init(buffer: maybeNewB)
        } catch {
            self = .init(buffer: nil)
        }
    }

    // In this case, since we initialize self before the try point, we will have
    // re-initialized self before hitting either the code after the try that is
    // inline or the catch block.
    mutating func appendThrowingNoError3(_ f: () throws -> ()) {
        do {
            let b = _move(self).buffer
            let maybeNewB = exchangeUse(b)
            self = .init(buffer: maybeNewB)
            try f()
        } catch {
        }
    }

    mutating func appendThrowingError0(_ f: () throws -> ()) throws { // expected-error {{'self' used after being moved}}
        let b = _move(self).buffer // expected-note {{move here}}
        let maybeNewB = exchangeUse(b)
        try f() // expected-note {{use here}}
        self = .init(buffer: maybeNewB)
    }


    mutating func appendThrowingError1(_ f: () throws -> ()) throws { // expected-error {{'self' used after being moved}}
        let b = _move(self).buffer // expected-note {{move here}}
        let maybeNewB = exchangeUse(b)
        let _ = maybeNewB
        try f() // expected-note {{use here}}
    }

    mutating func appendThrowingError2(_ f: () throws -> ()) { // expected-error {{'self' used after being moved}}
        do {
            let b = _move(self).buffer // expected-note {{move here}}
            let _ = b
            try f()
        } catch {
            self = .init(buffer: nil)
        }
    } // expected-note {{use here}}

    mutating func appendThrowingError3(_ f: () throws -> ()) { // expected-error {{'self' used after being moved}}
        do {
            let b = _move(self).buffer // expected-note {{move here}}
            try f()
            let maybeNewB = exchangeUse(b)
            self = .init(buffer: maybeNewB)
        } catch {
        }
    } // expected-note {{use here}}

    mutating func appendThrowingError4(_ f: () throws -> ()) { // expected-error {{'self' used after being moved}}
        do {
            let b = _move(self).buffer // expected-note {{move here}}
            let _ = b
            try f()
        } catch {
        }
    } // expected-note {{use here}}
}
