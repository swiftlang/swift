// RUN: %target-swift-frontend -verify %s -parse-stdlib -emit-sil

import Swift

public class Klass {
    var field: Int?
}

public typealias MoveOnlyKlass = @_moveOnly Klass

var shouldIDoSomething: Bool { return false }

public func consume(_ x: __owned @_moveOnly Klass) {}

public func simpleChainTest(_ x: Klass) {
    let x2 = Builtin.move(x)
    let y2 = x2
    let k2 = y2
    consume(k2)
}

public func simpleNonConsumingUseTest(_ x: Klass) {
    let x2 = Builtin.move(x)
    print(x2)
}

public func useMoveOnlyWithoutEscaping(_ x: @_moveOnly Klass) {
}

public func multipleNonConsumingUseTest(_ x: Klass) {
    let x2 = Builtin.move(x)
    useMoveOnlyWithoutEscaping(x2)
    useMoveOnlyWithoutEscaping(x2)
    consume(x2)
}

public func FailCase_doubleConsume(_ x: Klass) {
    let x2 = Builtin.move(x) // expected-error {{'x2' consumed more than once}}
    consume(x2) // expected-note {{consuming use}}
    consume(x2) // expected-note {{consuming use}}
}

public func FailCase_doubleConsume_loop(_ x: Klass) {
    let x2 = Builtin.move(x) // expected-error {{'x2' consumed more than once}}
    consume(x2) // expected-note {{consuming use}}
    for _ in 0..<1024 {
        // Make sure no error here.
        useMoveOnlyWithoutEscaping(x2)
    }
    consume(x2) // expected-note {{consuming use}}
}

public func SuccessCase_diamond(_ x: Klass) {
    let x2 = Builtin.move(x)

    if shouldIDoSomething {
        consume(x2)
    } else {
        consume(x2)
    }
}

public func SuccessCase_diamond2(_ x: Klass) {
    let x2 = Builtin.move(x)

    if shouldIDoSomething {
        consume(x2)
    } else {
        useMoveOnlyWithoutEscaping(x2)
    }
}

public func SuccessCase_diamond3(_ x: Klass) {
    let x2 = Builtin.move(x)

    if shouldIDoSomething {
        useMoveOnlyWithoutEscaping(x2)
    } else {
        useMoveOnlyWithoutEscaping(x2)
    }

    consume(x2)
}

public func FailCase_diamond1(_ x: Klass) {
    let x2: @_moveOnly Klass = Builtin.move(x) // expected-error {{'x2' consumed more than once}}

    if shouldIDoSomething {
        // TODO: We need to be able to point out the non-consuming use of x2.

        let y2: @_moveOnly Klass = x2 // expected-note {{consuming use}}
        useMoveOnlyWithoutEscaping(x2)
        useMoveOnlyWithoutEscaping(y2)
    } else {
        useMoveOnlyWithoutEscaping(x2)
    }
}

public func FailCase_diamond2(_ x: Klass) {
    let x2 = Builtin.move(x) // expected-error {{'x2' consumed more than once}}

    if shouldIDoSomething {
        consume(x2) // expected-note {{consuming use}}
        consume(x2) // expected-note {{consuming use}}
    } else {
        consume(x2) // expected-note {{consuming use}}
    }
}

// Loop case. TODO: Needs note saying consume is in a loop.
public func FailCase_loop(_ x: Klass) {
    let x2 = Builtin.move(x) // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        consume(x2) // expected-note {{consuming use}}
    }
}

// Loop case. TODO: Needs note saying consume is in a loop.
public func SuccessCase_loop(_ x: Klass) {
    let x2 = Builtin.move(x)
    for _ in 0..<1024 {
        useMoveOnlyWithoutEscaping(x2)
    }
}

public func FailCase_SeparateDiagnostics(_ x: Klass) {
    let x = Builtin.move(x)
    let y = x
    consume(x)
    consume(y)
    consume(y)
}
