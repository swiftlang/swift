// RUN: %target-swift-frontend -enable-experimental-move-only -verify %s -parse-stdlib -emit-sil -o /dev/null

// REQUIRES: optimized_stdlib

import Swift

public class Klass {}

//////////////////
// Declarations //
//////////////////

func consumingUse(_ k: __owned Klass) {}
var booleanValue: Bool { false }
func nonConsumingUse(_ k: Klass) {}

///////////
// Tests //
///////////

//===---
// Let + Non Consuming Use
//

public func simpleLinearUse(_ x: Klass) {
    let y = x // expected-error {{'y' used after being moved}}
    let _ = _move(y) // expected-note {{move here}}
    nonConsumingUse(y) // expected-note {{use here}}
}

public func conditionalUse1(_ x: Klass) {
    let y = x
    if booleanValue {
        let _ = _move(y)
    } else {
        nonConsumingUse(y)
    }
}

public func loopUse1(_ x: Klass) {
    let y = x  // expected-error {{'y' used after being moved}}
    let _ = _move(y) // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

//===---
// Let + Consuming Use
//

public func simpleLinearConsumingUse(_ x: Klass) {
    let y = x // expected-error {{'y' used after being moved}}
    let _ = _move(y) // expected-note {{move here}}
    consumingUse(y) // expected-note {{use here}}
}

public func conditionalUseOk1(_ x: Klass) {
    let y = x
    if booleanValue {
        let _ = _move(y)
    } else {
        consumingUse(y)
    }
}

// This test makes sure that in the case where we have two consuming uses, with
// different first level copies, we emit a single diagnostic.
public func conditionalBadConsumingUse(_ x: Klass) {
    let y = x // expected-error {{'y' used after being moved}}
    if booleanValue {
        let _ = _move(y) // expected-note {{move here}}
        // TODO: We should be able to also emit a note on the line
        // below. Without this the user will have to compile multiple times to
        // work through the errors. But this makes it simpler to implement a
        // first version and is still safe.
        consumingUse(y)
    } else {
        // We shouldn't get any diagnostic on this use.
        consumingUse(y)
    }

    // But this one and the first consumingUse should get a diagnostic.
    consumingUse(y) // expected-note {{use here}}
}


// This test makes sure that in the case where we have two consuming uses, with
// different first level copies, we emit a single diagnostic.
public func conditionalBadConsumingUseLoop(_ x: Klass) {
    let y = x // expected-error {{'y' used after being moved}}
    if booleanValue {
        let _ = _move(y) // expected-note {{move here}}
        // TODO: We should be able to also emit a note on the line
        // below. Without this the user will have to compile multiple times to
        // work through the errors. But this makes it simpler to implement a
        // first version and is still safe.
        consumingUse(y)
    } else {
        // We shouldn't get any diagnostic on this use.
        consumingUse(y)
    }

    // But this one and the first consumingUse should get a diagnostic.
    for _ in 0..<1024 {
        consumingUse(y) // expected-note {{use here}}
    }
}

//===
// Parameters

// This is ok, no uses after.
public func simpleMoveOfParameter(_ x: Klass) -> () {
    let _ = _move(x)
}

public func errorSimpleMoveOfParameter(_ x: Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    let _ = _move(x) // expected-note {{use here}}
}

public func errorSimple2MoveOfParameter(_ x: Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    let _ = consumingUse(x) // expected-note {{use here}}
}

// TODO: I wonder if we could do better for the 2nd error. At least we tell the
// user it is due to the loop.
public func errorLoopMultipleMove(_ x: Klass) -> () { // expected-error {{'x' used after being moved}}
                                                      // expected-error @-1 {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        let _ = _move(x) // expected-note {{move here}}
                         // expected-note @-1 {{cyclic move here. move will occur multiple times in the loop}}
                         // expected-note @-2 {{use here}}
    }
}

public func errorLoopMoveOfParameter(_ x: Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        consumingUse(x) // expected-note {{use here}}
    }
}

public func errorLoop2MoveOfParameter(_ x: Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(x) // expected-note {{use here}}
    }
}

public func errorSimple2MoveOfParameterNonConsume(_ x: Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    let _ = nonConsumingUse(x) // expected-note {{use here}}
}

public func errorLoopMoveOfParameterNonConsume(_ x: Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(x) // expected-note {{use here}}
    }
}

////////////////////////
// Pattern Match Lets //
////////////////////////

public func patternMatchIfCaseLet(_ x: Klass?) {
    if case let .some(y) = x { // expected-error {{'y' used after being moved}}
        let _ = _move(y) // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

public func patternMatchSwitchLet(_ x: Klass?) {
    switch x {
    case .none:
        break
    case .some(let y): // expected-error {{'y' used after being moved}}
        let _ = _move(y) // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

public func patternMatchSwitchLet2(_ x: (Klass?, Klass?)?) {
    switch x {
    case .some((.some(let y), _)): // expected-error {{'y' used after being moved}}
        let _ = _move(y) // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    default:
        break
    }
}

public func patternMatchSwitchLet3(_ x: (Klass?, Klass?)?) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    switch x { // expected-note {{use here}}
    case .some((.some(_), .some(let z))): // expected-error {{'z' used after being moved}}
        let _ = _move(z) // expected-note {{move here}}
        nonConsumingUse(z) // expected-note {{use here}}
    default:
        break
    }
}

////////////////
// Aggregates //
////////////////

public struct Pair {
    var x: Klass
    var y: Klass
    var z: Int
}

// Current semantics is that we error on any use of any part of pair once we
// have invalidated a part of pair. We can be less restrictive in the future.
//
// TODO: Why are we emitting two uses here.
public func performMoveOnOneEltOfPair(_ p: Pair) { // expected-error {{'p' used after being moved}}
    let _ = p.z // Make sure we don't crash when we access a trivial value from Pair.
    let _ = _move(p.x) // expected-note {{move here}}
    nonConsumingUse(p.y) // expected-note {{use here}}
                         // expected-note @-1 {{use here}}
}

public class KlassPair {
    var x = Klass()
    var y = Klass()
}

// TODO: Emit a better error here! We should state that we are applying _move to
// a class field and that is illegal.
public func performMoveOnOneEltOfKlassPair(_ p: KlassPair) {
    let _ = _move(p.x) // expected-error {{_move applied to value that the compiler does not supporting checking.}}
    nonConsumingUse(p.y)
}

let myLetGlobal = Klass()
var myVarGlobal = Klass()

public func performMoveOnVarGlobalError() {
    let _ = _move(myVarGlobal) // expected-error {{_move applied to value that the compiler does not supporting checking.}}
}

// TODO: Support vars
public func performMoveOnVarError() {
    var k = myVarGlobal
    let _ = _move(k) // expected-error {{_move applied to value that the compiler does not supporting checking.}}
    k = myVarGlobal
}

public func performMoveOnLetGlobalError() {
    let _ = _move(myVarGlobal) // expected-error {{_move applied to value that the compiler does not supporting checking.}}
}
