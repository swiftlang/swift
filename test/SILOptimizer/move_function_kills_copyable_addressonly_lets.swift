// RUN: %target-swift-frontend -enable-experimental-move-only -verify %s -parse-stdlib -emit-sil -o /dev/null

// REQUIRES: optimized_stdlib

import Swift

//////////////////
// Declarations //
//////////////////

public class Klass {}

public protocol P {}
public protocol SubP1 : P {}
public protocol SubP2 : P {}

func consumingUse<T>(_ k: __owned T) {}
var booleanValue: Bool { false }
func nonConsumingUse<T>(_ k: T) {}

///////////
// Tests //
///////////

//===---
// Let + Non Consuming Use
//

public func simpleLinearUse<T>(_ x: T) {
    let y = x // expected-error {{'y' used after being moved}}
    let _ = _move(y) // expected-note {{move here}}
    nonConsumingUse(y) // expected-note {{use here}}
}

// We just emit an error today for the first error in a block.
public func simpleLinearUse2<T>(_ x: T) {
    let y = x // expected-error {{'y' used after being moved}}
    let _ = _move(y) // expected-note {{move here}}
    nonConsumingUse(y) // expected-note {{use here}}
    nonConsumingUse(y)
}

public func conditionalUse1<T>(_ x: T) {
    let y = x
    if booleanValue {
        let _ = _move(y)
    } else {
        nonConsumingUse(y)
    }
}

public func loopUse1<T>(_ x: T) {
    let y = x  // expected-error {{'y' used after being moved}}
    let _ = _move(y) // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

//===---
// Let + Consuming Use
//

public func simpleLinearConsumingUse<T>(_ x: T) {
    let y = x // expected-error {{'y' used after being moved}}
    let _ = _move(y) // expected-note {{move here}}
    consumingUse(y) // expected-note {{use here}}
}

public func conditionalUseOk1<T>(_ x: T) {
    let y = x
    if booleanValue {
        let _ = _move(y)
    } else {
        consumingUse(y)
    }
}

// This test makes sure that in the case where we have two consuming uses, with
// different first level copies, we emit a single diagnostic.
public func conditionalBadConsumingUse<T>(_ x: T) {
    let y = x // expected-error {{'y' used after being moved}}
    if booleanValue {
        let _ = _move(y) // expected-note {{move here}}
        consumingUse(y) // expected-note {{use here}}
    } else {
        // We shouldn't get any diagnostic on this use.
        consumingUse(y)
    }

    // But this one and the first consumingUse should get a diagnostic. But
    // since this is a later error, we require the user to recompile for now.
    consumingUse(y)
}

public func conditionalBadConsumingUse2<T>(_ x: T) {
    let y = x // expected-error {{'y' used after being moved}}
    if booleanValue {
        let _ = _move(y) // expected-note {{move here}}
    } else {
        // We shouldn't get any diagnostic on this use.
        consumingUse(y)
    }

    consumingUse(y) // expected-note {{use here}}
}


// This test makes sure that in the case where we have two consuming uses, with
// different first level copies, we emit a single diagnostic.
public func conditionalBadConsumingUseLoop<T>(_ x: T) {
    let y = x // expected-error {{'y' used after being moved}}
    if booleanValue {
        let _ = _move(y) // expected-note {{move here}}
        consumingUse(y) // expected-note {{use here}}
    } else {
        // We shouldn't get any diagnostic on this use.
        consumingUse(y)
    }

    // But this one and the first consumingUse should get a diagnostic.
    //
    // We do not actually emit the diagnostic here since we emit only one
    // diagnostic per move at a time.
    for _ in 0..<1024 {
        consumingUse(y)
    }
}

// This test makes sure that in the case where we have two consuming uses, with
// different first level copies, we emit a single diagnostic.
public func conditionalBadConsumingUseLoop2<T>(_ x: T) {
    let y = x // expected-error {{'y' used after being moved}}
    if booleanValue {
        let _ = _move(y) // expected-note {{move here}}
    } else {
        // We shouldn't get any diagnostic on this use.
        consumingUse(y)
    }

    for _ in 0..<1024 {
        consumingUse(y) // expected-note {{use here}}
    }
}

//===
// Parameters

// This is ok, no uses after.
public func simpleMoveOfParameter<T>(_ x: T) -> () {
    let _ = _move(x) // expected-error {{_move applied to value that the compiler does not support checking}}
}

public func simpleMoveOfOwnedParameter<T>(_ x: __owned T) -> () {
    let _ = _move(x)
}

public func errorSimpleMoveOfParameter<T>(_ x: __owned T) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    let _ = _move(x) // expected-note {{use here}}
}

public func errorSimple2MoveOfParameter<T>(_ x: __owned T) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    let _ = consumingUse(x) // expected-note {{use here}}
}

// TODO: I wonder if we could do better for the 2nd error. At least we tell the
// user it is due to the loop.
public func errorLoopMultipleMove<T>(_ x: __owned T) -> () { // expected-error {{'x' used after being moved}}
                                                      // expected-error @-1 {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        let _ = _move(x) // expected-note {{move here}}
                         // expected-note @-1 {{use here}}
                         // expected-note @-2 {{use here}}
    }
}

public func errorLoopMoveOfParameter<T>(_ x: __owned T) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        consumingUse(x) // expected-note {{use here}}
    }
}

public func errorLoop2MoveOfParameter<T>(_ x: __owned T) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(x) // expected-note {{use here}}
    }
}

public func errorSimple2MoveOfParameterNonConsume<T>(_ x: __owned T) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    let _ = nonConsumingUse(x) // expected-note {{use here}}
}

public func errorLoopMoveOfParameterNonConsume<T>(_ x: __owned T) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(x) // expected-note {{use here}}
    }
}

////////////////////////
// Pattern Match Lets //
////////////////////////

public func patternMatchIfCaseLet<T>(_ x: T?) {
    if case let .some(y) = x { // expected-error {{'y' used after being moved}}
        let _ = _move(y) // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

public func patternMatchSwitchLet<T>(_ x: T?) {
    switch x {
    case .none:
        break
    case .some(let y): // expected-error {{'y' used after being moved}}
        let _ = _move(y) // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

public func patternMatchSwitchLet2<T>(_ x: (T?, T?)?) {
    switch x {
    case .some((.some(let y), _)): // expected-error {{'y' used after being moved}}
        let _ = _move(y) // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    default:
        break
    }
}

public func patternMatchSwitchLet3<T>(_ x: __owned (T?, T?)?) { // expected-error {{'x' used after being moved}}
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

public struct Pair<T> {
    var x: T
    var y: T
    var z: Int
}

// Current semantics is that we error on any use of any part of pair once we
// have invalidated a part of pair. We can be less restrictive in the future.
//
// TODO: Why are we emitting two uses here.
public func performMoveOnOneEltOfPair<T>(_ p: __owned Pair<T>) { // expected-error {{'p' used after being moved}}
    let _ = p.z // Make sure we don't crash when we access a trivial value from Pair.
    let _ = _move(p) // expected-note {{move here}}
    nonConsumingUse(p.y) // expected-note {{use here}}
}

public class TPair<T> {
    var x: T? = nil
    var y: T? = nil
}

// TODO: Emit a better error here! We should state that we are applying _move to
// a class field and that is illegal.
public func performMoveOnOneEltOfTPair<T>(_ p: TPair<T>) {
    let _ = _move(p.x) // expected-error {{_move applied to value that the compiler does not support checking}}
    nonConsumingUse(p.y)
}

public func performMoveOnOneEltOfTPair2<T>(_ p: __owned TPair<T>) {
    let _ = _move(p.x) // expected-error {{_move applied to value that the compiler does not support checking}}
    nonConsumingUse(p.y)
}

public func multipleVarsWithSubsequentBorrows<T : Equatable>(_ p: T) -> Bool {
    let k = p
    let k2 = k
    let k3 = _move(k)
    return k2 == k3
}

////////////////
// Cast Tests //
////////////////

public func castTest0<T : SubP1>(_ x: __owned T) -> P { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    return x as P // expected-note {{use here}}
}

public func castTest1<T : P>(_ x: __owned T) -> SubP2 { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    return x as! SubP2 // expected-note {{use here}}
}

public func castTest2<T : P>(_ x: __owned T) -> SubP1? { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    return x as? SubP1 // expected-note {{use here}}
}

public func castTestSwitch1<T : P>(_ x: __owned T) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    switch x { // expected-note {{use here}}
    case let k as SubP1:
        print(k)
    default:
        print("Nope")
    }
}

public func castTestSwitch2<T : P>(_ x: __owned T) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    switch x { // expected-note {{use here}}
    case let k as SubP1:
        print(k)
    case let k as SubP2:
        print(k)
    default:
        print("Nope")
    }
}

public func castTestSwitchInLoop<T : P>(_ x: __owned T) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}

    for _ in 0..<1024 {
        switch x { // expected-note {{use here}}
        case let k as SubP1:
            print(k)
        default:
            print("Nope")
        }
    }
}

public func castTestIfLet<T : P>(_ x: __owned T) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    if case let k as SubP1 = x { // expected-note {{use here}}
        print(k)
    } else {
        print("no")
    }
}

public func castTestIfLetInLoop<T : P>(_ x: __owned T) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    for _ in 0..<1024 {
        if case let k as SubP1 = x { // expected-note {{use here}}
            print(k)
        } else {
            print("no")
        }
    }
}

public enum EnumWithKlass {
    case none
    case klass(P)
}

public func castTestIfLet2(_ x : __owned EnumWithKlass) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    if case let .klass(k as SubP1) = x { // expected-note {{use here}}
        print(k)
    } else {
        print("no")
    }
}

/////////////////////////
// Partial Apply Tests //
/////////////////////////

// Emit a better error here. At least we properly error.
public func partialApplyTest<T>(_ x: __owned T) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    let f = { // expected-note {{use here}}
        nonConsumingUse(x)
    }
    f()
}

/////////////////
// Defer Tests //
/////////////////

// TODO: Emit an error in the defer.
public func deferTest<T>(_ x: __owned T) { // expected-error {{'x' used after being moved}}
    let _ = _move(x) // expected-note {{move here}}
    defer { // expected-note {{use here}}
        nonConsumingUse(x)
    }
    print("do Something")
}
