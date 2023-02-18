// RUN: %target-swift-frontend -enable-experimental-move-only -verify %s -parse-stdlib -emit-sil -o /dev/null

import Swift

public class Klass {}
public class SubKlass1 : Klass {}
public class SubKlass2 : Klass {}

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

public func simpleLinearUse(_ x: __owned Klass) {
    let y = x // expected-error {{'y' used after being moved}}
    let _ = _move y // expected-note {{move here}}
    nonConsumingUse(y) // expected-note {{use here}}
}

public func conditionalUse1(_ x: Klass) {
    let y = x
    if booleanValue {
        let _ = _move y
    } else {
        nonConsumingUse(y)
    }
}

public func loopUse1(_ x: Klass) {
    let y = x  // expected-error {{'y' used after being moved}}
    let _ = _move y // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

//===---
// Let + Non Consuming Assignment
//

public func simpleLinearUseAssignment(_ x: __owned Klass) {
    let y = x // expected-error {{'y' used after being moved}}
    let _ = _move y // expected-note {{move here}}
    let m = y // expected-note {{use here}}
    let _ = m
}

public func conditionalUse1Assignment(_ x: Klass) {
    let y = x
    if booleanValue {
        let _ = _move y
    } else {
        let m = y
        let _ = m
    }
}

public func loopUse1Assignment(_ x: Klass) {
    let y = x  // expected-error {{'y' used after being moved}}
    let _ = _move y // expected-note {{move here}}
    for _ in 0..<1024 {
        let m = y // expected-note {{use here}}
        let _ = m
    }
}

//===---
// Let + Consuming Use
//

public func simpleLinearConsumingUse(_ x: Klass) {
    let y = x // expected-error {{'y' used after being moved}}
    let _ = _move y // expected-note {{move here}}
    consumingUse(y) // expected-note {{use here}}
}

public func conditionalUseOk1(_ x: Klass) {
    let y = x
    if booleanValue {
        let _ = _move y
    } else {
        consumingUse(y)
    }
}

// This test makes sure that in the case where we have two consuming uses, with
// different first level copies, we emit a single diagnostic.
public func conditionalBadConsumingUse(_ x: Klass) {
    let y = x // expected-error {{'y' used after being moved}}
    if booleanValue {
        let _ = _move y // expected-note {{move here}}
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
        let _ = _move y // expected-note {{move here}}
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

public func simpleMoveOfParameter(_ x: __owned Klass) -> () {
    let _ = _move x
}

public func errorSimpleMoveOfParameter(_ x: __owned Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    let _ = _move x // expected-note {{use here}}
}

public func errorSimple2MoveOfParameter(_ x: __owned Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    let _ = consumingUse(x) // expected-note {{use here}}
}

// TODO: I wonder if we could do better for the 2nd error. At least we tell the
// user it is due to the loop.
public func errorLoopMultipleMove(_ x: __owned Klass) -> () { // expected-error {{'x' used after being moved}}
                                                      // expected-error @-1 {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    for _ in 0..<1024 {
        let _ = _move x // expected-note {{move here}}
                         // expected-note @-1 {{cyclic move here. move will occur multiple times in the loop}}
                         // expected-note @-2 {{use here}}
    }
}

public func errorLoopMultipleMove1(_ x: __owned Klass) -> () { // expected-error {{'x' used after being moved}}
    for _ in 0..<1024 {
        let _ = _move x // expected-note {{move here}}
                         // expected-note @-1 {{cyclic move here. move will occur multiple times in the loop}}
    }
}

public func errorLoopMoveOfParameter(_ x: __owned Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    for _ in 0..<1024 {
        consumingUse(x) // expected-note {{use here}}
    }
}

public func errorLoop2MoveOfParameter(_ x: __owned Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(x) // expected-note {{use here}}
    }
}

public func errorSimple2MoveOfParameterNonConsume(_ x: __owned Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    let _ = nonConsumingUse(x) // expected-note {{use here}}
}

public func errorLoopMoveOfParameterNonConsume(_ x: __owned Klass) -> () { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    for _ in 0..<1024 {
        nonConsumingUse(x) // expected-note {{use here}}
    }
}

////////////////////////
// Pattern Match Lets //
////////////////////////

public func patternMatchIfCaseLet(_ x: __owned Klass?) {
    if case let .some(y) = x { // expected-error {{'y' used after being moved}}
        let _ = _move y // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

public func patternMatchSwitchLet(_ x: __owned Klass?) {
    switch x {
    case .none:
        break
    case .some(let y): // expected-error {{'y' used after being moved}}
        let _ = _move y // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    }
}

public func patternMatchSwitchLet2(_ x: __owned (Klass?, Klass?)?) {
    switch x {
    case .some((.some(let y), _)): // expected-error {{'y' used after being moved}}
        let _ = _move y // expected-note {{move here}}
        nonConsumingUse(y) // expected-note {{use here}}
    default:
        break
    }
}

public func patternMatchSwitchLet3(_ x: __owned (Klass?, Klass?)?) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    switch x { // expected-note {{use here}}
    case .some((.some(_), .some(let z))): // expected-error {{'z' used after being moved}}
        let _ = _move z // expected-note {{move here}}
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

public class KlassPair {
    var x = Klass()
    var y = Klass()
}

let myLetGlobal = Klass()
var myVarGlobal = Klass()

public func performMoveOnVarGlobalError() {
    let _ = _move myVarGlobal // expected-error {{move applied to value that the compiler does not support checking}}
}

public func performMoveOnLetGlobalError() {
    let _ = _move myVarGlobal // expected-error {{_move applied to value that the compiler does not support checking}}
}

public func multipleVarsWithSubsequentBorrows() -> Bool {
    let k = Klass()
    let k2 = k
    let k3 = _move k
    return k2 === k3
}

////////////////
// Cast Tests //
////////////////

public func castTest0(_ x: __owned SubKlass1) -> Klass { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    return x as Klass // expected-note {{use here}}
}

public func castTest1(_ x: __owned Klass) -> SubKlass1 { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    return x as! SubKlass1 // expected-note {{use here}}
}

public func castTest2(_ x: __owned Klass) -> SubKlass1? { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    return x as? SubKlass1 // expected-note {{use here}}
}

public func castTestSwitch1(_ x : __owned Klass) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    switch x {
    case let k as SubKlass1: // expected-note {{use here}}
        print(k)
    default:
        print("Nope")
    }
}

public func castTestSwitch2(_ x : __owned Klass) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    switch x {
    case let k as SubKlass1:
        print(k)
    case let k as SubKlass2: // expected-note {{use here}}
        print(k)
    default:
        print("Nope")
    }
}

public func castTestSwitchInLoop(_ x : __owned Klass) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}

    for _ in 0..<1024 {
        switch x {
        case let k as SubKlass1: // expected-note {{use here}}
            print(k)
        default:
            print("Nope")
        }
    }
}

public func castTestIfLet(_ x : __owned Klass) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    if case let k as SubKlass1 = x { // expected-note {{use here}}
        print(k)
    } else {
        print("no")
    }
}

public func castTestIfLetInLoop(_ x : __owned Klass) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    for _ in 0..<1024 {
        if case let k as SubKlass1 = x { // expected-note {{use here}}
            print(k)
        } else {
            print("no")
        }
    }
}

public enum EnumWithKlass {
    case none
    case klass(Klass)
}

public func castTestIfLet2(_ x : __owned EnumWithKlass) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    if case let .klass(k as SubKlass1) = x { // expected-note {{use here}}
        print(k)
    } else {
        print("no")
    }
}

/////////////////////////
// Partial Apply Tests //
/////////////////////////

// Emit a better error here. At least we properly error.
public func partialApplyTest(_ x: __owned Klass) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    let f = { // expected-note {{use here}}
        print(x)
    }
    f()
}

/////////////////
// Defer Tests //
/////////////////

// TODO: Improve this error msg.
//
// NOTE: This will require adding knowledge about captured defer arguments for
// values. This at least prevents the error from happening.
public func deferTest(_ x: __owned Klass) { // expected-error {{'x' used after being moved}}
    let _ = _move x // expected-note {{move here}}
    defer { // expected-note {{use here}}
        nonConsumingUse(x)
    }
    print("do Something")
}
