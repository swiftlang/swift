// RUN: %target-swift-frontend -enable-experimental-move-only -verify %s -parse-stdlib -emit-sil

import Swift

public struct Trivial {
    var value: Int
}

var boolValue: Bool { return true }

public func trivialUseMoveOnlyWithoutEscaping(_ x: Trivial) {
}

public func trivialSimpleChainTest(_ x: Trivial) {
    @_noImplicitCopy let x2 = x
    let y2 = x2
    let k2 = y2
    trivialUseMoveOnlyWithoutEscaping(k2)
}

public func trivialMultipleNonConsumingUseTest(_ x: Trivial) {
    @_noImplicitCopy let x2 = x
    trivialUseMoveOnlyWithoutEscaping(x2)
    trivialUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func trivialUseAfterConsume(_ x: Trivial) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    let y = x2 // expected-note {{consuming use}}
    let z = x2 // expected-note {{consuming use}}
    let _ = y
    let _ = z
    print(x2) // expected-note {{consuming use}}
}

public func trivialDoubleConsume(_ x: Trivial) {
    @_noImplicitCopy let x2 = x  // expected-error {{'x2' consumed more than once}}
    let y = x2 // expected-note {{consuming use}}
    let z = x2 // expected-note {{consuming use}}
    let _ = y
    let _ = z
}

public func trivialLoopConsume(_ x: Trivial) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
    }
}

public func trivialDiamond(_ x: Trivial) {
    @_noImplicitCopy let x2 = x
    if boolValue {
        let y = x2
        let _ = y
    } else {
        let z = x2
        let _ = z
    }
}

public func trivialDiamondInLoop(_ x: Trivial) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          let y = x2 // expected-note {{consuming use}}
          let _ = y
      } else {
          let z = x2 // expected-note {{consuming use}}
          let _ = z
      }
    }
}

public func trivialAssignToVar1(_ x: Trivial) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x
    print(x3)
}

public func trivialAssignToVar2(_ x: Trivial) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    trivialUseMoveOnlyWithoutEscaping(x3)
}

public func trivialAssignToVar3(_ x: Trivial) {
    @_noImplicitCopy let x2 = x
    var x3 = x2
    x3 = x
    print(x3)
}

public func trivialAssignToVar4(_ x: Trivial) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func trivialAssignToVar5(_ x: Trivial) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consuming use}}
    trivialUseMoveOnlyWithoutEscaping(x2)
    x3 = x
    print(x3)
}

public func trivialAccessField(_ x: Trivial) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    print(x2.value) // expected-note {{consuming use}}
    for _ in 0..<1024 {
        print(x2.value) // expected-note {{consuming use}}
    }
}

//////////////////////
// Aggregate Struct //
//////////////////////

public struct AggStruct {
    var lhs: Trivial
    var center: Builtin.Int32
    var rhs: Trivial
}

public func aggStructUseMoveOnlyWithoutEscaping(_ x: AggStruct) {
}
public func aggStructConsume(_ x: __owned AggStruct) {
}

public func aggStructSimpleChainTest(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x
    let y2 = x2
    let k2 = y2
    aggStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggStructSimpleNonConsumingUseTest(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x
    aggStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggStructMultipleNonConsumingUseTest(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggStructUseAfterConsume(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    let y = x2 // expected-note {{consuming use}}
    let _ = y
    let z = x2 // expected-note {{consuming use}}
    let _ = z
    print(x2) // expected-note {{consuming use}}
}

public func aggStructDoubleConsume(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x  // expected-error {{'x2' consumed more than once}}
    let y = x2 // expected-note {{consuming use}}
    let _ = y
    let z = x2 // expected-note {{consuming use}}
    let _ = z
}

public func aggStructLoopConsume(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
    }
}

public func aggStructDiamond(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x
    if boolValue {
        let y = x2
        let _ = y
    } else {
        let y = x2
        let _ = y
    }
}

public func aggStructDiamondInLoop(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        if boolValue {
            let y = x2 // expected-note {{consuming use}}
            let _ = y

            aggStructConsume(x2)
        } else {
            let y = x2 // expected-note {{consuming use}}
            let _ = y
            aggStructConsume(x2)
        }
    }
}

public func aggStructAccessField(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    print(x2.lhs) // expected-note {{consuming use}}
    for _ in 0..<1024 {
        print(x2.lhs) // expected-note {{consuming use}}
    }
}

//////////////////////////////
// Aggregate Generic Struct //
//////////////////////////////

public struct AggGenericStruct<T> {
    var lhs: Trivial
    var rhs: Builtin.RawPointer
}

public func aggGenericStructUseMoveOnlyWithoutEscaping(_ x: AggGenericStruct<Trivial>) {
}
public func aggGenericStructConsume(_ x: __owned AggGenericStruct<Trivial>) {
}

public func aggGenericStructSimpleChainTest(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x
    let y2 = x2
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleNonConsumingUseTest(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructMultipleNonConsumingUseTest(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggGenericStructUseAfterConsume(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    let y = x2  // expected-note {{consuming use}}
    let _ = y
    aggGenericStructConsume(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructDoubleConsume(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x  // expected-error {{'x2' consumed more than once}}
    let y = x2  // expected-note {{consuming use}}
    let _ = y
    let z = x2  // expected-note {{consuming use}}
    let _ = z
}

public func aggGenericStructLoopConsume(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        let y = x2  // expected-note {{consuming use}}
        let _ = y
    }
}

// This is wrong.
public func aggGenericStructDiamond(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    if boolValue {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
        aggGenericStructConsume(x2)
    } else {
        let z = x2 // expected-note {{consuming use}}
        let _ = z
    }
}

public func aggGenericStructDiamondInLoop(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        if boolValue {
            let y = x2 // expected-note {{consuming use}}
            let _ = y
        } else {
            let y = x2 // expected-note {{consuming use}}
            let _ = y
        }
    }
}

public func aggGenericStructAccessField(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    print(x2.lhs) // expected-note {{consuming use}}
    for _ in 0..<1024 {
        print(x2.lhs) // expected-note {{consuming use}}
    }
}

////////////////////////////////////////////////////////////
// Aggregate Generic Struct + Generic But Body is Trivial //
////////////////////////////////////////////////////////////

public func aggGenericStructUseMoveOnlyWithoutEscaping<T>(_ x: AggGenericStruct<T>) {
}
public func aggGenericStructConsume<T>(_ x: __owned AggGenericStruct<T>) {
}

public func aggGenericStructSimpleChainTest<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x
    let y2 = x2
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleNonConsumingUseTest<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructMultipleNonConsumingUseTest<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggGenericStructUseAfterConsume<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    let y = x2 // expected-note {{consuming use}}
    let _ = y
    aggGenericStructConsume(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructDoubleConsume<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x  // expected-error {{'x2' consumed more than once}}
    let y = x2 // expected-note {{consuming use}}
    let _ = y
    let z = x2 // expected-note {{consuming use}}
    let _ = z
}

public func aggGenericStructLoopConsume<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        let z = x2 // expected-note {{consuming use}}
        let _ = z
    }
}

public func aggGenericStructDiamond<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x
    if boolValue {
        let z = x2
        let _ = z
    } else {
        let z = x2
        let _ = z
    }
}

public func aggGenericStructDiamondInLoop<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        if boolValue {
            let z = x2 // expected-note {{consuming use}}
            let _ = z
        } else {
            let y = x2 // expected-note {{consuming use}}
            let _ = y
        }
    }
}

public func aggGenericStructAccessField<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x // expected-error {{'x2' consumed more than once}}
    print(x2.lhs) // expected-note {{consuming use}}
    for _ in 0..<1024 {
        print(x2.lhs) // expected-note {{consuming use}}
    }
}

/////////////////////////////////
// No Implicit Copy Attributes //
/////////////////////////////////

public func klassNoImplicitCopyArgument(@_noImplicitCopy _ x: Trivial) -> Trivial {
    return x
}

public func klassNoImplicitCopyArgumentError(@_noImplicitCopy _ x: Trivial) -> Trivial { // expected-error {{'x' consumed more than once}}
    let y = x // expected-note {{consuming use}}
    print(y)
    return x // expected-note {{consuming use}}
}

public func klassNoImplicitCopyTrivial(_ x: Int) -> Int {
    @_noImplicitCopy let y = x
    return y
}
