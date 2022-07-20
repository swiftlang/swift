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

public func trivialSimpleChainTestArg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    trivialUseMoveOnlyWithoutEscaping(k2)
}

public func trivialSimpleChainTestOwnedArg(@_noImplicitCopy _ x2: __owned Trivial) {
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

public func trivialMultipleNonConsumingUseTestArg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    trivialUseMoveOnlyWithoutEscaping(x2)
    trivialUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func trivialMultipleNonConsumingUseTestOwnedArg(@_noImplicitCopy _ x2: __owned Trivial) {
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

public func trivialUseAfterConsumeArg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y = x2 // expected-note {{consuming use}}
    let z = x2 // expected-note {{consuming use}}
    let _ = y
    let _ = z
    print(x2) // expected-note {{consuming use}}
}

public func trivialUseAfterConsumeOwnedArg(@_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func trivialDoubleConsumeArg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y = x2 // expected-note {{consuming use}}
    let z = x2 // expected-note {{consuming use}}
    let _ = y
    let _ = z
}

public func trivialDoubleConsumeOwnedArg(@_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func trivialLoopConsumeArg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
    }
}

public func trivialLoopConsumeOwnedArg(@_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func trivialDiamondArg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
    } else {
        let z = x2 // expected-note {{consuming use}}
        let _ = z
    }
}

public func trivialDiamondOwnedArg(@_noImplicitCopy _ x2: __owned Trivial) {
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

public func trivialDiamondInLoopArg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
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

public func trivialDiamondInLoopOwnedArg(@_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func trivialAssignToVar1Arg(_ x: Trivial, @_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x
    print(x3)
}

public func trivialAssignToVar1OwnedArg(_ x: Trivial, @_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func trivialAssignToVar2Arg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    trivialUseMoveOnlyWithoutEscaping(x3)
}

public func trivialAssignToVar2OwnedArg(@_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func trivialAssignToVar3Arg(_ x: Trivial, @_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x
    print(x3)
}

public func trivialAssignToVar3OwnedArg(_ x: Trivial, @_noImplicitCopy _ x2: __owned Trivial) {
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

public func trivialAssignToVar4Arg(@_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func trivialAssignToVar4OwnedArg(@_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func trivialAssignToVar5Arg(_ x: Trivial, @_noImplicitCopy _ x2: Trivial) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    trivialUseMoveOnlyWithoutEscaping(x2)
    x3 = x
    print(x3)
}

public func trivialAssignToVar5OwnedArg(_ x: Trivial, @_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func trivialAccessFieldArg(@_noImplicitCopy _ x2: Trivial) {
    print(x2.value)
    for _ in 0..<1024 {
        print(x2.value)
    }
}

public func trivialAccessFieldOwnedArg(@_noImplicitCopy _ x2: __owned Trivial) { // expected-error {{'x2' consumed more than once}}
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

public func aggStructSimpleChainTestArg(@_noImplicitCopy _ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    aggStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggStructSimpleChainTestOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) {
    let y2 = x2
    let k2 = y2
    aggStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggStructSimpleNonConsumingUseTest(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x
    aggStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggStructSimpleNonConsumingUseTestArg(@_noImplicitCopy _ x2: AggStruct) {
    aggStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggStructSimpleNonConsumingUseTestOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) {
    aggStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggStructMultipleNonConsumingUseTest(_ x: AggStruct) {
    @_noImplicitCopy let x2 = x
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggStructMultipleNonConsumingUseTestArg(@_noImplicitCopy _ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggStructMultipleNonConsumingUseTestOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) {
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

public func aggStructUseAfterConsumeArg(@_noImplicitCopy _ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y = x2 // expected-note {{consuming use}}
    let _ = y
    let z = x2 // expected-note {{consuming use}}
    let _ = z
    print(x2) // expected-note {{consuming use}}
}


public func aggStructUseAfterConsumeOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
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

public func aggStructDoubleConsumeArg(@_noImplicitCopy _ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y = x2 // expected-note {{consuming use}}
    let _ = y
    let z = x2 // expected-note {{consuming use}}
    let _ = z
}

public func aggStructDoubleConsumeOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
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

public func aggStructLoopConsumeArg(@_noImplicitCopy _ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
    }
}

public func aggStructLoopConsumeOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
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

public func aggStructDiamondArg(@_noImplicitCopy _ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
    } else {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
    }
}

public func aggStructDiamondOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) {
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

public func aggStructDiamondInLoopArg(@_noImplicitCopy _ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
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

public func aggStructDiamondInLoopOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
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

public func aggStructAccessFieldArg(@_noImplicitCopy _ x2: AggStruct) {
    print(x2.lhs)
    for _ in 0..<1024 {
        print(x2.lhs)
    }
}

public func aggStructAccessFieldOwnedArg(@_noImplicitCopy _ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructSimpleChainTestArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleChainTestOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) {
    let y2 = x2
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleNonConsumingUseTest(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructMultipleNonConsumingUseTest(_ x: AggGenericStruct<Trivial>) {
    @_noImplicitCopy let x2 = x
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggGenericStructMultipleNonConsumingUseTestArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructMultipleNonConsumingUseTestOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) {
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

public func aggGenericStructUseAfterConsumeArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    let y = x2  // expected-note {{consuming use}}
    let _ = y
    aggGenericStructConsume(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructUseAfterConsumeOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructDoubleConsumeArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y = x2  // expected-note {{consuming use}}
    let _ = y
    let z = x2  // expected-note {{consuming use}}
    let _ = z
}

public func aggGenericStructDoubleConsumeOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructLoopConsumeArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        let y = x2  // expected-note {{consuming use}}
        let _ = y
    }
}

public func aggGenericStructLoopConsumeOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructDiamondArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        let y = x2 // expected-note {{consuming use}}
        let _ = y
        aggGenericStructConsume(x2)
    } else {
        let z = x2 // expected-note {{consuming use}}
        let _ = z
    }
}

public func aggGenericStructDiamondOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructDiamondInLoopArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
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

public func aggGenericStructDiamondInLoopOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructAccessFieldArg(@_noImplicitCopy _ x2: AggGenericStruct<Trivial>) {
    print(x2.lhs)
    for _ in 0..<1024 {
        print(x2.lhs)
    }
}

public func aggGenericStructAccessFieldOwnedArg(@_noImplicitCopy _ x2: __owned AggGenericStruct<Trivial>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructSimpleChainTestArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleChainTestOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) {
    let y2 = x2
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleNonConsumingUseTest<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructMultipleNonConsumingUseTest<T>(_ x: AggGenericStruct<T>) {
    @_noImplicitCopy let x2 = x
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggGenericStructMultipleNonConsumingUseTestArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructMultipleNonConsumingUseTestOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) {
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

public func aggGenericStructUseAfterConsumeArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    let y = x2 // expected-note {{consuming use}}
    let _ = y
    aggGenericStructConsume(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructUseAfterConsumeOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructDoubleConsumeArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y = x2 // expected-note {{consuming use}}
    let _ = y
    let z = x2 // expected-note {{consuming use}}
    let _ = z
}

public func aggGenericStructDoubleConsumeOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructLoopConsumeArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        let z = x2 // expected-note {{consuming use}}
        let _ = z
    }
}

public func aggGenericStructLoopConsumeOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructDiamondArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        let z = x2 // expected-note {{consuming use}}
        let _ = z
    } else {
        let z = x2 // expected-note {{consuming use}}
        let _ = z
    }
}

public func aggGenericStructDiamondOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) {
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

public func aggGenericStructDiamondInLoopArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
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

public func aggGenericStructDiamondInLoopOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
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

public func aggGenericStructAccessFieldArg<T>(@_noImplicitCopy _ x2: AggGenericStruct<T>) {
    print(x2.lhs)
    for _ in 0..<1024 {
        print(x2.lhs)
    }
}

public func aggGenericStructAccessFieldOwnedArg<T>(@_noImplicitCopy _ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
    print(x2.lhs) // expected-note {{consuming use}}
    for _ in 0..<1024 {
        print(x2.lhs) // expected-note {{consuming use}}
    }
}

///////////////////
// Return Values //
///////////////////

public func noImplicitCopyArgReturn(@_noImplicitCopy _ x: Trivial) -> Trivial { // expected-error {{'x' has guaranteed ownership but was consumed}}
    return x // expected-note {{consuming use}}
}

public func noImplicitCopyArgReturnWithAssign(@_noImplicitCopy _ x: Trivial) -> Trivial { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let y = x // expected-note {{consuming use}}
    print(y)
    return x // expected-note {{consuming use}}
}

public func noImplicitCopyReturn(_ x: Int) -> Int {
    @_noImplicitCopy let y = x
    return y
}

public func noImplicitCopyReturnUse(_ x: Int) -> Int {
    @_noImplicitCopy let y = x // expected-error {{'y' consumed more than once}}
    let z = y // expected-note {{consuming use}}
    let _ = z
    return y // expected-note {{consuming use}}
}
