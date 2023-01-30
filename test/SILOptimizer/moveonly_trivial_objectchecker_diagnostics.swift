// RUN: %target-swift-emit-sil -verify -enable-experimental-move-only %s

//////////////////
// Declarations //
//////////////////

public class Klass {}

var boolValue: Bool { return true }

@_moveOnly
public struct NonTrivialStruct {
    var i: Int = 0
}

public func nonConsumingUseNonTrivialStruct(_ s: NonTrivialStruct) {}

public func classUseMoveOnlyWithoutEscaping(_ x: Int) {}
public func classUseMoveOnlyWithoutEscaping(_ x: NonTrivialStruct) {}
public func classConsume(_ x: Int) {}
public func classConsume(_ x: __owned NonTrivialStruct) {}

@_moveOnly
public enum NonTrivialEnum {
    case first
    case second((Int, Int))
    case third(NonTrivialStruct)
}

public func nonConsumingUseNonTrivialEnum(_ e : NonTrivialEnum) {}

///////////
// Tests //
///////////

//////////////////////
// Aggregate Struct //
//////////////////////

@_moveOnly
public struct KlassPair {
    var lhs: Int
    var rhs: Int
}

@_moveOnly
public struct AggStruct {
    var lhs: Int
    var center: Int
    var rhs: Int
    var pair: KlassPair
}

public func aggStructUseMoveOnlyWithoutEscaping(_ x: AggStruct) {
}
public func aggStructConsume(_ x: __owned AggStruct) {
}

public func aggStructSimpleChainTest(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let y2 = x2
    let k2 = y2
    aggStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggStructSimpleChainTestArg(_ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    aggStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggStructSimpleChainTestOwnedArg(_ x2: __owned AggStruct) {
    let y2 = x2
    let k2 = y2
    aggStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggStructSimpleNonConsumingUseTest(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    aggStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggStructSimpleNonConsumingUseTestArg(_ x2: AggStruct) {
    aggStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggStructSimpleNonConsumingUseTestOwnedArg(_ x2: __owned AggStruct) {
    aggStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggStructMultipleNonConsumingUseTest(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggStructMultipleNonConsumingUseTestArg(_ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggStructMultipleNonConsumingUseTestOwnedArg(_ x2: __owned AggStruct) {
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggStructUseAfterConsume(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggStructUseAfterConsumeArg(_ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggStructUseAfterConsumeOwnedArg(_ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
    aggStructUseMoveOnlyWithoutEscaping(x2)
    aggStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggStructDoubleConsume(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consuming use}}
    aggStructConsume(x2) // expected-note {{consuming use}}
    aggStructConsume(x2) // expected-note {{consuming use}}
}

public func aggStructDoubleConsumeArg(_ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggStructConsume(x2) // expected-note {{consuming use}}
    aggStructConsume(x2) // expected-note {{consuming use}}
}

public func aggStructDoubleConsumeOwnedArg(_ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
    aggStructConsume(x2) // expected-note {{consuming use}}
    aggStructConsume(x2) // expected-note {{consuming use}}
}

public func aggStructLoopConsume(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
        aggStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggStructLoopConsumeArg(_ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        aggStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggStructLoopConsumeOwnedArg(_ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        aggStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggStructDiamond(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    if boolValue {
        aggStructConsume(x2)
    } else {
        aggStructConsume(x2)
    }
}

public func aggStructDiamondArg(_ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        aggStructConsume(x2) // expected-note {{consuming use}}
    } else {
        aggStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggStructDiamondOwnedArg(_ x2: __owned AggStruct) {
    if boolValue {
        aggStructConsume(x2)
    } else {
        aggStructConsume(x2)
    }
}

public func aggStructDiamondInLoop(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
      if boolValue {
          aggStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggStructDiamondInLoopArg(_ x2: AggStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
      if boolValue {
          aggStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggStructDiamondInLoopOwnedArg(_ x2: __owned AggStruct) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          aggStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggStructAccessField(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggStructAccessFieldArg(_ x2: AggStruct) {
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggStructAccessFieldOwnedArg(_ x2: __owned AggStruct) {
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggStructConsumeField(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

// TODO: We should error here!
public func aggStructConsumeFieldArg(_ x2: AggStruct) {
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

public func aggStructConsumeFieldOwnedArg(_ x2: __owned AggStruct) {
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

public func aggStructAccessGrandField(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggStructAccessGrandFieldArg(_ x2: AggStruct) {
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggStructAccessGrandFieldOwnedArg(_ x2: __owned AggStruct) {
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggStructConsumeGrandField(_ x: AggStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

// TODO: This needs to error.
public func aggStructConsumeGrandFieldArg(_ x2: AggStruct) {
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

public func aggStructConsumeGrandFieldOwnedArg(_ x2: __owned AggStruct) {
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

//////////////////////////////
// Aggregate Generic Struct //
//////////////////////////////

@_moveOnly
public struct AggGenericStruct<T> {
    var lhs: Int
    var rhs: UnsafeRawPointer
    var pair: KlassPair
}

public func aggGenericStructUseMoveOnlyWithoutEscaping(_ x: AggGenericStruct<Klass>) {
}
public func aggGenericStructConsume(_ x: __owned AggGenericStruct<Klass>) {
}

public func aggGenericStructSimpleChainTest(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let y2 = x2
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleChainTestArg(_ x2: AggGenericStruct<Klass>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleChainTestOwnedArg(_ x2: __owned AggGenericStruct<Klass>) {
    let y2 = x2
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleNonConsumingUseTest(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestArg(_ x2: AggGenericStruct<Klass>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestOwnedArg(_ x2: __owned AggGenericStruct<Klass>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructMultipleNonConsumingUseTest(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggGenericStructMultipleNonConsumingUseTestArg(_ x2: AggGenericStruct<Klass>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructMultipleNonConsumingUseTestOwnedArg(_ x2: __owned AggGenericStruct<Klass>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggGenericStructUseAfterConsume(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructUseAfterConsumeArg(_ x2: AggGenericStruct<Klass>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructUseAfterConsumeOwnedArg(_ x2: __owned AggGenericStruct<Klass>) { // expected-error {{'x2' consumed more than once}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructDoubleConsume(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consuming use}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
}

public func aggGenericStructDoubleConsumeArg(_ x2: AggGenericStruct<Klass>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
}

public func aggGenericStructDoubleConsumeOwnedArg(_ x2: __owned AggGenericStruct<Klass>) { // expected-error {{'x2' consumed more than once}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
}

public func aggGenericStructLoopConsume(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggGenericStructLoopConsumeArg(_ x2: AggGenericStruct<Klass>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggGenericStructLoopConsumeOwnedArg(_ x2: __owned AggGenericStruct<Klass>) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggGenericStructDiamond(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    if boolValue {
        aggGenericStructConsume(x2)
    } else {
        aggGenericStructConsume(x2)
    }
}

public func aggGenericStructDiamondArg(_ x2: AggGenericStruct<Klass>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    } else {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggGenericStructDiamondOwnedArg(_ x2: __owned AggGenericStruct<Klass>) {
    if boolValue {
        aggGenericStructConsume(x2)
    } else {
        aggGenericStructConsume(x2)
    }
}

public func aggGenericStructDiamondInLoop(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
      if boolValue {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggGenericStructDiamondInLoopArg(_ x2: AggGenericStruct<Klass>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
      if boolValue {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggGenericStructDiamondInLoopOwnedArg(_ x2: __owned AggGenericStruct<Klass>) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggGenericStructAccessField(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggGenericStructAccessFieldArg(_ x2: AggGenericStruct<Klass>) {
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggGenericStructAccessFieldOwnedArg(_ x2: __owned AggGenericStruct<Klass>) {
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggGenericStructConsumeField(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

public func aggGenericStructConsumeFieldArg(_ x2: AggGenericStruct<Klass>) {
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

public func aggGenericStructConsumeFieldOwnedArg(_ x2: __owned AggGenericStruct<Klass>) {
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

public func aggGenericStructAccessGrandField(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggGenericStructAccessGrandFieldArg(_ x2: AggGenericStruct<Klass>) {
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggGenericStructAccessGrandFieldOwnedArg(_ x2: __owned AggGenericStruct<Klass>) {
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggGenericStructConsumeGrandField(_ x: AggGenericStruct<Klass>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

public func aggGenericStructConsumeGrandFieldArg(_ x2: AggGenericStruct<Klass>) {
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

public func aggGenericStructConsumeGrandFieldOwnedArg(_ x2: __owned AggGenericStruct<Klass>) {
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

////////////////////////////////////////////////////////////
// Aggregate Generic Struct + Generic But Body is Trivial //
////////////////////////////////////////////////////////////

public func aggGenericStructUseMoveOnlyWithoutEscaping<T>(_ x: AggGenericStruct<T>) {
}
public func aggGenericStructConsume<T>(_ x: __owned AggGenericStruct<T>) {
}

public func aggGenericStructSimpleChainTest<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let y2 = x2
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleChainTestArg<T>(_ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleChainTestOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) {
    let y2 = x2
    let k2 = y2
    aggGenericStructUseMoveOnlyWithoutEscaping(k2)
}

public func aggGenericStructSimpleNonConsumingUseTest<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestArg<T>(_ x2: AggGenericStruct<T>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
}

public func aggGenericStructMultipleNonConsumingUseTest<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggGenericStructMultipleNonConsumingUseTestArg<T>(_ x2: AggGenericStruct<T>) { //expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructMultipleNonConsumingUseTestOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) {
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func aggGenericStructUseAfterConsume<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructUseAfterConsumeArg<T>(_ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructUseAfterConsumeOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
    aggGenericStructUseMoveOnlyWithoutEscaping(x2)
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func aggGenericStructDoubleConsume<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consuming use}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
}

public func aggGenericStructDoubleConsumeArg<T>(_ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
}

public func aggGenericStructDoubleConsumeOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
    aggGenericStructConsume(x2) // expected-note {{consuming use}}
}

public func aggGenericStructLoopConsume<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggGenericStructLoopConsumeArg<T>(_ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggGenericStructLoopConsumeOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggGenericStructDiamond<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    if boolValue {
        aggGenericStructConsume(x2)
    } else {
        aggGenericStructConsume(x2)
    }
}

public func aggGenericStructDiamondArg<T>(_ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    } else {
        aggGenericStructConsume(x2) // expected-note {{consuming use}}
    }
}

public func aggGenericStructDiamondOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) {
    if boolValue {
        aggGenericStructConsume(x2)
    } else {
        aggGenericStructConsume(x2)
    }
}

public func aggGenericStructDiamondInLoop<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
      if boolValue {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggGenericStructDiamondInLoopArg<T>(_ x2: AggGenericStruct<T>) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
      if boolValue {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggGenericStructDiamondInLoopOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      } else {
          aggGenericStructConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func aggGenericStructAccessField<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggGenericStructAccessFieldArg<T>(_ x2: AggGenericStruct<T>) {
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggGenericStructAccessFieldOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) {
    classUseMoveOnlyWithoutEscaping(x2.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.lhs)
    }
}

public func aggGenericStructConsumeField<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

public func aggGenericStructConsumeFieldArg<T>(_ x2: AggGenericStruct<T>) {
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

public func aggGenericStructConsumeFieldOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) {
    classConsume(x2.lhs)
    for _ in 0..<1024 {
        classConsume(x2.lhs)
    }
}

public func aggGenericStructAccessGrandField<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggGenericStructAccessGrandFieldArg<T>(_ x2: AggGenericStruct<T>) {
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggGenericStructAccessGrandFieldOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) {
    classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.pair.lhs)
    }
}

public func aggGenericStructConsumeGrandField<T>(_ x: AggGenericStruct<T>) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

public func aggGenericStructConsumeGrandFieldArg<T>(_ x2: AggGenericStruct<T>) {
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

public func aggGenericStructConsumeGrandFieldOwnedArg<T>(_ x2: __owned AggGenericStruct<T>) {
    classConsume(x2.pair.lhs)
    for _ in 0..<1024 {
        classConsume(x2.pair.lhs)
    }
}

/////////////////////
// Enum Test Cases //
/////////////////////

@_moveOnly
public enum EnumTy {
    case klass(NonTrivialStruct)
    case int(Int)

    func doSomething() -> Bool { true }
}

public func enumUseMoveOnlyWithoutEscaping(_ x: EnumTy) {
}
public func enumConsume(_ x: __owned EnumTy) {
}

public func enumSimpleChainTest(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let y2 = x2
    let k2 = y2
    enumUseMoveOnlyWithoutEscaping(k2)
}

public func enumSimpleChainTestArg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    enumUseMoveOnlyWithoutEscaping(k2)
}

public func enumSimpleChainTestOwnedArg(_ x2: __owned EnumTy) {
    let y2 = x2
    let k2 = y2
    enumUseMoveOnlyWithoutEscaping(k2)
}

public func enumSimpleNonConsumingUseTest(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    enumUseMoveOnlyWithoutEscaping(x2)
}

public func enumSimpleNonConsumingUseTestArg(_ x2: EnumTy) {
    enumUseMoveOnlyWithoutEscaping(x2)
}

public func enumSimpleNonConsumingUseTestOwnedArg(_ x2: __owned EnumTy) {
    enumUseMoveOnlyWithoutEscaping(x2)
}

public func enumMultipleNonConsumingUseTest(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    enumUseMoveOnlyWithoutEscaping(x2)
    enumUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func enumMultipleNonConsumingUseTestArg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    enumUseMoveOnlyWithoutEscaping(x2)
    enumUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func enumMultipleNonConsumingUseTestOwnedArg(_ x2: __owned EnumTy) {
    enumUseMoveOnlyWithoutEscaping(x2)
    enumUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func enumUseAfterConsume(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    enumUseMoveOnlyWithoutEscaping(x2)
    enumConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func enumUseAfterConsumeArg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    enumUseMoveOnlyWithoutEscaping(x2)
    enumConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func enumUseAfterConsumeOwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    enumUseMoveOnlyWithoutEscaping(x2)
    enumConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func enumDoubleConsume(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consuming use}}
    enumConsume(x2) // expected-note {{consuming use}}
    enumConsume(x2) // expected-note {{consuming use}}
}

public func enumDoubleConsumeArg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    enumConsume(x2) // expected-note {{consuming use}}
    enumConsume(x2) // expected-note {{consuming use}}
}

public func enumDoubleConsumeOwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    enumConsume(x2) // expected-note {{consuming use}}
    enumConsume(x2) // expected-note {{consuming use}}
}

public func enumLoopConsume(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
        enumConsume(x2) // expected-note {{consuming use}}
    }
}

public func enumLoopConsumeArg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        enumConsume(x2) // expected-note {{consuming use}}
    }
}

public func enumLoopConsumeOwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        enumConsume(x2) // expected-note {{consuming use}}
    }
}

public func enumDiamond(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    if boolValue {
        enumConsume(x2)
    } else {
        enumConsume(x2)
    }
}

public func enumDiamondArg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        enumConsume(x2) // expected-note {{consuming use}}
    } else {
        enumConsume(x2) // expected-note {{consuming use}}
    }
}

public func enumDiamondOwnedArg(_ x2: __owned EnumTy) {
    if boolValue {
        enumConsume(x2)
    } else {
        enumConsume(x2)
    }
}

public func enumDiamondInLoop(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
      if boolValue {
          enumConsume(x2) // expected-note {{consuming use}}
      } else {
          enumConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func enumDiamondInLoopArg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
      if boolValue {
          enumConsume(x2) // expected-note {{consuming use}}
      } else {
          enumConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func enumDiamondInLoopOwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          enumConsume(x2) // expected-note {{consuming use}}
      } else {
          enumConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func enumAssignToVar1(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar1Arg(_ x: EnumTy, _ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                             // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar1OwnedArg(_ x: EnumTy, _ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
                                                                          // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar2(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    enumUseMoveOnlyWithoutEscaping(x3)
}

public func enumAssignToVar2Arg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    enumUseMoveOnlyWithoutEscaping(x3)
}

public func enumAssignToVar2OwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    enumUseMoveOnlyWithoutEscaping(x3)
}

public func enumAssignToVar3(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    var x3 = x2
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar3Arg(_ x: EnumTy, _ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                             // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar3OwnedArg(_ x: EnumTy, _ x2: __owned EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar4(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar4Arg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar4OwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar5(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    enumUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar5Arg(_ x: EnumTy, _ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                             // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    enumUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumAssignToVar5OwnedArg(_ x: EnumTy, _ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
                                                                          // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    enumUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3)
}

public func enumPatternMatchIfLet1(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x.i)
    }
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x.i)
    }
}

public func enumPatternMatchIfLet1Arg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x.i)
    }
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x.i)
    }
}

public func enumPatternMatchIfLet1OwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x)
    }
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x)
    }
}

public func enumPatternMatchIfLet2(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
        if case let .klass(x) = x2 {  // expected-note {{consuming use}}
            classUseMoveOnlyWithoutEscaping(x)
        }
    }
}

public func enumPatternMatchIfLet2Arg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        if case let .klass(x) = x2 {  // expected-note {{consuming use}}
            classUseMoveOnlyWithoutEscaping(x)
        }
    }
}

public func enumPatternMatchIfLet2OwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        if case let .klass(x) = x2 {  // expected-note {{consuming use}}
            classUseMoveOnlyWithoutEscaping(x)
        }
    }
}

// This is wrong.
public func enumPatternMatchSwitch1(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    switch x2 { // expected-note {{consuming use}}
    case let .klass(k):
        classUseMoveOnlyWithoutEscaping(k)
        // This should be flagged as the use after free use. We are atleast
        // erroring though.
        enumUseMoveOnlyWithoutEscaping(x2)
    case .int:
        break
    }
}

public func enumPatternMatchSwitch1Arg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    switch x2 { // expected-note {{consuming use}}
    case let .klass(k):
        classUseMoveOnlyWithoutEscaping(k)
        // This should be flagged as the use after free use. We are atleast
        // erroring though.
        enumUseMoveOnlyWithoutEscaping(x2)
    case .int:
        break
    }
}

public func enumPatternMatchSwitch1OwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    switch x2 { // expected-note {{consuming use}}
    case let .klass(k):
        classUseMoveOnlyWithoutEscaping(k)
        // This should be flagged as the use after free use. We are atleast
        // erroring though.
        enumUseMoveOnlyWithoutEscaping(x2)
    case .int:
        break
    }
}

public func enumPatternMatchSwitch2(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    switch x2 {
    case let .klass(k):
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    }
}

public func enumPatternMatchSwitch2Arg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    switch x2 { // expected-note {{consuming use}}
    case let .klass(k):
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    }
}

public func enumPatternMatchSwitch2OwnedArg(_ x2: __owned EnumTy) {
    switch x2 {
    case let .klass(k):
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    }
}

// QOI: We can do better here. We should also flag x2
public func enumPatternMatchSwitch2WhereClause(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    switch x2 { // expected-note {{consuming use}}
    case let .klass(k)
           where x2.doSomething():
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    case .klass:
        break
    }
}

public func enumPatternMatchSwitch2WhereClauseArg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    switch x2 { // expected-note {{consuming use}}
    case let .klass(k)
           where x2.doSomething():
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    case .klass:
        break
    }
}

public func enumPatternMatchSwitch2WhereClauseOwnedArg(_ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
    switch x2 { // expected-note {{consuming use}}
    case let .klass(k)
           where x2.doSomething():
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    case .klass:
        break
    }
}

public func enumPatternMatchSwitch2WhereClause2(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    switch x2 {
    case let .klass(k)
           where boolValue:
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    case .klass:
        break
    }
}

public func enumPatternMatchSwitch2WhereClause2Arg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    switch x2 { // expected-note {{consuming use}}
    case let .klass(k)
           where boolValue:
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    case .klass:
        break
    }
}

public func enumPatternMatchSwitch2WhereClause2OwnedArg(_ x2: __owned EnumTy) {
    switch x2 {
    case let .klass(k)
           where boolValue:
        classUseMoveOnlyWithoutEscaping(k)
    case .int:
        break
    case .klass:
        break
    }
}

/////////////////////////////
// Closure and Defer Tests //
/////////////////////////////

public func closureClassUseAfterConsume1(_ x: NonTrivialStruct) {
    // expected-error @-1 {{'x' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // expected-error @-2 {{'x' has guaranteed ownership but was consumed due to being captured by a closure}}
    let f = { // expected-note {{closure capture}}
        let x2 = x // expected-error {{'x2' consumed more than once}}
        // expected-note @-1 {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f()
}

public func closureClassUseAfterConsume2(_ argX: NonTrivialStruct) {
    let f = { (_ x: NonTrivialStruct) in // expected-error {{'x' has guaranteed ownership but was consumed}}
        let x2 = x // expected-error {{'x2' consumed more than once}}
                   // expected-note @-1 {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f(argX)
}

public func closureClassUseAfterConsumeArg(_ argX: NonTrivialStruct) {
    // TODO: Fix this
    let f = { (_ x2: NonTrivialStruct) in // expected-error {{'x2' has guaranteed ownership but was consumed}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f(argX)
}

public func closureCaptureClassUseAfterConsume(_ x: NonTrivialStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f()
}

public func closureCaptureClassUseAfterConsumeError(_ x: NonTrivialStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
    // expected-note @-1 {{consuming use}}
    // expected-error @-2 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f()
    let x3 = x2 // expected-note {{consuming use}}
    let _ = x3
}

public func closureCaptureClassArgUseAfterConsume(_ x2: NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // expected-error @-2 {{'x2' has guaranteed ownership but was consumed due to being captured by a closure}}
    let f = { // expected-note {{closure capture}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f()
}

public func closureCaptureClassOwnedArgUseAfterConsume(_ x2: __owned NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f()
}

public func closureCaptureClassOwnedArgUseAfterConsume2(_ x2: __owned NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f()
    let x3 = x2 // expected-note {{consuming use}}
    let _ = x3
}

public func deferCaptureClassUseAfterConsume(_ x: NonTrivialStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    print(x) // expected-note {{consuming use}}
}

public func deferCaptureClassUseAfterConsume2(_ x: NonTrivialStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
    // expected-note @-1 {{consuming use}}
    // expected-error @-2 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // TODO: Defer error is b/c we have to lifetime extend x2 for the defer. The
    // use is a guaranteed use, so we don't emit an error on that use.
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    let x3 = x2 // expected-note {{consuming use}}
    let _ = x3
}

public func deferCaptureClassArgUseAfterConsume(_ x2: NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    classUseMoveOnlyWithoutEscaping(x2)
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    print("foo")
}

public func deferCaptureClassOwnedArgUseAfterConsume(_ x2: __owned NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    print("foo")
}

public func deferCaptureClassOwnedArgUseAfterConsume2(_ x2: __owned NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    print(x2) // expected-note {{consuming use}}
}

public func closureAndDeferCaptureClassUseAfterConsume(_ x: NonTrivialStruct) {
    // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = {
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        print("foo")
    }
    f()
}

public func closureAndDeferCaptureClassUseAfterConsume2(_ x: NonTrivialStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // expected-error @-2 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = {
        classConsume(x2) // expected-note {{consuming use}}
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        print("foo")
    }
    f()
}

public func closureAndDeferCaptureClassUseAfterConsume3(_ x: NonTrivialStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
    // expected-note @-1 {{consuming use}}
    // expected-error @-2 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // expected-error @-3 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = { // expected-note {{consuming use}}
        classConsume(x2) // expected-note {{consuming use}}
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        print("foo")
    }
    f()
    classConsume(x2) // expected-note {{consuming use}}
}

public func closureAndDeferCaptureClassArgUseAfterConsume(_ x2: NonTrivialStruct) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = { // expected-note {{closure capture}}
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        print("foo")
    }
    f()
}

public func closureAndDeferCaptureClassOwnedArgUseAfterConsume(_ x2: __owned NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = {
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        print("foo")
    }
    f()
}

public func closureAndDeferCaptureClassOwnedArgUseAfterConsume2(_ x2: __owned NonTrivialStruct) { // expected-error {{'x2' consumed more than once}}
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    let f = { // expected-note {{consuming use}}
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) //  expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        print("foo")
    }
    f()
    print(x2) // expected-note {{consuming use}}
}

public func closureAndClosureCaptureClassUseAfterConsume(_ x: NonTrivialStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // expected-error @-2 {{'x2' has guaranteed ownership but was consumed due to being captured by a closure}}
    let f = {
        let g = { // expected-note {{closure capture}}
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        g()
    }
    f()
}

public func closureAndClosureCaptureClassUseAfterConsume2(_ x: NonTrivialStruct) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
               // expected-error @-2 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
               // expected-error @-3 {{'x2' has guaranteed ownership but was consumed due to being captured by a closure}}
    let f = { // expected-note {{consuming use}}
        let g = { // expected-note {{closure capture}}
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        g()
    }
    f()
    print(x2) // expected-note {{consuming use}}
}


public func closureAndClosureCaptureClassArgUseAfterConsume(_ x2: NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // expected-error @-2 {{'x2' has guaranteed ownership but was consumed due to being captured by a closure}}
    // expected-error @-3 {{'x2' has guaranteed ownership but was consumed due to being captured by a closure}}
    let f = { // expected-note {{closure capture}}
        let g = { // expected-note {{closure capture}}
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        g()
    }
    f()
}

public func closureAndClosureCaptureClassOwnedArgUseAfterConsume(_ x2: __owned NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // expected-error @-2 {{'x2' has guaranteed ownership but was consumed due to being captured by a closure}}
    let f = {
        let g = { // expected-note {{closure capture}}
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        g()
    }
    f()
}

public func closureAndClosureCaptureClassOwnedArgUseAfterConsume2(_ x2: __owned NonTrivialStruct) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{'x2' consumed in closure. This is illegal since if the closure is invoked more than once the binding will be uninitialized on later invocations}}
    // expected-error @-3 {{'x2' has guaranteed ownership but was consumed due to being captured by a closure}}
    let f = { // expected-note {{consuming use}}
        let g = { // expected-note {{closure capture}}
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2) // expected-note {{consuming use}}
            print(x2) // expected-note {{consuming use}}
        }
        g()
    }
    f()
    print(x2) // expected-note {{consuming use}}
}
