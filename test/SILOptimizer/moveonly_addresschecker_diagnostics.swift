// RUN: %target-swift-emit-sil %s -O -sil-verify-all -verify -enable-experimental-feature MoveOnlyPartialReinitialization -enable-experimental-feature NoImplicitCopy -enable-experimental-feature MoveOnlyClasses

// REQUIRES: swift_feature_MoveOnlyClasses
// REQUIRES: swift_feature_MoveOnlyPartialReinitialization
// REQUIRES: swift_feature_NoImplicitCopy

//////////////////
// Declarations //
//////////////////

public class MyClass {}

public func borrowVal(_ x: borrowing MyClass) {}
public func borrowVal(_ x: borrowing Klass) {}
public func borrowVal(_ s: borrowing NonTrivialStruct) {}
public func borrowVal(_ s: borrowing NonTrivialStruct2) {}
public func borrowVal(_ s: borrowing NonTrivialCopyableStruct) {}
public func borrowVal(_ s: borrowing NonTrivialCopyableStruct2) {}
public func borrowVal(_ e : borrowing NonTrivialEnum) {}
public func borrowVal(_ x: borrowing FinalKlass) {}
public func borrowVal(_ x: borrowing AggStruct) {}
public func borrowVal(_ x: borrowing AggGenericStruct<MyClass>) {}
public func borrowVal<T>(_ x: borrowing AggGenericStruct<T>) {}
public func borrowVal(_ x: borrowing EnumTy) {}
public func borrowVal<T>(_ x: borrowing AddressOnlyGeneric<T>) {}
public func borrowVal(_ x: borrowing AddressOnlyProtocol) {}
public func borrowVal<T>(_ x: borrowing T) {}

public func consumeVal(_ x: __owned MyClass) {}
public func consumeVal(_ x: __owned Klass) {}
public func consumeVal(_ x: __owned FinalKlass) {}
public func consumeVal(_ x: __owned AggStruct) {}
public func consumeVal(_ x: __owned AggGenericStruct<MyClass>) {}
public func consumeVal<T>(_ x: __owned AggGenericStruct<T>) {}
public func consumeVal(_ x: __owned EnumTy) {}
public func consumeVal(_ x: __owned NonTrivialStruct) {}
public func consumeVal(_ x: __owned NonTrivialStruct2) {}
public func consumeVal<T>(_ x: __owned AddressOnlyGeneric<T>) {}
public func consumeVal(_ x: __owned AddressOnlyProtocol) {}
public func consumeVal<T>(_ x: __owned T) {}

public final class Klass: ~Copyable {
    var intField: Int
    var k: Klass
    init() {
        k = Klass()
        intField = 5
    }
}

public final class FinalKlass: ~Copyable {
    var k: Klass = Klass()
}

var boolValue: Bool { return true }

public struct NonTrivialStruct: ~Copyable {
    var k = Klass()
    var copyableK = MyClass()
    var nonTrivialStruct2 = NonTrivialStruct2()
    var nonTrivialCopyableStruct = NonTrivialCopyableStruct()

    var computedCopyableK: MyClass { MyClass() }
}

public struct NonTrivialStruct2: ~Copyable {
    var copyableKlass = MyClass()
}

public struct NonTrivialCopyableStruct {
    var copyableKlass = MyClass()
    var nonTrivialCopyableStruct2 = NonTrivialCopyableStruct2()
}

public struct NonTrivialCopyableStruct2 {
    var copyableKlass = MyClass()
    var computedMyClass: MyClass { MyClass() }
}

public enum NonTrivialEnum: ~Copyable {
    case first
    case second(Klass)
    case third(NonTrivialStruct)
    case fourth(MyClass)
}

public final class MyClassWithMoveOnlyField {
    var moveOnlyVarStruct = NonTrivialStruct()
    let moveOnlyLetStruct = NonTrivialStruct()
}

public protocol P {
    static var value: Self { get }
    var name: MyClass { get }
    static var value2: any P { get }
}

public struct AddressOnlyGeneric<T : P>: ~Copyable {
    var copyable: T
    var moveOnly = NonTrivialStruct()

    init() {
        self.copyable = T.value
    }

    init(_ input1: T) {
        copyable = input1
        moveOnly = NonTrivialStruct()
    }
}

extension MyClass : P {
    public static var value: Self { fatalError() }
    public static var value2: any P { MyClass() }
    public var name: MyClass { MyClass() }
}

public struct AddressOnlyProtocol: ~Copyable {
    var copyable: any P = MyClass.value2
    var moveOnly = NonTrivialStruct()
}

///////////
// Tests //
///////////

/////////////////
// Class Tests //
/////////////////

public func classSimpleChainTest(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
               // expected-error @-1 {{'x2' consumed more than once}}
    x2 = x // expected-note {{consumed here}}
    let y2 = x2 // expected-note {{consumed here}}
    let k2 = y2
    let k3 = x2 // expected-note {{consumed again here}}
    let _ = k3
    borrowVal(k2)
}

public func classSimpleChainArgTest(_ x2: inout Klass) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    var y2 = x2 // expected-note {{consumed here}}
    y2 = x2 // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
    let k2 = y2
    borrowVal(k2)
}

public func classSimpleNonConsumingUseTest(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
}

public func classSimpleNonConsumingUseArgTest(_ x2: inout Klass) {
    borrowVal(x2)
}

public func classMultipleNonConsumingUseTest(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func classMultipleNonConsumingUseArgTest(_ x2: inout Klass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
}

public func classMultipleNonConsumingUseArgTest2(_ x2: inout Klass) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
}

public func classMultipleNonConsumingUseArgTest3(_ x2: inout Klass) {  // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                                       // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                   // expected-note @-1 {{consumed again here}}
}

public func classMultipleNonConsumingUseArgTest4(_ x2: inout Klass) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x2 = Klass()
}


public func classUseAfterConsume(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func classUseAfterConsumeArg(_ x2: inout Klass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                         // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                   // expected-note @-1 {{consumed again here}}
}

public func classDoubleConsume(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consumed here}}
    x2 = Klass()
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func classDoubleConsumeArg(_ x2: inout Klass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                     // expected-note @-1 {{consumed again here}}
}

public func classLoopConsume(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed in a loop}}
               // expected-note @-1 {{consumed here}}
    x2 = Klass()
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func classLoopConsumeArg(_ x2: inout Klass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func classLoopConsumeArg2(_ x2: inout Klass) { // expected-error {{'x2' consumed in a loop}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
    x2 = Klass()
}

public func classDiamond(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = Klass()
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func classDiamondArg(_ x2: inout Klass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                 // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    if boolValue {
        consumeVal(x2) // expected-note {{consumed here}}
    } else {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func classDiamondInLoop(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed in a loop}}
               // expected-error @-1 {{'x2' consumed more than once}}
               // expected-note @-2 {{consumed here}}
    x2 = Klass()
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                           // expected-note @-1 {{consumed again here}}
      }
    }
}

public func classDiamondInLoopArg(_ x2: inout Klass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                       // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
      }
    }
}

public func classDiamondInLoopArg2(_ x2: inout Klass) { // expected-error {{'x2' consumed in a loop}}
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                           // expected-note @-1 {{consumed again here}}
      }
    }
    x2 = Klass()
}

public func classAssignToVar1(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = Klass()
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func classAssignToVar1Arg(_ x2: inout Klass) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
    x3 = Klass()
    consumeVal(x3)
}

public func classAssignToVar2(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = Klass()
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    borrowVal(x3)
}

public func classAssignToVar2Arg(_ x2: inout Klass) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
    borrowVal(x3)
}

public func classAssignToVar3(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = Klass()
    var x3 = x2
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func classAssignToVar3Arg(_ x: borrowing Klass, _ x2: inout Klass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                            // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func classAssignToVar3Arg2(_ x: borrowing Klass, _ x2: inout Klass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                                   // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func classAssignToVar4(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = Klass()
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
    consumeVal(x3)
}

public func classAssignToVar4Arg(_ x2: inout Klass) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2)   // expected-note {{consumed here}}
                // expected-note @-1 {{consumed again here}}
    consumeVal(x3)
}

public func classAssignToVar5() {
    var x2 = Klass() // expected-error {{'x2' used after consume}}
    x2 = Klass()
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = Klass()
    consumeVal(x3)
}

public func classAssignToVar5Arg(_ x: borrowing Klass, _ x2: inout Klass) {
    // expected-error @-1 {{'x2' used after consume}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func classAssignToVar5Arg2(_ x: borrowing Klass, _ x2: inout Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
                                                                   // expected-error @-1 {{'x2' used after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
    x2 = Klass()
}

public func classAccessAccessField(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = Klass()
    borrowVal(x2.k)
    for _ in 0..<1024 {
        borrowVal(x2.k)
    }
}

public func classAccessAccessFieldArg(_ x2: inout Klass) {
    for _ in 0..<1024 {
        borrowVal(x2.k)
    }
}

public func classAccessConsumeField(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = Klass()
    // Since a class is a reference type, we do not emit an error here.
    consumeVal(x2.k)
    // expected-error @-1 {{cannot consume noncopyable stored property 'x2.k' of a class}}
    for _ in 0..<1024 {
        consumeVal(x2.k)
        // expected-error @-1 {{cannot consume noncopyable stored property 'x2.k' of a class}}
    }
}

public func classAccessConsumeFieldArg(_ x2: inout Klass) {
    // Since a class is a reference type, we do not emit an error here.
    consumeVal(x2.k)
    // expected-error @-1 {{cannot consume noncopyable stored property 'x2.k' of a class}}

    for _ in 0..<1024 {
        consumeVal(x2.k)
        // expected-error @-1 {{cannot consume noncopyable stored property 'x2.k' of a class}}
    }
}

extension Klass {
    func testNoUseSelf() { // expected-error {{'self' is borrowed and cannot be consumed}}
        let x = self // expected-note {{consumed here}}
        let _ = x
    }
}

/////////////////
// Final Class //
/////////////////

public func finalClassSimpleChainTest() {
    var x2 = FinalKlass()
    x2 = FinalKlass()
    let y2 = x2
    let k2 = y2
    borrowVal(k2)
}

public func finalClassSimpleChainTestArg(_ x2: inout FinalKlass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    let y2 = x2 // expected-note {{consumed here}}
    let k2 = y2
    borrowVal(k2)
}

public func finalClassSimpleChainTestArg2(_ x2: inout FinalKlass) {
    let y2 = x2
    let k2 = y2
    borrowVal(k2)
    x2 = FinalKlass()
}

public func finalClassSimpleChainTestArg3(_ x2: inout FinalKlass) {
    for _ in 0..<1024 {}
    let y2 = x2
    let k2 = y2
    borrowVal(k2)
    x2 = FinalKlass()
}

public func finalClassSimpleNonConsumingUseTest(_ x: __owned FinalKlass) {
    var x2 = x
    x2 = FinalKlass()
    borrowVal(x2)
}

public func finalClassSimpleNonConsumingUseTest2(_ x: consuming FinalKlass) {
    var x2 = x
    x2 = FinalKlass()
    borrowVal(x2)
}

public func finalClassSimpleNonConsumingUseTestArg(_ x2: inout FinalKlass) {
    borrowVal(x2)
}

public func finalClassMultipleNonConsumingUseTest() {
    var x2 = FinalKlass()
    x2 = FinalKlass()
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func finalClassMultipleNonConsumingUseTestArg(_ x2: inout FinalKlass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
}

public func finalClassUseAfterConsume() {
    var x2 = FinalKlass() // expected-error {{'x2' consumed more than once}}
    x2 = FinalKlass()
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func finalClassUseAfterConsumeArg(_ x2: inout FinalKlass) { // expected-error {{'x2' consumed more than once}}
                                                                   // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
              // expected-note @-1 {{consumed again here}}
}

public func finalClassDoubleConsume() {
    var x2 = FinalKlass()  // expected-error {{'x2' consumed more than once}}
    x2 = FinalKlass()
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func finalClassDoubleConsumeArg(_ x2: inout FinalKlass) { // expected-error {{'x2' consumed more than once}}
                                                                 // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                          // expected-note @-1 {{consumed again here}}
}

public func finalClassLoopConsume() {
    var x2 = FinalKlass() // expected-error {{'x2' consumed in a loop}}
    x2 = FinalKlass()
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func finalClassLoopConsumeArg(_ x2: inout FinalKlass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func finalClassDiamond() {
    var x2 = FinalKlass()
    x2 = FinalKlass()
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func finalClassDiamondArg(_ x2: inout FinalKlass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                           // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    if boolValue {
        consumeVal(x2) // expected-note {{consumed here}}
    } else {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func finalClassDiamondInLoop() {
    var x2 = FinalKlass() // expected-error {{'x2' consumed in a loop}}
                          // expected-error @-1 {{'x2' consumed more than once}}
    x2 = FinalKlass()
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                                // expected-note @-1 {{consumed again here}}
      }
    }
}

public func finalClassDiamondInLoopArg(_ x2: inout FinalKlass) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                                 // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
      }
    }
}

public func finalClassDiamondInLoopArg2(_ x2: inout FinalKlass) { // expected-error {{consumed in a loop}}
                                                                  // expected-error @-1 {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                                // expected-note @-1 {{consumed again here}}
      }
    }

    x2 = FinalKlass()
}

public func finalClassAssignToVar1() {
    var x2 = FinalKlass() // expected-error {{'x2' consumed more than once}}
    x2 = FinalKlass()
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    x3 = FinalKlass()
    consumeVal(x3)
}

public func finalClassAssignToVar1Arg(_ x2: inout FinalKlass) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
    x3 = FinalKlass()
    consumeVal(x3)
}

public func finalClassAssignToVar2() {
    var x2 = FinalKlass() // expected-error {{'x2' consumed more than once}}
    x2 = FinalKlass()
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    borrowVal(x3)
}

public func finalClassAssignToVar2Arg(_ x2: inout FinalKlass) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
    borrowVal(x3)
}

public func finalClassAssignToVar3() {
    var x2 = Klass()
    x2 = Klass()
    var x3 = x2
    x3 = Klass()
    consumeVal(x3)
}

public func finalClassAssignToVar3Arg(_ x2: inout FinalKlass) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = FinalKlass()
    consumeVal(x3)
}

public func finalClassAssignToVar4() {
    var x2 = FinalKlass() // expected-error {{'x2' consumed more than once}}
    x2 = FinalKlass()
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
    consumeVal(x3)
}

public func finalClassAssignToVar4Arg(_ x2: inout FinalKlass) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
              // expected-note @-1 {{consumed again here}}
    consumeVal(x3)
}

public func finalClassAssignToVar5() {
    var x2 = FinalKlass() // expected-error {{'x2' used after consume}}
    x2 = FinalKlass()
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = FinalKlass()
    consumeVal(x3)
}

public func finalClassAssignToVar5Arg(_ x2: inout FinalKlass) {
    // expected-error @-1 {{'x2' used after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = FinalKlass()
    consumeVal(x3)
}

public func finalClassAssignToVar5Arg2(_ x2: inout FinalKlass) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = FinalKlass()
    consumeVal(x3)
}

public func finalClassAccessField() {
    var x2 = FinalKlass()
    x2 = FinalKlass()
    borrowVal(x2.k)
    for _ in 0..<1024 {
        borrowVal(x2.k)
    }
}

public func finalClassAccessFieldArg(_ x2: inout FinalKlass) {
    borrowVal(x2.k)
    for _ in 0..<1024 {
        borrowVal(x2.k)
    }
}

public func finalClassConsumeField() {
    var x2 = FinalKlass()
    x2 = FinalKlass()

    consumeVal(x2.k)
    // expected-error @-1 {{cannot consume noncopyable stored property 'x2.k' of a class}}
    for _ in 0..<1024 {
        consumeVal(x2.k)
        // expected-error @-1 {{cannot consume noncopyable stored property 'x2.k' of a class}}
    }
}

public func finalClassConsumeFieldArg(_ x2: inout FinalKlass) {
    consumeVal(x2.k)
    // expected-error @-1 {{cannot consume noncopyable stored property 'x2.k' of a class}}
    for _ in 0..<1024 {
        consumeVal(x2.k)
        // expected-error @-1 {{cannot consume noncopyable stored property 'x2.k' of a class}}
    }
}

//////////////////////
// Aggregate Struct //
//////////////////////

public struct KlassPair: ~Copyable {
    var lhs: Klass = Klass()
    var rhs: Klass = Klass()
}

public struct AggStruct: ~Copyable {
    var lhs: Klass = Klass()
    var center: Int = 5
    var rhs: Klass = Klass()
    var pair: KlassPair = KlassPair()

    init() {}

    // Testing that DI ignores normal init. We also get an error on our return
    // value from the function since we do not reinitialize self.
    //
    // TODO: Improve error message!
    init(myInit: Int) { // expected-error {{'self' consumed more than once}}
        let x = self // expected-note {{consumed here}}
        let _ = x
    } // expected-note {{consumed again here}}

    // Make sure we can reinitialize successfully.
    init(myInit2: Int) {
        let x = self
        let _ = x
        self = AggStruct(myInit: myInit2)
    }

    // Testing delegating init.
    //
    // TODO: Improve error to say need to reinitialize self.lhs before end of
    // function.
    init(myInit3: Int) { // expected-error {{'self' consumed more than once}}
        self.init()
        self.center = myInit3
        let x = self.lhs // expected-note {{consumed here}}
        let _ = x
    } // expected-note {{consumed again here}}

    init(myInit4: Int) {
        self.init()
        self.center = myInit4
        let x = self.lhs
        let _ = x
        self = AggStruct(myInit: myInit4)
    }

    init(myInit5: Int) {
        self.init()
        self.center = myInit5
        let x = self.lhs
        let _ = x
        self.lhs = Klass()
    }
}

public func aggStructSimpleChainTest() {
    var x2 = AggStruct()
    x2 = AggStruct()
    let y2 = x2
    let k2 = y2
    borrowVal(k2)
}

public func aggStructSimpleChainTestArg(_ x2: inout AggStruct) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    var y2 = x2 // expected-note {{consumed here}}
    y2 = x2 // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
    let k2 = y2
    borrowVal(k2)
}

public func aggStructSimpleNonConsumingUseTest() {
    var x2 = AggStruct()
    x2 = AggStruct()
    borrowVal(x2)
}

public func aggStructSimpleNonConsumingUseTestArg(_ x2: inout AggStruct) {
    borrowVal(x2)
}

public func aggStructMultipleNonConsumingUseTest() {
    var x2 = AggStruct()
    x2 = AggStruct()
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func aggStructMultipleNonConsumingUseTestArg(_ x2: inout AggStruct) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
}

public func aggStructUseAfterConsume() {
    var x2 = AggStruct() // expected-error {{'x2' consumed more than once}}
    x2 = AggStruct()
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func aggStructUseAfterConsumeArg(_ x2: inout AggStruct) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
              // expected-note @-1 {{consumed again here}}
}

public func aggStructDoubleConsume() {
    var x2 = AggStruct()  // expected-error {{'x2' consumed more than once}}
    x2 = AggStruct()
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func aggStructDoubleConsumeArg(_ x2: inout AggStruct) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
}

public func aggStructLoopConsume() {
    var x2 = AggStruct() // expected-error {{'x2' consumed in a loop}}
    x2 = AggStruct()
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggStructLoopConsumeArg(_ x2: inout AggStruct) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggStructDiamond() {
    var x2 = AggStruct()
    x2 = AggStruct()
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func aggStructDiamondArg(_ x2: inout AggStruct) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    if boolValue {
        consumeVal(x2) // expected-note {{consumed here}}
    } else {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggStructDiamondInLoop() {
    var x2 = AggStruct()
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = AggStruct()
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
          // expected-note @-1 {{consumed again here}}
      }
    }
}

public func aggStructDiamondInLoopArg(_ x2: inout AggStruct) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
      }
    }
}

public func aggStructAccessField() {
    var x2 = AggStruct()
    x2 = AggStruct()
    borrowVal(x2.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.lhs)
    }
}

public func aggStructAccessFieldArg(_ x2: inout AggStruct) {
    borrowVal(x2.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.lhs)
    }
}

public func aggStructConsumeField() {
    var x2 = AggStruct() // expected-error {{'x2' consumed in a loop}}
    // expected-error @-1 {{'x2' consumed more than once}}
    x2 = AggStruct()
    consumeVal(x2.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.lhs) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func aggStructConsumeFieldArg(_ x2: inout AggStruct) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.lhs) // expected-note {{consumed here}}
    }
}

public func aggStructAccessGrandField() {
    var x2 = AggStruct()
    x2 = AggStruct()
    borrowVal(x2.pair.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.pair.lhs)
    }
}

public func aggStructAccessGrandFieldArg(_ x2: inout AggStruct) {
    borrowVal(x2.pair.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.pair.lhs)
    }
}

public func aggStructConsumeGrandField() {
    var x2 = AggStruct() // expected-error {{'x2' consumed in a loop}}
    // expected-error @-1 {{'x2' consumed more than once}}
    x2 = AggStruct()
    consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func aggStructConsumeGrandFieldArg(_ x2: inout AggStruct) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    }
}

//////////////////////////////
// Aggregate Generic Struct //
//////////////////////////////

public struct AggGenericStruct<T>: ~Copyable {
    var lhs: Klass = Klass()
    var rhs: UnsafeRawPointer? = nil
    var pair: KlassPair = KlassPair()

    // FIXME: this is the only use of the generic parameter and it's totally unused!
    // What are we testing here that's not covered by the non-generic one?
    var ptr2: UnsafePointer<T>? = nil

    init() {}

    // Testing that DI ignores normal init. We also get an error on our return
    // value from the function since we do not reinitialize self.
    //
    // TODO: Improve error message!
    init(myInit: UnsafeRawPointer) { // expected-error {{'self' consumed more than once}}
        let x = self // expected-note {{consumed here}}
        let _ = x
    } // expected-note {{consumed again here}}

    // Make sure we can reinitialize successfully.
    init(myInit2: UnsafeRawPointer) {
        let x = self
        let _ = x
        self = AggGenericStruct(myInit: myInit2)
    }

    // Testing delegating init.
    //
    // TODO: Improve error to say need to reinitialize self.lhs before end of
    // function.
    init(myInit3: UnsafeRawPointer) { // expected-error {{'self' consumed more than once}}
        self.init()
        self.rhs = myInit3
        let x = self.lhs // expected-note {{consumed here}}
        let _ = x
    } // expected-note {{consumed again here}}

    init(myInit4: UnsafeRawPointer) {
        self.init()
        self.rhs = myInit4
        let x = self.lhs
        let _ = x
        self = AggGenericStruct(myInit: myInit4)
    }

    init(myInit5: UnsafeRawPointer) {
        self.init()
        self.rhs = myInit5
        let x = self.lhs
        let _ = x
        self.lhs = Klass()
    }
}

public func aggGenericStructSimpleChainTest() {
    var x2 = AggGenericStruct<MyClass>()
    x2 = AggGenericStruct<MyClass>()
    let y2 = x2
    let k2 = y2
    borrowVal(k2)
}

public func aggGenericStructSimpleChainTestArg(_ x2: inout AggGenericStruct<MyClass>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    let y2 = x2 // expected-note {{consumed here}}
    let k2 = y2
    borrowVal(k2)
}

public func aggGenericStructSimpleNonConsumingUseTest() {
    var x2 = AggGenericStruct<MyClass>()
    x2 = AggGenericStruct<MyClass>()
    borrowVal(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestArg(_ x2: inout AggGenericStruct<MyClass>) {
    borrowVal(x2)
}

public func aggGenericStructMultipleNonConsumingUseTest() {
    var x2 = AggGenericStruct<MyClass>()
    x2 = AggGenericStruct<MyClass>()
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func aggGenericStructMultipleNonConsumingUseTestArg(_ x2: inout AggGenericStruct<MyClass>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
}

public func aggGenericStructUseAfterConsume() {
    var x2 = AggGenericStruct<MyClass>() // expected-error {{'x2' consumed more than once}}
    x2 = AggGenericStruct<MyClass>()
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func aggGenericStructUseAfterConsumeArg(_ x2: inout AggGenericStruct<MyClass>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}x
}

public func aggGenericStructDoubleConsume() {
    var x2 = AggGenericStruct<MyClass>()  // expected-error {{'x2' consumed more than once}}
    x2 = AggGenericStruct<MyClass>()
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func aggGenericStructDoubleConsumeArg(_ x2: inout AggGenericStruct<MyClass>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
}

public func aggGenericStructLoopConsume() {
    var x2 = AggGenericStruct<MyClass>() // expected-error {{'x2' consumed in a loop}}
    x2 = AggGenericStruct<MyClass>()
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggGenericStructLoopConsumeArg(_ x2: inout AggGenericStruct<MyClass>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggGenericStructDiamond() {
    var x2 = AggGenericStruct<MyClass>()
    x2 = AggGenericStruct<MyClass>()
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func aggGenericStructDiamondArg(_ x2: inout AggGenericStruct<MyClass>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    if boolValue {
        consumeVal(x2) // expected-note {{consumed here}}
    } else {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggGenericStructDiamondInLoop() {
    var x2 = AggGenericStruct<MyClass>() // expected-error {{'x2' consumed in a loop}}
    // expected-error @-1 {{'x2' consumed more than once}}
    x2 = AggGenericStruct<MyClass>()
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
          // expected-note @-1 {{consumed again here}}
      }
    }
}

public func aggGenericStructDiamondInLoopArg(_ x2: inout AggGenericStruct<MyClass>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
      }
    }
}

public func aggGenericStructAccessField() {
    var x2 = AggGenericStruct<MyClass>()
    x2 = AggGenericStruct<MyClass>()
    borrowVal(x2.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.lhs)
    }
}

public func aggGenericStructAccessFieldArg(_ x2: inout AggGenericStruct<MyClass>) {
    borrowVal(x2.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.lhs)
    }
}

public func aggGenericStructConsumeField() {
    var x2 = AggGenericStruct<MyClass>() // expected-error {{'x2' consumed in a loop}}
    // expected-error @-1 {{'x2' consumed more than once}}
    x2 = AggGenericStruct<MyClass>()
    consumeVal(x2.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.lhs) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func aggGenericStructConsumeFieldArg(_ x2: inout AggGenericStruct<MyClass>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.lhs) // expected-note {{consumed here}}
    }
}

public func aggGenericStructAccessGrandField() {
    var x2 = AggGenericStruct<MyClass>()
    x2 = AggGenericStruct<MyClass>()
    borrowVal(x2.pair.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.pair.lhs)
    }
}

public func aggGenericStructAccessGrandFieldArg(_ x2: inout AggGenericStruct<MyClass>) {
    borrowVal(x2.pair.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.pair.lhs)
    }
}

public func aggGenericStructConsumeGrandField() {
    var x2 = AggGenericStruct<MyClass>() // expected-error {{'x2' consumed in a loop}}
    // expected-error @-1 {{'x2' consumed more than once}}
    x2 = AggGenericStruct<MyClass>()
    consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func aggGenericStructConsumeGrandField2() {
    var x2 = AggGenericStruct<MyClass>() // expected-error {{'x2' consumed more than once}}
    x2 = AggGenericStruct<MyClass>()
    consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
    }
    consumeVal(x2.pair.lhs) // expected-note {{consumed again here}}
}

public func aggGenericStructConsumeGrandFieldArg(_ x2: inout AggGenericStruct<MyClass>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    }
}

////////////////////////////////////////////////////////////
// Aggregate Generic Struct + Generic But Body is Trivial //
////////////////////////////////////////////////////////////

public func aggGenericStructSimpleChainTest<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>()
    x2 = AggGenericStruct<T>()
    let y2 = x2
    let k2 = y2
    borrowVal(k2)
}

public func aggGenericStructSimpleChainTestArg<T>(_ x2: inout AggGenericStruct<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    let y2 = x2 // expected-note {{consumed here}}
    let k2 = y2
    borrowVal(k2)
}

public func aggGenericStructSimpleNonConsumingUseTest<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>()
    x2 = AggGenericStruct<T>()
    borrowVal(x2)
}

public func aggGenericStructSimpleNonConsumingUseTestArg<T>(_ x2: inout AggGenericStruct<T>) {
    borrowVal(x2)
}

public func aggGenericStructMultipleNonConsumingUseTest<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>()
    x2 = AggGenericStruct<T>()
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func aggGenericStructMultipleNonConsumingUseTestArg<T>(_ x2: inout AggGenericStruct<T>) { //expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
}

public func aggGenericStructUseAfterConsume<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>() // expected-error {{'x2' consumed more than once}}
    x2 = AggGenericStruct<T>()
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func aggGenericStructUseAfterConsumeArg<T>(_ x2: inout AggGenericStruct<T>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
}

public func aggGenericStructDoubleConsume<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>() // expected-error {{'x2' consumed more than once}}
    x2 = AggGenericStruct<T>()
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func aggGenericStructDoubleConsumeArg<T>(_ x2: inout AggGenericStruct<T>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
}

public func aggGenericStructLoopConsume<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>() // expected-error {{'x2' consumed in a loop}}
    x2 = AggGenericStruct<T>()
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggGenericStructLoopConsumeArg<T>(_ x2: inout AggGenericStruct<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggGenericStructDiamond<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>()
    x2 = AggGenericStruct<T>()
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func aggGenericStructDiamondArg<T>(_ x2: inout AggGenericStruct<T>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    if boolValue {
        consumeVal(x2) // expected-note {{consumed here}}
    } else {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func aggGenericStructDiamondInLoop<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>() // expected-error {{'x2' consumed more than once}}
    // expected-error @-1 {{'x2' consumed in a loop}}
    x2 = AggGenericStruct<T>()
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
          // expected-note @-1 {{consumed again here}}
      }
    }
}

public func aggGenericStructDiamondInLoopArg<T>(_ x2: inout AggGenericStruct<T>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
      }
    }
}

public func aggGenericStructAccessField<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>()
    x2 = AggGenericStruct<T>()
    borrowVal(x2.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.lhs)
    }
}

public func aggGenericStructAccessFieldArg<T>(_ x2: inout AggGenericStruct<T>) {
    borrowVal(x2.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.lhs)
    }
}

public func aggGenericStructConsumeField<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>() // expected-error {{'x2' consumed in a loop}}
    // expected-error @-1 {{'x2' consumed more than once}}
    x2 = AggGenericStruct<T>()
    consumeVal(x2.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.lhs) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func aggGenericStructConsumeFieldArg<T>(_ x2: inout AggGenericStruct<T>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.lhs) // expected-note {{consumed here}}
    }
}

public func aggGenericStructAccessGrandField<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>()
    x2 = AggGenericStruct<T>()
    borrowVal(x2.pair.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.pair.lhs)
    }
}

public func aggGenericStructAccessGrandFieldArg<T>(_ x2: inout AggGenericStruct<T>) {
    borrowVal(x2.pair.lhs)
    for _ in 0..<1024 {
        borrowVal(x2.pair.lhs)
    }
}

public func aggGenericStructConsumeGrandField<T>(_ x: T.Type) {
    var x2 = AggGenericStruct<T>() // expected-error {{'x2' consumed in a loop}}
    // expected-error @-1 {{'x2' consumed more than once}}
    x2 = AggGenericStruct<T>()
    consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func aggGenericStructConsumeGrandFieldArg<T>(_ x2: inout AggGenericStruct<T>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.pair.lhs) // expected-note {{consumed here}}
    }
}

/////////////////////
// Enum Test Cases //
/////////////////////

public enum EnumTy: ~Copyable {
    case klass(Klass)
    case int(Int)

    func doSomething() -> Bool { true }
}

public func enumSimpleChainTest() {
    var x2 = EnumTy.klass(Klass())
    x2 = EnumTy.klass(Klass())
    let y2 = x2
    let k2 = y2
    borrowVal(k2)
}

public func enumSimpleChainTestArg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    let y2 = x2 // expected-note {{consumed here}}
    let k2 = y2
    borrowVal(k2)
}

public func enumSimpleNonConsumingUseTest() {
    var x2 = EnumTy.klass(Klass())
    x2 = EnumTy.klass(Klass())
    borrowVal(x2)
}

public func enumSimpleNonConsumingUseTestArg(_ x2: inout EnumTy) {
    borrowVal(x2)
}

public func enumMultipleNonConsumingUseTest() {
    var x2 = EnumTy.klass(Klass())
    x2 = EnumTy.klass(Klass())
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func enumMultipleNonConsumingUseTestArg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
}

public func enumUseAfterConsume() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' consumed more than once}}
    x2 = EnumTy.klass(Klass())
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func enumUseAfterConsumeArg(_ x2: inout EnumTy) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
}

public func enumDoubleConsume() {
    var x2 = EnumTy.klass(Klass())  // expected-error {{'x2' consumed more than once}}
    x2 = EnumTy.klass(Klass())
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func enumDoubleConsumeArg(_ x2: inout EnumTy) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
}

public func enumLoopConsume() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' consumed in a loop}}
    x2 = EnumTy.klass(Klass())
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func enumLoopConsumeArg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func enumDiamond() {
    var x2 = EnumTy.klass(Klass())
    x2 = EnumTy.klass(Klass())
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func enumDiamondArg(_ x2: inout EnumTy) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    if boolValue {
        consumeVal(x2) // expected-note {{consumed here}}
    } else {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func enumDiamondInLoop() {
    var x2 = EnumTy.klass(Klass())
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = EnumTy.klass(Klass())
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
          // expected-note @-1 {{consumed again here}}
      }
    }
}

public func enumDiamondInLoopArg(_ x2: inout EnumTy) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
      }
    }
}

public func enumAssignToVar1() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' consumed more than once}}
    x2 = EnumTy.klass(Klass())
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    x3 = EnumTy.klass(Klass())
    consumeVal(x3)
}

public func enumAssignToVar1Arg(_ x2: inout EnumTy) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}

    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
    x3 = EnumTy.klass(Klass())
    consumeVal(x3)
}

public func enumAssignToVar2() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' consumed more than once}}
    x2 = EnumTy.klass(Klass())
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    borrowVal(x3)
}

public func enumAssignToVar2Arg(_ x2: inout EnumTy) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
     // expected-note @-1 {{consumed again here}}
    borrowVal(x3)
}

public func enumAssignToVar3() {
    var x2 = EnumTy.klass(Klass())
    x2 = EnumTy.klass(Klass())
    var x3 = x2
    x3 = EnumTy.klass(Klass())
    consumeVal(x3)
}

public func enumAssignToVar3Arg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}

    var x3 = x2 // expected-note {{consumed here}}
    x3 = EnumTy.klass(Klass())
    consumeVal(x3)
}

public func enumAssignToVar4() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' consumed more than once}}
    x2 = EnumTy.klass(Klass())
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
    consumeVal(x3)
}

public func enumAssignToVar4Arg(_ x2: inout EnumTy) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
    consumeVal(x3)
}

public func enumAssignToVar5() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' used after consume}}
    x2 = EnumTy.klass(Klass())
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = EnumTy.klass(Klass())
    consumeVal(x3)
}

public func enumAssignToVar5Arg(_ x2: inout EnumTy) {
    // expected-error @-1 {{'x2' used after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = EnumTy.klass(Klass())
    consumeVal(x3)
}

public func enumAssignToVar5Arg2(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}

    var x3 = x2 // expected-note {{consumed here}}
    x3 = EnumTy.klass(Klass())
    consumeVal(x3)
}


public func enumPatternMatchIfLet1() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' consumed more than once}}
    x2 = EnumTy.klass(Klass())
    if case let EnumTy.klass(x) = consume x2 { // expected-note {{consumed here}}
        borrowVal(x)
    }
    if case let EnumTy.klass(x) = consume x2 { // expected-note {{consumed again here}}
        borrowVal(x)
    }
}

public func enumPatternMatchIfLet1Arg(_ x2: inout EnumTy) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    if case let EnumTy.klass(x) = consume x2 { // expected-note {{consumed here}}
        borrowVal(x)
    }
    if case let EnumTy.klass(x) = consume x2 { // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
        borrowVal(x)
    }
}

public func enumPatternMatchIfLet2() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' consumed in a loop}}
    x2 = EnumTy.klass(Klass())
    for _ in 0..<1024 {
        if case let EnumTy.klass(x) = consume x2 {  // expected-note {{consumed here}}
            borrowVal(x)
        }
    }
}

public func enumPatternMatchIfLet2Arg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        if case let EnumTy.klass(x) = consume x2 {  // expected-note {{consumed here}}
            borrowVal(x)
        }
    }
}

public func enumPatternMatchSwitch1() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' used after consume}}
    x2 = EnumTy.klass(Klass())
    switch consume x2 { // expected-note {{consumed here}}
    case let EnumTy.klass(k):
        borrowVal(k)
        borrowVal(x2) // expected-note {{used here}}
    case .int:
        break
    }
}

public func enumPatternMatchSwitch1Arg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    switch consume x2 { // expected-note {{consumed here}}
    case let EnumTy.klass(k):
        borrowVal(k)
        borrowVal(x2)
    case .int:
        break
    }
}

public func enumPatternMatchSwitch2() {
    var x2 = EnumTy.klass(Klass())
    x2 = EnumTy.klass(Klass())
    switch consume x2 {
    case let EnumTy.klass(k):
        borrowVal(k)
    case .int:
        break
    }
}

public func enumPatternMatchSwitch2Arg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    switch consume x2 { // expected-note {{consumed here}}
    case let EnumTy.klass(k):
        borrowVal(k)
    case .int:
        break
    }
}

// QOI: We can do better here. We should also flag x2
public func enumPatternMatchSwitch2WhereClause() {
    var x2 = EnumTy.klass(Klass()) // expected-error {{'x2' used after consume}}
    x2 = EnumTy.klass(Klass())
    switch consume x2 { // expected-note {{consumed here}}
    case let EnumTy.klass(k)
           where x2.doSomething(): // expected-note {{used here}}
        borrowVal(k)
    case .int:
        break
    case EnumTy.klass:
        break
    }
}

public func enumPatternMatchSwitch2WhereClauseArg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    switch consume x2 { // expected-note {{consumed here}}
    case let EnumTy.klass(k)
           where x2.doSomething():
        borrowVal(k)
    case .int:
        break
    case EnumTy.klass:
        break
    }
}

public func enumPatternMatchSwitch2WhereClause2() {
    var x2 = EnumTy.klass(Klass())
    x2 = EnumTy.klass(Klass())
    switch consume x2 {
    case let EnumTy.klass(k)
           where boolValue:
        borrowVal(k)
    case .int:
        break
    case EnumTy.klass:
        break
    }
}

public func enumPatternMatchSwitch2WhereClause2Arg(_ x2: inout EnumTy) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    switch consume x2 { // expected-note {{consumed here}}
    case let EnumTy.klass(k)
           where boolValue:
        borrowVal(k)
    case .int:
        break
    case EnumTy.klass:
        break
    }
}

////////////////////////////////
// Address Only Generic Tests //
////////////////////////////////

public func addressOnlyGenericSimpleChainTest<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
               // expected-error @-1 {{'x2' consumed more than once}}
    x2 = x // expected-note {{consumed here}}
    let y2 = x2 // expected-note {{consumed here}}
    let k2 = y2
    let k3 = x2 // expected-note {{consumed again here}}
    let _ = k3
    borrowVal(k2)
}

public func addressOnlyGenericSimpleChainArgTest<T>(_ x2: inout AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    var y2 = x2 // expected-note {{consumed here}}
    y2 = x2 // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
    let k2 = y2
    borrowVal(k2)
}

public func addressOnlyGenericSimpleChainConsumingArgTest<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x2' consumed more than once}}
    var y2 = x2 // expected-note {{consumed here}}
    y2 = x2 // expected-note {{consumed again here}}
    let k2 = y2
    borrowVal(k2)
}

public func addressOnlyGenericSimpleNonConsumingUseTest<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
}

public func addressOnlyGenericSimpleNonConsumingUseArgTest<T>(_ x2: inout AddressOnlyGeneric<T>) {
    borrowVal(x2)
}

public func addressOnlyGenericMultipleNonConsumingUseTest<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func addressOnlyGenericMultipleNonConsumingUseArgTest<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
}

public func addressOnlyGenericMultipleNonConsumingUseArgTest2<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
}

public func addressOnlyGenericMultipleNonConsumingUseArgTest3<T>(_ x2: inout AddressOnlyGeneric<T>) {  // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                                       // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                   // expected-note @-1 {{consumed again here}}
}

public func addressOnlyGenericMultipleNonConsumingUseArgTest4<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x2 = AddressOnlyGeneric<T>()
}

public func addressOnlyGenericMultipleNonConsumingUseConsumingArgTest<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func addressOnlyGenericMultipleNonConsumingUseConsumingArgTest2<T>(_ x2: consuming AddressOnlyGeneric<T>) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
}

public func addressOnlyGenericMultipleNonConsumingUseConsumingArgTest3<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyGenericMultipleNonConsumingUseConsumingArgTest4<T>(_ x2: consuming AddressOnlyGeneric<T>) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x2 = AddressOnlyGeneric<T>()
}

public func addressOnlyGenericUseAfterConsume<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyGenericUseAfterConsumeArg<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                         // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                   // expected-note @-1 {{consumed again here}}
}

public func addressOnlyGenericUseAfterConsumeArg2<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyGenericDoubleConsume<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyGenericDoubleConsumeArg<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                     // expected-note @-1 {{consumed again here}}
}

public func addressOnlyGenericDoubleConsumeArg2<T>(_ x2: consuming AddressOnlyGeneric<T>) {
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyGenericLoopConsume<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed in a loop}}
               // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func addressOnlyGenericLoopConsumeArg<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func addressOnlyGenericLoopConsumeArg2<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed in a loop}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
    x2 = AddressOnlyGeneric<T>()
}

public func addressOnlyGenericLoopConsumeArg3<T>(_ x2: consuming AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed in a loop}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
    x2 = AddressOnlyGeneric<T>()
}

public func addressOnlyGenericDiamond<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func addressOnlyGenericDiamondArg<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                 // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    if boolValue {
        consumeVal(x2) // expected-note {{consumed here}}
    } else {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func addressOnlyGenericDiamondArg2<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func addressOnlyGenericDiamondInLoop<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed in a loop}}
               // expected-error @-1 {{'x2' consumed more than once}}
               // expected-note @-2 {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                           // expected-note @-1 {{consumed again here}}
      }
    }
}

public func addressOnlyGenericDiamondInLoopArg<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                       // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
      }
    }
}

public func addressOnlyGenericDiamondInLoopArg2<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed in a loop}}
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                           // expected-note @-1 {{consumed again here}}
      }
    }
    x2 = AddressOnlyGeneric<T>()
}

public func addressOnlyGenericDiamondInLoopArg3<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}

    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
          // expected-note @-1 {{consumed again here}}
      }
    }
}

public func addressOnlyGenericDiamondInLoopArg4<T>(_ x2: consuming AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed in a loop}}
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                           // expected-note @-1 {{consumed again here}}
      }
    }
    x2 = AddressOnlyGeneric<T>()
}

public func addressOnlyGenericAssignToVar1<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar1Arg<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
    x3 = AddressOnlyGeneric<T>()
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar1Arg2<T>(_ x2: consuming AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    x3 = AddressOnlyGeneric<T>()
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar2<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    borrowVal(x3)
}

public func addressOnlyGenericAssignToVar2Arg<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
    borrowVal(x3)
}

public func addressOnlyGenericAssignToVar2Arg2<T>(_ x2: consuming AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    borrowVal(x3)
}

public func addressOnlyGenericAssignToVar3<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    var x3 = x2
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar3Arg<T>(_ x: borrowing AddressOnlyGeneric<T>, _ x2: inout AddressOnlyGeneric<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                            // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar3Arg2<T>(_ x: borrowing AddressOnlyGeneric<T>, _ x2: inout AddressOnlyGeneric<T>) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                                   // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar4<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar4Arg<T>(_ x2: inout AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2)   // expected-note {{consumed here}}
                // expected-note @-1 {{consumed again here}}
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar4Arg2<T>(_ x2: consuming AddressOnlyGeneric<T>) { // expected-error {{'x2' consumed more than once}}
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2)   // expected-note {{consumed again here}}
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar5<T : P>(_ ty: T.Type) {
    var x2 = AddressOnlyGeneric<T>() // expected-error {{'x2' used after consume}}
    x2 = AddressOnlyGeneric<T>()
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = AddressOnlyGeneric<T>()
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar5Arg<T>(_ x: borrowing AddressOnlyGeneric<T>, _ x2: inout AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x2' used after consume}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyGenericAssignToVar5Arg2<T>(_ x: borrowing AddressOnlyGeneric<T>, _ x2: inout AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
                                                                   // expected-error @-1 {{'x2' used after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
    x2 = AddressOnlyGeneric<T>()
}

// MG: We are calling these consuming uses since I have not taught the checker
// that a use of a copy_addr that is copyable is not a consuming use. I will
// remove them when I fix it in the next commit.
public func addressOnlyGenericAccessAccessField<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    borrowVal(x2.copyable)
    for _ in 0..<1024 {
        borrowVal(x2.copyable)
    }
}

public func addressOnlyGenericAccessAccessField2<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyGeneric<T>()
    borrowVal(x2.moveOnly)
    for _ in 0..<1024 {
        borrowVal(x2.moveOnly)
    }
}

public func addressOnlyGenericAccessAccessFieldArg<T>(_ x2: inout AddressOnlyGeneric<T>) {
    borrowVal(x2.copyable)
    for _ in 0..<1024 {
        borrowVal(x2.copyable)
    }
}

public func addressOnlyGenericAccessAccessFieldArg2<T>(_ x2: inout AddressOnlyGeneric<T>) {
    borrowVal(x2.moveOnly)
    for _ in 0..<1024 {
        borrowVal(x2.moveOnly)
    }
}

public func addressOnlyGenericAccessAccessFieldArg3<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    borrowVal(x2.copyable)
    for _ in 0..<1024 {
        borrowVal(x2.copyable)
    }
}

public func addressOnlyGenericAccessAccessFieldArg4<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    borrowVal(x2.moveOnly)
    for _ in 0..<1024 {
        borrowVal(x2.moveOnly)
    }
}

public func addressOnlyGenericAccessConsumeField<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyGeneric<T>()

    consumeVal(x2.copyable)
    for _ in 0..<1024 {
        consumeVal(x2.copyable)
    }
}

public func addressOnlyGenericAccessConsumeField2<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = AddressOnlyGeneric<T>()

    consumeVal(x2.moveOnly) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.moveOnly) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func addressOnlyGenericAccessConsumeGrandField<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyGeneric<T>()

    consumeVal(x2.copyable.name)
    for _ in 0..<1024 {
        consumeVal(x2.copyable.name)
    }
}

public func addressOnlyGenericAccessConsumeGrandField2<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{'x2' consumed in a loop}}
    x2 = AddressOnlyGeneric<T>()

    consumeVal(x2.moveOnly.k) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.moveOnly.k) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func addressOnlyGenericAccessConsumeGrandField2a<T>(_ x: borrowing AddressOnlyGeneric<T>) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyGeneric<T>()

    consumeVal(x2.moveOnly.copyableK)
    for _ in 0..<1024 {
        consumeVal(x2.moveOnly.copyableK)
    }
}

public func addressOnlyGenericAccessConsumeFieldArg<T>(_ x2: inout AddressOnlyGeneric<T>) {
    consumeVal(x2.copyable)
    for _ in 0..<1024 {
        consumeVal(x2.copyable)
    }
}

public func addressOnlyGenericAccessConsumeFieldArg2<T>(_ x2: inout AddressOnlyGeneric<T>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.moveOnly) // expected-note {{consumed here}}

    for _ in 0..<1024 {
        consumeVal(x2.moveOnly) // expected-note {{consumed here}}
    }
}

public func addressOnlyGenericAccessConsumeFieldArg3<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    consumeVal(x2.copyable)

    for _ in 0..<1024 {
        consumeVal(x2.copyable)
    }
}

public func addressOnlyGenericAccessConsumeFieldArg4<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}
    consumeVal(x2.moveOnly) // expected-note {{consumed here}}

    for _ in 0..<1024 {
        consumeVal(x2.moveOnly) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func addressOnlyGenericAccessConsumeGrandFieldArg<T>(_ x2: inout AddressOnlyGeneric<T>) {
    consumeVal(x2.copyable.name)
    for _ in 0..<1024 {
        consumeVal(x2.copyable.name)
    }
}

public func addressOnlyGenericAccessConsumeGrandFieldArg2<T>(_ x2: inout AddressOnlyGeneric<T>) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.moveOnly.k) // expected-note {{consumed here}}

    for _ in 0..<1024 {
        consumeVal(x2.moveOnly.k) // expected-note {{consumed here}}
    }
}

public func addressOnlyGenericAccessConsumeGrandFieldArg2a<T>(_ x2: inout AddressOnlyGeneric<T>) {
    consumeVal(x2.moveOnly.copyableK)

    for _ in 0..<1024 {
        consumeVal(x2.moveOnly.copyableK)
    }
}

public func addressOnlyGenericAccessConsumeGrandFieldArg3<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    consumeVal(x2.copyable.name)

    for _ in 0..<1024 {
        consumeVal(x2.copyable.name)
    }
}

public func addressOnlyGenericAccessConsumeGrandFieldArg4<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}
    consumeVal(x2.moveOnly.k) // expected-note {{consumed here}}

    for _ in 0..<1024 {
        consumeVal(x2.moveOnly.k) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func addressOnlyGenericAccessConsumeGrandFieldArg4a<T>(_ x2: consuming AddressOnlyGeneric<T>) {
    consumeVal(x2.moveOnly.copyableK)

    for _ in 0..<1024 {
        consumeVal(x2.moveOnly.copyableK)
    }
}

public func addressOnlyGenericBorrowingConsume<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    let _ = x // expected-note {{consumed here}}
}

public func addressOnlyGenericBorrowingConsumeField<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    let _ = x.moveOnly // expected-note {{consumed here}}
}

public func addressOnlyGenericBorrowingConsumeField2<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    let _ = x.copyable
}

public func addressOnlyGenericBorrowingConsumeGrandField<T>(_ x: borrowing AddressOnlyGeneric<T>) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    let _ = x.moveOnly.k // expected-note {{consumed here}}
}

public func addressOnlyGenericLetAccessFieldTest<T>(_ x: consuming AddressOnlyGeneric<T>) {
    let x2 = x

    let _ = x2.moveOnly
}

public func addressOnlyGenericLetAccessFieldTest2<T>(_ x: consuming AddressOnlyGeneric<T>) {
    let x2 = x // expected-error {{'x2' consumed more than once}}

    let _ = x2.moveOnly // expected-note {{consumed here}}
    let _ = x2.moveOnly // expected-note {{consumed again here}}
}

public func addressOnlyGenericLetAccessFieldTest2a<T>(_ x: consuming AddressOnlyGeneric<T>) {
    let x2 = x // expected-error {{'x2' consumed more than once}}

    let _ = x2.moveOnly // expected-note {{consumed here}}
    let _ = x2.moveOnly.k // expected-note {{consumed again here}}
}

public func addressOnlyGenericLetAccessFieldTest2b<T>(_ x: consuming AddressOnlyGeneric<T>) {
    let x2 = x // expected-error {{'x2' consumed more than once}}

    let _ = x2.moveOnly // expected-note {{consumed here}}
    let _ = x2.copyable
    let _ = x2.moveOnly.k // expected-note {{consumed again here}}
}

public func addressOnlyGenericLetAccessFieldTest3<T>(_ x: consuming AddressOnlyGeneric<T>) {
    let x2 = x

    let _ = x2.moveOnly
    let _ = x2.copyable
}

extension AddressOnlyGeneric {
    func testNoUseSelf() { // expected-error {{'self' is borrowed and cannot be consumed}}
        let x = self // expected-note {{consumed here}}
        let _ = x
    }

    mutating func testNoUseSelf2() { // expected-error {{missing reinitialization of inout parameter 'self' after consume}}
        let x = self // expected-note {{consumed here}}
        let _ = x
    }
}

struct AddressOnlyGenericInit<T : P>: ~Copyable {
    var copyable: T
    var moveOnly: NonTrivialStruct
    var moveOnly2: AddressOnlyGeneric<T>

    init(_ input1: T, _ input2: consuming NonTrivialStruct) {
        if boolValue {
            copyable = input1
        } else {
            copyable = T.value
        }
        moveOnly2 = AddressOnlyGeneric<T>(T.value)
        moveOnly = input2
    }

    init(_ input1: T, _ input2: consuming NonTrivialStruct, _ input3 : consuming AddressOnlyGeneric<T>) {
        copyable = input1
        if boolValue {
            moveOnly2 = input3
        } else {
            moveOnly2 = AddressOnlyGeneric<T>(T.value)
        }
        moveOnly = input2
    }
}

///////////////////////////
// Address Only Protocol //
///////////////////////////

public func addressOnlyProtocolSimpleChainTest(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
               // expected-error @-1 {{'x2' consumed more than once}}
    x2 = x // expected-note {{consumed here}}
    let y2 = x2 // expected-note {{consumed here}}
    let k2 = y2
    let k3 = x2 // expected-note {{consumed again here}}
    let _ = k3
    borrowVal(k2)
}

public func addressOnlyProtocolSimpleChainArgTest(_ x2: inout AddressOnlyProtocol) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    var y2 = x2 // expected-note {{consumed here}}
    y2 = x2 // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
    let k2 = y2
    borrowVal(k2)
}

public func addressOnlyProtocolSimpleChainConsumingArgTest(_ x2: consuming AddressOnlyProtocol) {
    // expected-error @-1 {{'x2' consumed more than once}}
    var y2 = x2 // expected-note {{consumed here}}
    y2 = x2 // expected-note {{consumed again here}}
    let k2 = y2
    borrowVal(k2)
}

public func addressOnlyProtocolSimpleNonConsumingUseTest(_ x: borrowing AddressOnlyProtocol) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
}

public func addressOnlyProtocolSimpleNonConsumingUseArgTest(_ x2: inout AddressOnlyProtocol) {
    borrowVal(x2)
}

public func addressOnlyProtocolMultipleNonConsumingUseTest(_ x: borrowing AddressOnlyProtocol) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func addressOnlyProtocolMultipleNonConsumingUseArgTest(_ x2: inout AddressOnlyProtocol) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
}

public func addressOnlyProtocolMultipleNonConsumingUseArgTest2(_ x2: inout AddressOnlyProtocol) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
}

public func addressOnlyProtocolMultipleNonConsumingUseArgTest3(_ x2: inout AddressOnlyProtocol) {  // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                                       // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                   // expected-note @-1 {{consumed again here}}
}

public func addressOnlyProtocolMultipleNonConsumingUseArgTest4(_ x2: inout AddressOnlyProtocol) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x2 = AddressOnlyProtocol()
}

public func addressOnlyProtocolMultipleNonConsumingUseConsumingArgTest(_ x2: consuming AddressOnlyProtocol) {
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2)
}

public func addressOnlyProtocolMultipleNonConsumingUseConsumingArgTest2(_ x2: consuming AddressOnlyProtocol) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
}

public func addressOnlyProtocolMultipleNonConsumingUseConsumingArgTest3(_ x2: consuming AddressOnlyProtocol) {
    // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyProtocolMultipleNonConsumingUseConsumingArgTest4(_ x2: consuming AddressOnlyProtocol) { // expected-error {{'x2' used after consume}}
    borrowVal(x2)
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x2 = AddressOnlyProtocol()
}

public func addressOnlyProtocolUseAfterConsume(_ x: borrowing AddressOnlyProtocol) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = x // expected-note {{consumed here}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyProtocolUseAfterConsumeArg(_ x2: inout AddressOnlyProtocol) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                         // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                   // expected-note @-1 {{consumed again here}}
}

public func addressOnlyProtocolUseAfterConsumeArg2(_ x2: consuming AddressOnlyProtocol) {
    // expected-error @-1 {{'x2' consumed more than once}}
    borrowVal(x2)
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyProtocolDoubleConsume(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyProtocol()
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyProtocolDoubleConsumeArg(_ x2: inout AddressOnlyProtocol) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed here}}
                     // expected-note @-1 {{consumed again here}}
}

public func addressOnlyProtocolDoubleConsumeArg2(_ x2: consuming AddressOnlyProtocol) {
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    consumeVal(x2) // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
}

public func addressOnlyProtocolLoopConsume(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed in a loop}}
               // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyProtocol()
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func addressOnlyProtocolLoopConsumeArg(_ x2: inout AddressOnlyProtocol) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func addressOnlyProtocolLoopConsumeArg2(_ x2: inout AddressOnlyProtocol) { // expected-error {{'x2' consumed in a loop}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
    x2 = AddressOnlyProtocol()
}

public func addressOnlyProtocolLoopConsumeArg3(_ x2: consuming AddressOnlyProtocol) { // expected-error {{'x2' consumed in a loop}}
    for _ in 0..<1024 {
        consumeVal(x2) // expected-note {{consumed here}}
    }
    x2 = AddressOnlyProtocol()
}

public func addressOnlyProtocolDiamond(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyProtocol()
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func addressOnlyProtocolDiamondArg(_ x2: inout AddressOnlyProtocol) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                 // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    if boolValue {
        consumeVal(x2) // expected-note {{consumed here}}
    } else {
        consumeVal(x2) // expected-note {{consumed here}}
    }
}

public func addressOnlyProtocolDiamondArg2(_ x2: consuming AddressOnlyProtocol) {
    if boolValue {
        consumeVal(x2)
    } else {
        consumeVal(x2)
    }
}

public func addressOnlyProtocolDiamondInLoop(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed in a loop}}
               // expected-error @-1 {{'x2' consumed more than once}}
               // expected-note @-2 {{consumed here}}
    x2 = AddressOnlyProtocol()
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                           // expected-note @-1 {{consumed again here}}
      }
    }
}

public func addressOnlyProtocolDiamondInLoopArg(_ x2: inout AddressOnlyProtocol) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                       // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
      }
    }
}

public func addressOnlyProtocolDiamondInLoopArg2(_ x2: inout AddressOnlyProtocol) { // expected-error {{'x2' consumed in a loop}}
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                           // expected-note @-1 {{consumed again here}}
      }
    }
    x2 = AddressOnlyProtocol()
}

public func addressOnlyProtocolDiamondInLoopArg3(_ x2: consuming AddressOnlyProtocol) {
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}

    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
          // expected-note @-1 {{consumed again here}}
      }
    }
}

public func addressOnlyProtocolDiamondInLoopArg4(_ x2: consuming AddressOnlyProtocol) { // expected-error {{'x2' consumed in a loop}}
                                                       // expected-error @-1 {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          consumeVal(x2) // expected-note {{consumed here}}
      } else {
          consumeVal(x2) // expected-note {{consumed here}}
                           // expected-note @-1 {{consumed again here}}
      }
    }
    x2 = AddressOnlyProtocol()
}

public func addressOnlyProtocolAssignToVar1(_ x: borrowing AddressOnlyProtocol) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyProtocol()
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar1Arg(_ x2: inout AddressOnlyProtocol) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
    x3 = AddressOnlyProtocol()
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar1Arg2(_ x2: consuming AddressOnlyProtocol) { // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    x3 = AddressOnlyProtocol()
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar2(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyProtocol()
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    borrowVal(x3)
}

public func addressOnlyProtocolAssignToVar2Arg(_ x2: inout AddressOnlyProtocol) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
    borrowVal(x3)
}

public func addressOnlyProtocolAssignToVar2Arg2(_ x2: consuming AddressOnlyProtocol) { // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x2 // expected-note {{consumed again here}}
    borrowVal(x3)
}

public func addressOnlyProtocolAssignToVar3(_ x: borrowing AddressOnlyProtocol) {
    // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyProtocol()
    var x3 = x2
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar3Arg(_ x: borrowing AddressOnlyProtocol, _ x2: inout AddressOnlyProtocol) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                            // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar3Arg2(_ x: borrowing AddressOnlyProtocol, _ x2: inout AddressOnlyProtocol) { // expected-error {{missing reinitialization of inout parameter 'x2' after consume}}
                                                                   // expected-error @-1 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar4(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consumed here}}
    x2 = AddressOnlyProtocol()
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2) // expected-note {{consumed again here}}
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar4Arg(_ x2: inout AddressOnlyProtocol) { // expected-error {{'x2' consumed more than once}}
                                                      // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2)   // expected-note {{consumed here}}
                // expected-note @-1 {{consumed again here}}
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar4Arg2(_ x2: consuming AddressOnlyProtocol) { // expected-error {{'x2' consumed more than once}}
    let x3 = x2 // expected-note {{consumed here}}
    consumeVal(x2)   // expected-note {{consumed again here}}
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar5<T : P>(_ ty: T.Type) {
    var x2 = AddressOnlyProtocol() // expected-error {{'x2' used after consume}}
    x2 = AddressOnlyProtocol()
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = AddressOnlyProtocol()
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar5Arg(_ x: borrowing AddressOnlyProtocol, _ x2: inout AddressOnlyProtocol) {
    // expected-error @-1 {{'x2' used after consume}}
    // expected-error @-2 {{'x' is borrowed and cannot be consumed}}
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
}

public func addressOnlyProtocolAssignToVar5Arg2(_ x: borrowing AddressOnlyProtocol, _ x2: inout AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
                                                                   // expected-error @-1 {{'x2' used after consume}}
    var x3 = x2 // expected-note {{consumed here}}
    borrowVal(x2) // expected-note {{used here}}
    x3 = x // expected-note {{consumed here}}
    consumeVal(x3)
    x2 = AddressOnlyProtocol()
}

// MG: We are calling these consuming uses since I have not taught the checker
// that a use of a copy_addr that is copyable is not a consuming use. I will
// remove them when I fix it in the next commit.
public func addressOnlyProtocolAccessAccessField(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyProtocol()
    borrowVal(x2.copyable)
    for _ in 0..<1024 {
        borrowVal(x2.copyable)
    }
}

public func addressOnlyProtocolAccessAccessField2(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyProtocol()
    borrowVal(x2.moveOnly)
    for _ in 0..<1024 {
        borrowVal(x2.moveOnly)
    }
}

public func addressOnlyProtocolAccessAccessFieldArg(_ x2: inout AddressOnlyProtocol) {
    borrowVal(x2.copyable)
    for _ in 0..<1024 {
        borrowVal(x2.copyable)
    }
}

public func addressOnlyProtocolAccessAccessFieldArg2(_ x2: inout AddressOnlyProtocol) {
    borrowVal(x2.moveOnly)
    for _ in 0..<1024 {
        borrowVal(x2.moveOnly)
    }
}

public func addressOnlyProtocolAccessAccessFieldArg3(_ x2: consuming AddressOnlyProtocol) {
    borrowVal(x2.copyable)
    for _ in 0..<1024 {
        borrowVal(x2.copyable)
    }
}

public func addressOnlyProtocolAccessAccessFieldArg4(_ x2: consuming AddressOnlyProtocol) {
    borrowVal(x2.moveOnly)
    for _ in 0..<1024 {
        borrowVal(x2.moveOnly)
    }
}

public func addressOnlyProtocolAccessConsumeField(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    x2 = AddressOnlyProtocol()

    consumeVal(x2.copyable)
    for _ in 0..<1024 {
        consumeVal(x2.copyable)
    }
}

public func addressOnlyProtocolAccessConsumeField2(_ x: borrowing AddressOnlyProtocol) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x // expected-note {{consumed here}}
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = AddressOnlyProtocol()

    consumeVal(x2.moveOnly) // expected-note {{consumed here}}
    for _ in 0..<1024 {
        consumeVal(x2.moveOnly) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

public func addressOnlyProtocolAccessConsumeFieldArg(_ x2: inout AddressOnlyProtocol) {
    consumeVal(x2.copyable)
    for _ in 0..<1024 {
        consumeVal(x2.copyable)
    }
}

public func addressOnlyProtocolAccessConsumeFieldArg2(_ x2: inout AddressOnlyProtocol) {
    // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
    // expected-error @-2 {{missing reinitialization of inout parameter 'x2' after consume}}
    consumeVal(x2.moveOnly) // expected-note {{consumed here}}

    for _ in 0..<1024 {
        consumeVal(x2.moveOnly) // expected-note {{consumed here}}
    }
}

public func addressOnlyProtocolAccessConsumeFieldArg3(_ x2: consuming AddressOnlyProtocol) {
    consumeVal(x2.copyable)

    for _ in 0..<1024 {
        consumeVal(x2.copyable)
    }
}

public func addressOnlyProtocolAccessConsumeFieldArg4(_ x2: consuming AddressOnlyProtocol) {
    // expected-error @-1 {{'x2' consumed in a loop}}
    // expected-error @-2 {{'x2' consumed more than once}}
    consumeVal(x2.moveOnly) // expected-note {{consumed here}}

    for _ in 0..<1024 {
        consumeVal(x2.moveOnly) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
}

extension AddressOnlyProtocol {
    func testNoUseSelf() { // expected-error {{'self' is borrowed and cannot be consumed}}
        let x = self // expected-note {{consumed here}}
        let _ = x
    }
}

/////////////////////////////
// MARK: Closure Let Tests //
/////////////////////////////

// These are considered to be non-escaping since we are not storing them into
// memory.

public func closureLetClassUseAfterConsume1() {
    let f = {
        var x2 = Klass() // expected-error {{'x2' consumed more than once}}
        x2 = Klass()
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed again here}}
    }
    f()
}

public func closureLetClassUseAfterConsume2() {
    let f = { () in
        var x2 = Klass() // expected-error {{'x2' consumed more than once}}
        x2 = Klass()
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed again here}}
    }
    f()
}

public func closureLetClassUseAfterConsumeArg(_ argX: inout Klass) {
    // TODO: Fix this
    let f = { (_ x2: inout Klass) in
        // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
        // expected-error @-2 {{'x2' consumed more than once}}
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
    f(&argX)
}

// We do not support captures of vars by closures today.
public func closureLetCaptureClassUseAfterConsume() {
    var x2 = Klass() // expected-error {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-1 {{'x2' consumed more than once}}
    x2 = Klass()
    let f = {
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
    f()
}

public func closureLetCaptureClassUseAfterConsume2() {
    var x2 = Klass() // expected-error {{missing reinitialization of closure capture 'x2' after consume}}
    x2 = Klass()
    let f = {
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
    }
    f()
}

public func closureLetCaptureClassUseAfterConsumeError() {
    var x2 = Klass() // expected-error {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = Klass()
    let f = {
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
    f()
    consumeVal(x2) // expected-note {{consumed here}}
    let x3 = x2 // expected-note {{consumed again here}}
    x2 = Klass()
    let _ = x3
}

public func closureLetCaptureClassArgUseAfterConsume(_ x2: inout Klass) {
    // expected-note @-1 {{'x2' is declared 'inout'}}
    let f = {
        // expected-error @-1 {{escaping closure captures 'inout' parameter 'x2'}}
        borrowVal(x2) // expected-note {{captured here}}
        consumeVal(x2) // expected-note {{captured here}}
        consumeVal(x2) // expected-note {{captured here}}
    }
    f()
}

func closureLetStoreClosureInVariableIsEscape() {
    let s = NonTrivialStruct()

    struct StoreClosure {
        var f: () -> ()
    }

    let f = {
        borrowVal(s)
        consumeVal(s) // expected-error {{noncopyable 's' cannot be consumed when captured by an escaping closure}}
    }
    let c = StoreClosure(f: f)
    _ = c
    consumeVal(s) // expected-error {{noncopyable 's' cannot be consumed when captured by an escaping closure}}
}

/////////////////////////////
// MARK: Closure Var Tests //
/////////////////////////////

// These are considered to be escaping since we are storing them into a
// var. This matches the behavior of how we emit inout diagnostics.

public func closureVarClassUseAfterConsume1() {
    var f = {}
    f = {
        var x2 = Klass() // expected-error {{'x2' consumed more than once}}
        x2 = Klass()
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed again here}}
    }
    f()
}

public func closureVarClassUseAfterConsume2() {
    var f = { () in}
    f = { () in
        var x2 = Klass() // expected-error {{'x2' consumed more than once}}
        x2 = Klass()
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed again here}}
    }
    f()
}

public func closureVarClassUseAfterConsumeArg(_ argX: inout Klass) {
    // TODO: Fix this
    var f = { (_ x2: inout Klass) in}
    f = { (_ x2: inout Klass) in
        // expected-error @-1 {{missing reinitialization of inout parameter 'x2' after consume}}
        // expected-error @-2 {{'x2' consumed more than once}}
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
    f(&argX)
}

// We do not support captures of vars by closures today.
public func closureVarCaptureClassUseAfterConsume() {
    var x2 = Klass()
    x2 = Klass()
    var f = {}
    f = {
        borrowVal(x2)
        consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    }
    f()
}

public func closureVarCaptureClassUseAfterConsume2() {
    var x2 = Klass()
    x2 = Klass()
    var f = {}
    f = {
        borrowVal(x2)
        consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    }
    f()
}

public func closureVarCaptureClassUseAfterConsumeError() {
    var x2 = Klass()
    x2 = Klass()
    var f = {}
    f = {
        borrowVal(x2)
        consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    }
    f()
    consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    let x3 = x2 // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    x2 = Klass()
    let _ = x3
}

public func closureVarCaptureClassArgUseAfterConsume(_ x2: inout Klass) {
    // expected-note @-1 {{'x2' is declared 'inout'}}
    var f = {}
    f = {
        // expected-error @-1 {{escaping closure captures 'inout' parameter 'x2'}}
        borrowVal(x2) // expected-note {{captured here}}
        consumeVal(x2) // expected-note {{captured here}}
        consumeVal(x2) // expected-note {{captured here}}
    }
    f()
}

///////////////////////
// MARK: Defer Tests //
///////////////////////

public func deferCaptureClassUseAfterConsume() {
    var x2 = Klass()
    // expected-error @-1 {{'x2' used after consume}}
    // expected-error @-2 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-3 {{'x2' consumed more than once}}
    x2 = Klass()
    defer { // expected-note {{used here}}
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2)
        // expected-note @-1 {{consumed here}}
        // expected-note @-2 {{consumed again here}}
    }
    consumeVal(x2) // expected-note {{consumed here}}
}

public func deferCaptureClassUseAfterConsume2() {
    var x2 = Klass()
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    // expected-error @-3 {{'x2' used after consume}}
    x2 = Klass()
    defer { // expected-note {{used here}}
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
    let x3 = x2 // expected-note {{consumed here}}
    let _ = x3
}

public func deferCaptureClassArgUseAfterConsume(_ x2: inout Klass) {
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    borrowVal(x2)
    defer {
        borrowVal(x2)
        consumeVal(x2) // expected-note {{consumed here}}
        consumeVal(x2) // expected-note {{consumed here}}
        // expected-note @-1 {{consumed again here}}
    }
    print("foo")
}

public func closureLetAndDeferCaptureClassUseAfterConsume() {
    var x2 = Klass()
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = Klass()
    let f = {
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2)
            // expected-note @-1 {{consumed here}}
            // expected-note @-2 {{consumed again here}}
        }
        print("foo")
    }
    f()
}

public func closureLetAndDeferCaptureClassUseAfterConsume2() {
    var x2 = Klass() // expected-error {{'x2' used after consume}}
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = Klass()
    let f = {
        consumeVal(x2) // expected-note {{consumed here}}
        defer { // expected-note {{used here}}
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2)
            // expected-note @-1 {{consumed here}}
            // expected-note @-2 {{consumed again here}}
        }
        print("foo")
    }
    f()
}

public func closureLetAndDeferCaptureClassUseAfterConsume3() {
    var x2 = Klass() // expected-error {{'x2' used after consume}}
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = Klass()
    let f = {
        consumeVal(x2) // expected-note {{consumed here}}
        defer { // expected-note {{used here}}
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2)
            // expected-note @-1 {{consumed here}}
            // expected-note @-2 {{consumed again here}}
        }
        print("foo")
    }
    f()
    consumeVal(x2)
}

public func closureLetAndDeferCaptureClassArgUseAfterConsume(_ x2: inout Klass) {
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    // expected-note @-3 {{'x2' is declared 'inout'}}
    let f = { // expected-error {{escaping closure captures 'inout' parameter 'x2'}}
        defer { // expected-note {{captured indirectly by this call}}
            borrowVal(x2) // expected-note {{captured here}}
            consumeVal(x2) // expected-note {{captured here}}
            // expected-note @-1 {{consumed here}}
            consumeVal(x2) // expected-note {{captured here}}
            // expected-note @-1 {{consumed here}}
            // expected-note @-2 {{consumed again here}}
        }
        print("foo")
    }
    f()
}

///////////////////////////////////////////
// MARK: Multiple Levels of Let Closures //
///////////////////////////////////////////

public func closureLetAndClosureCaptureClassUseAfterConsume() {
    var x2 = Klass() // expected-error {{'x2' consumed more than once}}
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    x2 = Klass()
    let f = {
        let g = {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
        }
        g()
    }
    f()
}

public func closureLetAndClosureCaptureClassUseAfterConsume2() {
    var x2 = Klass() // expected-error {{'x2' consumed more than once}}
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    x2 = Klass()
    let f = {
        let g = {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
        }
        g()
    }
    f()
    consumeVal(x2)
}


public func closureLetAndClosureCaptureClassArgUseAfterConsume(_ x2: inout Klass) {
    // expected-note @-1 {{'x2' is declared 'inout'}}
    // expected-note @-2 {{'x2' is declared 'inout'}}
    let f = { // expected-error {{escaping closure captures 'inout' parameter 'x2'}}
        let g = { // expected-error {{escaping closure captures 'inout' parameter 'x2'}}
            // expected-note @-1 {{captured indirectly by this call}}
            borrowVal(x2)
            // expected-note @-1 {{captured here}}
            // expected-note @-2 {{captured here}}
            consumeVal(x2)
            // expected-note @-1 {{captured here}}
            // expected-note @-2 {{captured here}}
            consumeVal(x2)
            // expected-note @-1 {{captured here}}
            // expected-note @-2 {{captured here}}
        }
        g()
    }
    f()
}

/////////////////////////////////
// MARK: Defer and Var Closure //
/////////////////////////////////

public func closureVarAndDeferCaptureClassUseAfterConsume(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = Klass() // expected-error {{'x2' consumed more than once}}
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    x2 = x // expected-note {{consumed here}}
    var f = {}
    f = {
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
        }
        print("foo")
    }
    f()
}

public func closureVarAndDeferCaptureClassUseAfterConsume2(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = Klass()
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    x2 = x // expected-note {{consumed here}}
    var f = {}
    f = {
        consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
        }
        print("foo")
    }
    f()
}

// TODO: MG
public func closureVarAndDeferCaptureClassUseAfterConsume3(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = Klass()
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{missing reinitialization of closure capture 'x2' after consume}}
    x2 = x
    // expected-note @-1 {{consumed here}}
    var f = {}
    f = {
        consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
        }
        print("foo")
    }
    f()
    consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
}

public func closureVarAndDeferCaptureClassArgUseAfterConsume(_ x2: borrowing Klass) {
    // expected-error @-1 {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    // expected-error @-2 {{'x2' cannot be captured by an escaping closure since it is a borrowed parameter}}
    var f = {}
    f = {// expected-note {{closure capturing 'x2' here}}
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
        }
        print("foo")
    }
    f()
}

public func closureVarAndDeferCaptureClassOwnedArgUseAfterConsume(_ x2: __owned Klass) {
    // expected-error @-1 {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    var f = {}
    f = {
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
        }
        print("foo")
    }
    f()
}

public func closureVarAndDeferCaptureClassOwnedArgUseAfterConsume2(_ x2: consuming Klass) {
    // expected-error @-1 {{missing reinitialization of closure capture 'x2' after consume}}
    // expected-error @-2 {{'x2' consumed more than once}}
    var f = {}
    f = {
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
        }
        print("foo")
    }
    f()
}

// TODO: MG
public func closureVarAndDeferCaptureClassOwnedArgUseAfterConsume3(_ x2: __owned Klass) {
    // expected-error @-1 {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    var f = {}
    f = {
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
        }
        print("foo")
    }
    f()
    consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
}

public func closureVarAndDeferCaptureClassOwnedArgUseAfterConsume4(_ x2: consuming Klass) {
    // expected-error @-1 {{'x2' consumed more than once}}
    // expected-error @-2 {{missing reinitialization of closure capture 'x2' after consume}}
    var f = {}
    f = {
        defer {
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
            // expected-note @-1 {{consumed again here}}
        }
        print("foo")
    }
    f()
    consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
}

///////////////////////////////////////////
// MARK: Multiple Levels of Var Closures //
///////////////////////////////////////////

public func closureVarAndClosureCaptureClassUseAfterConsume(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = Klass()
    x2 = x // expected-note {{consumed here}}
    var f = {}
    f = {
        var g = {}
        g = {
            borrowVal(x2)
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        }
        g()
        consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    }
    f()
}

public func closureVarAndClosureCaptureClassUseAfterConsume2(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = Klass()
    x2 = x
    // expected-note @-1 {{consumed here}}

    var f = {}
    f = {
        var g = {}
        g = {
            borrowVal(x2)
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        }
        g()
    }
    f()
    consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    let x3 = x2 // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    _ = x3
}

public func closureVarAndClosureCaptureClassUseAfterConsume3(_ x: borrowing Klass) { // expected-error {{'x' is borrowed and cannot be consumed}}
    var x2 = x
    // expected-note @-1 {{consumed here}}
    x2 = x
    // expected-note @-1 {{consumed here}}

    var f = {}
    f = {
        var g = {}
        g = {
            borrowVal(x2)
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        }
        g()
    }
    f()
    consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    let x3 = x2 // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    _ = x3
}

public func closureVarAndClosureCaptureClassArgUseAfterConsume(_ x2: borrowing Klass) {
    // expected-error @-1 {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
    // expected-error @-2 {{'x2' cannot be captured by an escaping closure since it is a borrowed parameter}}
    // expected-error @-3 {{'x2' cannot be captured by an escaping closure since it is a borrowed parameter}}
    var f = {}
    f = {// expected-note {{closure capturing 'x2' here}}
        var g = {}
        g = {// expected-note {{closure capturing 'x2' here}}
            borrowVal(x2)
            consumeVal(x2) // expected-note {{consumed here}}
            consumeVal(x2) // expected-note {{consumed here}}
        }
        g()
    }
    f()
}

public func closureVarAndClosureCaptureClassOwnedArgUseAfterConsume(_ x2: __owned Klass) {
    var f = {}
    f = {
        var g = {}
        g = {
            borrowVal(x2)
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        }
        g()
    }
    f()
}

public func closureVarAndClosureCaptureClassOwnedArgUseAfterConsume2(_ x2: consuming Klass) {
    var f = {}
    f = {
        var g = {}
        g = {
            borrowVal(x2)
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        }
        g()
    }
    f()
}

public func closureVarAndClosureCaptureClassOwnedArgUseAfterConsume3(_ x2: __owned Klass) {
    var f = {}
    f = {
        var g = {}
        g = {
            borrowVal(x2)
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        }
        g()
    }
    f()
    consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
}

public func closureVarAndClosureCaptureClassOwnedArgUseAfterConsume4(_ x2: consuming Klass) {
    var f = {}
    f = {
        var g = {}
        g = {
            borrowVal(x2)
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
            consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
        }
        g()
    }
    f()
    consumeVal(x2) // expected-error {{noncopyable 'x2' cannot be consumed when captured by an escaping closure}}
}

/////////////////////////////
// Tests For Move Operator //
/////////////////////////////

func moveOperatorTest(_ k: __owned Klass) {
    var k2 = k
    // expected-error @-1 {{'k2' consumed more than once}}
    // expected-error @-2 {{'k2' consumed more than once}}
    // expected-error @-3 {{'k2' consumed more than once}}
    k2 = Klass()
    let k3 = consume k2 // expected-note {{consumed here}}
    let _ = consume k2
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}
    _ = k2
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}
    let _ = k2
    // expected-note @-1 {{consumed again here}}
    let _ = k3
}

func moveOperatorTest2(_ k: consuming Klass) {
    var k2 = k
    // expected-error @-1 {{'k2' consumed more than once}}
    // expected-error @-2 {{'k2' consumed more than once}}
    // expected-error @-3 {{'k2' consumed more than once}}
    k2 = Klass()
    let k3 = consume k2 // expected-note {{consumed here}}
    let _ = consume k2
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}
    _ = k2
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}
    let _ = k2
    // expected-note @-1 {{consumed again here}}
    let _ = k3
}

// No diagnostics here.
func moveOperatorTestSuccess() {
  var k = Klass()
  k = Klass()
  let _ = consume k
}

/////////////////////////////////////////
// Black hole initialization test case//
/////////////////////////////////////////

func blackHoleKlassTestCase(_ k: __owned Klass) {
    var k2 = k
    // expected-error @-1 {{'k2' consumed more than once}}
    // expected-error @-2 {{'k2' consumed more than once}}
    // expected-error @-3 {{'k2' consumed more than once}}
    // expected-error @-4 {{'k2' consumed more than once}}
    let _ = k2 // expected-note {{consumed here}}
    let _ = k2 // expected-note {{consumed again here}}

    k2 = Klass()
    var _ = k2 // expected-note {{consumed here}}
    var _ = k2
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}

    _ = k2
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}

    // TODO: Why do we not also get 2 errors here?
    _ = k2
    // expected-note @-1 {{consumed again here}}
}

func blackHoleKlassTestCase2(_ k: consuming Klass) {
    var k2 = k
    // expected-error @-1 {{'k2' consumed more than once}}
    // expected-error @-2 {{'k2' consumed more than once}}
    // expected-error @-3 {{'k2' consumed more than once}}
    // expected-error @-4 {{'k2' consumed more than once}}
    let _ = k2 // expected-note {{consumed here}}
    let _ = k2 // expected-note {{consumed again here}}

    k2 = Klass()
    var _ = k2 // expected-note {{consumed here}}
    var _ = k2
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}

    _ = k2
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}

    // TODO: Why do we not also get 2 errors here?
    _ = k2
    // expected-note @-1 {{consumed again here}}
}

// rdar://109908383
struct NonCopyableStruct: ~Copyable {}
var globFn: () -> () = {}
func forceEscaping(_ esc: @escaping () -> ()) {
    globFn = esc
}
func closureDiagnosticsSimple() {
    var s = NonCopyableStruct()
    let f = {
        _ = consume s  // expected-error {{missing reinitialization of closure capture 's' after consume}} // expected-note {{consumed here}}
        s = NonCopyableStruct()
    }
    forceEscaping(f)
    f()
}

///////////////////////////////////////
// Copyable Type in a Move Only Type //
///////////////////////////////////////

func copyableKlassInAMoveOnlyStruct() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a.copyableK)
    consumeVal(a.copyableK)
}

// This shouldn't error since we are consuming a copyable type.
func copyableKlassInAMoveOnlyStruct2() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a.copyableK)
    consumeVal(a.copyableK)
    consumeVal(a.copyableK)
}

// This shouldn't error since we are working with a copyable type.
func copyableKlassInAMoveOnlyStruct3() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a.copyableK)
    consumeVal(a.copyableK)
    borrowVal(a.copyableK)
}

// This used to error, but no longer errors since we are using a true field
// sensitive model.
func copyableKlassInAMoveOnlyStruct4() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a.copyableK)
    consumeVal(a.copyableK)
    borrowVal(a.nonTrivialStruct2)
}

func copyableStructsInMoveOnlyStructNonConsuming() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a)
    borrowVal(a.nonTrivialStruct2)
    borrowVal(a.nonTrivialCopyableStruct)
    borrowVal(a.nonTrivialCopyableStruct.nonTrivialCopyableStruct2)
    borrowVal(a.nonTrivialCopyableStruct.nonTrivialCopyableStruct2.copyableKlass)
}

func computedMyClassInAMoveOnlyStruct() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a.computedCopyableK)
    consumeVal(a.computedCopyableK)
}

// This shouldn't error since we are consuming a copyable type.
func computedMyClassInAMoveOnlyStruct2() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a.computedCopyableK)
    consumeVal(a.computedCopyableK)
    consumeVal(a.computedCopyableK)
}

// This shouldn't error since we are working with a copyable type.
func computedMyClassInAMoveOnlyStruct3() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a.computedCopyableK)
    consumeVal(a.computedCopyableK)
    borrowVal(a.computedCopyableK)
}

// This used to error, but no longer errors since we are using a true field
// sensitive model.
func computedMyClassInAMoveOnlyStruct4() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a.computedCopyableK)
    consumeVal(a.computedCopyableK)
    borrowVal(a.nonTrivialStruct2)
}

func computedCopyableStructsInMoveOnlyStructNonConsuming() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    borrowVal(a)
    borrowVal(a.computedCopyableK)
    borrowVal(a.nonTrivialCopyableStruct.nonTrivialCopyableStruct2.computedMyClass)
}

///////////////////////////
// Field Sensitive Tests //
///////////////////////////

func fieldSensitiveTestReinitField() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    consumeVal(a.k)
    a.k = Klass()
    borrowVal(a.k)
}

func fieldSensitiveTestReinitFieldMultiBlock1() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    consumeVal(a.k)

    if boolValue {
        a.k = Klass()
        borrowVal(a.k)
    }
}

func fieldSensitiveTestReinitFieldMultiBlock2() {
    var a = NonTrivialStruct() // expected-error {{'a' used after consume}}
    a = NonTrivialStruct()
    consumeVal(a.k) // expected-note {{consumed here}}

    if boolValue {
        a.k = Klass()
    }

    borrowVal(a.k) // expected-note {{used here}}
}

func fieldSensitiveTestReinitFieldMultiBlock3() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    consumeVal(a.k)

    if boolValue {
        a.k = Klass()
    } else {
        a.k = Klass()
    }

    borrowVal(a.k)
}

// This test sees what happens if we partially reinit along one path and do a
// full reinit along another path.
func fieldSensitiveTestReinitFieldMultiBlock4() {
    var a = NonTrivialStruct()
    a = NonTrivialStruct()
    consumeVal(a.k)

    if boolValue {
        a.k = Klass()
    } else {
        a = NonTrivialStruct()
    }

    borrowVal(a.k)
}

func fieldSensitiveTestReinitEnumMultiBlock() {
    var e = NonTrivialEnum.first // expected-error {{'e' used after consume}}
    e = NonTrivialEnum.second(Klass())
    switch consume e { // expected-note {{consumed here}}
    case .second:
        e = NonTrivialEnum.third(NonTrivialStruct())
    default:
        break
    }
    borrowVal(e) // expected-note {{used here}}
}

func fieldSensitiveTestReinitEnumMultiBlock1() {
    var e = NonTrivialEnum.first
    e = NonTrivialEnum.second(Klass())
    switch consume e {
    case .second:
        e = NonTrivialEnum.third(NonTrivialStruct())
    default:
        e = NonTrivialEnum.fourth(MyClass())
    }
    borrowVal(e)
}

func fieldSensitiveTestReinitEnumMultiBlock2() {
    var e = NonTrivialEnum.first
    e = NonTrivialEnum.second(Klass())
    if boolValue {
        switch consume e {
        case .second:
            e = NonTrivialEnum.third(NonTrivialStruct())
        default:
            e = NonTrivialEnum.fourth(MyClass())
        }
    } else {
        e = NonTrivialEnum.third(NonTrivialStruct())
    }
    borrowVal(e)
}

////////////////////////////////////////////
// Multiple Use by Same CallSite TestCase //
////////////////////////////////////////////

func sameCallSiteTestConsumeTwice(_ k: inout Klass) { // expected-error {{'k' consumed more than once}}
    func consumeKlassTwice(_ k: __owned Klass, _ k2: __owned Klass) {}
    consumeKlassTwice(k, k) // expected-error {{overlapping accesses to 'k', but deinitialization requires exclusive access}}
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{consumed again here}}
    // expected-note @-3 {{conflicting access is here}}
    k = Klass()
}

func sameCallSiteConsumeAndUse(_ k: inout Klass) { // expected-error {{'k' used after consume}}
    func consumeKlassAndUseKlass(_ k: __owned Klass, _ k2: borrowing Klass) {}
    consumeKlassAndUseKlass(k, k) // expected-error {{overlapping accesses to 'k', but deinitialization requires exclusive access}}
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{used here}}
    // expected-note @-3 {{conflicting access is here}}
    k = Klass()
}

func inoutAndConsumingUse(_ k: inout Klass) { // expected-error {{'k' used after consume}}
    func consumeKlassAndInoutUseKlass(_ k: __owned Klass, _ k2: inout Klass) {}
    consumeKlassAndInoutUseKlass(k, &k) // expected-error {{overlapping accesses to 'k', but deinitialization requires exclusive access}}
    // expected-note @-1 {{used here}}
    // expected-note @-2 {{consumed here}}
    // expected-note @-3 {{conflicting access is here}}
}

////////////////////////////
// Ref Element Addr Tests //
////////////////////////////

func copyableKlassWithMoveOnlyFieldBorrowValue(_ x: MyClassWithMoveOnlyField) {
    borrowVal(x.moveOnlyVarStruct)
    borrowVal(x.moveOnlyVarStruct)
    borrowVal(x.moveOnlyVarStruct.nonTrivialStruct2)
    borrowVal(x.moveOnlyLetStruct)
    borrowVal(x.moveOnlyLetStruct)
    borrowVal(x.moveOnlyLetStruct.nonTrivialStruct2)
}

func copyableKlassWithMoveOnlyFieldConsumeValue(_ x: MyClassWithMoveOnlyField) {
    consumeVal(x.moveOnlyVarStruct)
    // expected-error @-1 {{cannot consume noncopyable stored property 'x.moveOnlyVarStruct' of a class}}
    consumeVal(x.moveOnlyVarStruct.nonTrivialStruct2) // expected-error {{cannot consume noncopyable stored property 'x.moveOnlyVarStruct' of a class}}
    // TODO: We should place a note on x. We need to make the diagnostic part of
    // this a little smarter.
    consumeVal(x.moveOnlyLetStruct) // expected-error {{cannot consume noncopyable stored property 'x.moveOnlyLetStruct' of a class}}
    consumeVal(x.moveOnlyLetStruct.nonTrivialStruct2) // expected-error {{cannot consume noncopyable stored property 'x.moveOnlyLetStruct' of a class}}
}

func copyableKlassWithMoveOnlyFieldAssignValue(_ x: MyClassWithMoveOnlyField) {
    x.moveOnlyVarStruct = NonTrivialStruct()
    x.moveOnlyVarStruct = NonTrivialStruct()
}

///////////////////////
// Global Addr Tests //
///////////////////////

var varGlobal = NonTrivialStruct()
let letGlobal = NonTrivialStruct()

func moveOnlyGlobalBorrowValue() {
    borrowVal(varGlobal)
    borrowVal(varGlobal.nonTrivialStruct2)
    borrowVal(letGlobal)
    borrowVal(letGlobal.nonTrivialStruct2)
}

func moveOnlyGlobalConsumeValue() {
    consumeVal(varGlobal) // expected-error {{cannot consume noncopyable stored property 'varGlobal' that is global}}
    // TODO: Fix error to say that it is from nonTrivialStruct2
    consumeVal(varGlobal.nonTrivialStruct2) // expected-error {{cannot consume noncopyable stored property 'varGlobal' that is global}}
    consumeVal(letGlobal) // expected-error {{cannot consume noncopyable stored property 'letGlobal' that is global}}
    // TODO: Fix error to say that it is from nonTrivialStruct2
    consumeVal(letGlobal.nonTrivialStruct2) // expected-error {{cannot consume noncopyable stored property 'letGlobal' that is global}}
}

func moveOnlyGlobalAssignValue() {
    varGlobal = NonTrivialStruct()
    varGlobal.nonTrivialStruct2 = NonTrivialStruct2()
}

///////////////////
// InOut Capture //
///////////////////

func inoutCaptureTest() -> (() -> ()) {
    var x = NonTrivialStruct()
    x = NonTrivialStruct()

    func useInOut(_ x: inout NonTrivialStruct) {}
    let f = {
        useInOut(&x)
    }

    borrowVal(x)
    consumeVal(x)
    // expected-error @-1 {{noncopyable 'x' cannot be consumed when captured by an escaping closure}}
    x = NonTrivialStruct()

    let g = {
        x = NonTrivialStruct()
        useInOut(&x)
        consumeVal(x)
        // expected-error @-1 {{noncopyable 'x' cannot be consumed when captured by an escaping closure}}
    }
    g()

    return f
}

func inoutCaptureTestAddressOnlyGeneric<T : P>(_ t: T.Type) -> (() -> ()) {
    var x = AddressOnlyGeneric<T>()
    x = AddressOnlyGeneric<T>()

    func useInOut(_ x: inout AddressOnlyGeneric<T>) {}
    let f = {
        useInOut(&x)
    }

    borrowVal(x)
    consumeVal(x) // expected-error {{noncopyable 'x' cannot be consumed when captured by an escaping closure}}
    x = AddressOnlyGeneric<T>()

    let g = {
        x = AddressOnlyGeneric<T>()
        useInOut(&x)
        consumeVal(x) // expected-error {{noncopyable 'x' cannot be consumed when captured by an escaping closure}}
    }
    g()

    return f
}

////////////////
// Misc Tests //
////////////////

func borrowAndConsumeAtSameTime(_: borrowing NonTrivialStruct, consume _: __owned NonTrivialStruct) {}

func borrowAndConsumeAtSameTimeTest(x: __owned NonTrivialStruct) { // expected-error {{'x' used after consume}}
    borrowAndConsumeAtSameTime(x, consume: x)
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{used here}}
}

func borrowAndConsumeAtSameTimeTest2(x: consuming NonTrivialStruct) { // expected-error {{'x' used after consume}}
    borrowAndConsumeAtSameTime(x, consume: x)
    // expected-note @-1 {{consumed here}}
    // expected-note @-2 {{used here}}
    // expected-error @-3 {{overlapping accesses to 'x', but deinitialization requires exclusive access}}
    // expected-note @-4 {{conflicting access is here}}
}

////////////////
// Yield Test //
////////////////

func yieldTest() {
  // Make sure we do not crash on this.
  struct S: ~Copyable {
    var c = MyClass()
    var c2: MyClass {
      _read { yield c }
    }
  }
}

///////////////////////
// Empty Struct Test //
///////////////////////

struct EmptyStruct: ~Copyable {
  var bool: Bool { false }
  func doSomething() {}
  mutating func doSomething2() {}
  consuming func doSomething3() {}
}

func borrow(_ x: borrowing EmptyStruct) {}
func consume(_ x: consuming EmptyStruct) {}

func testEmptyStruct() {
  func testArg1(_ x: consuming EmptyStruct) {
    borrow(x)
  }

  func testArg2(_ x: consuming EmptyStruct) {
    consume(x)
  }

  func testArg2a(_ x: consuming EmptyStruct) {
    // expected-error @-1 {{'x' consumed more than once}}
    consume(x) // expected-note {{consumed here}}
    consume(x) // expected-note {{consumed again here}}
  }

  func testArg2b(_ x: consuming EmptyStruct) {
    // expected-error @-1 {{'x' used after consume}}
    borrow(x)
    consume(x) // expected-note {{consumed here}}
    borrow(x) // expected-note {{used here}}
  }

  func testArg3(_ x: consuming EmptyStruct) {
    let _ = x
  }

  func testArg3a(_ x: consuming EmptyStruct) {
    // expected-error @-1 {{'x' consumed more than once}}
    let _ = x // expected-note {{consumed here}}
    let _ = x // expected-note {{consumed again here}}
  }

  func testArg4(_ x: consuming EmptyStruct) {
    _ = x
  }

  func testArg4a(_ x: consuming EmptyStruct) {
    // expected-error @-1 {{'x' consumed more than once}}
    _ = x // expected-note {{consumed here}}
    _ = x // expected-note {{consumed again here}}
  }

  func testArg4b(_ x: consuming EmptyStruct) {
    // expected-error @-1 {{'x' consumed more than once}}
    // expected-error @-2 {{'x' consumed more than once}}
    _ = x // expected-note {{consumed here}}
    _ = x // expected-note {{consumed here}}
    // expected-note @-1 {{consumed again here}}
    let _ = x // expected-note {{consumed again here}}
  }

  func testArg5(_ x: consuming EmptyStruct) {
    let y = x
    _ = y
  }

  func testArg6(_ x: consuming EmptyStruct) {
    x.doSomething()
  }

  func testArg7(_ x: consuming EmptyStruct) {
    x.doSomething3()
  }

  func testArg7a(_ x: consuming EmptyStruct) {
    // expected-error @-1 {{'x' consumed more than once}}
    x.doSomething3() // expected-note {{consumed here}}
    x.doSomething3() // expected-note {{consumed again here}}
  }
}

////////////////////////////////////
// Struct Containing Empty Struct //
////////////////////////////////////

// Make sure that we handle a struct that recursively holds an empty struct
// correctly.
struct StructContainingEmptyStruct: ~Copyable {
  var x: EmptyStruct
}

func borrow(_ x: consuming StructContainingEmptyStruct) {}
func consume(_ x: consuming StructContainingEmptyStruct) {}

func testStructContainingEmptyStruct() {
  func testArg1(_ x: consuming StructContainingEmptyStruct) {
    borrow(x)
  }

  func testArg2(_ x: consuming StructContainingEmptyStruct) {
    consume(x)
  }

  func testArg3(_ x: consuming StructContainingEmptyStruct) {
    let _ = x
  }

  func testArg4(_ x: consuming StructContainingEmptyStruct) {
    _ = x
  }

  func testArg5(_ x: consuming StructContainingEmptyStruct) {
    let y = x
    _ = y
  }

  func testArg6(_ x: consuming StructContainingEmptyStruct) {
    x.x.doSomething()
  }

  func testArg7(_ x: consuming StructContainingEmptyStruct) {
    x.x.doSomething3()
  }

  func testArg7a(_ x: consuming StructContainingEmptyStruct) {
    // expected-error @-1 {{'x' consumed more than once}}
    x.x.doSomething3() // expected-note {{consumed here}}
    x.x.doSomething3() // expected-note {{consumed again here}}
  }
}

////////////////////////////////////
// Struct Containing Empty Struct //
////////////////////////////////////

// Make sure that we handle a struct that recursively holds an empty struct
// correctly.
struct StructContainingTwoEmptyStruct: ~Copyable {
  var x: EmptyStruct
  var y: EmptyStruct
}

func borrow(_ x: consuming StructContainingTwoEmptyStruct) {}
func consume(_ x: consuming StructContainingTwoEmptyStruct) {}

func testStructContainingTwoEmptyStruct() {
  func testArg1(_ x: consuming StructContainingTwoEmptyStruct) {
    borrow(x)
  }

  func testArg2(_ x: consuming StructContainingTwoEmptyStruct) {
    consume(x)
  }

  func testArg3(_ x: consuming StructContainingTwoEmptyStruct) {
    let _ = x
  }

  func testArg4(_ x: consuming StructContainingTwoEmptyStruct) {
    _ = x
  }

  func testArg5(_ x: consuming StructContainingTwoEmptyStruct) {
    let y = x
    _ = y
  }

  func testArg6(_ x: consuming StructContainingTwoEmptyStruct) {
    x.x.doSomething()
  }

  func testArg7(_ x: consuming StructContainingTwoEmptyStruct) {
    x.x.doSomething3()
  }

  func testArg8(_ x: consuming StructContainingTwoEmptyStruct) {
    x.y.doSomething3()
  }

  func testArg9(_ x: consuming StructContainingTwoEmptyStruct) {
    x.x.doSomething3()
    x.y.doSomething3()
  }

  func testArg10(_ x: consuming StructContainingTwoEmptyStruct) {
    // expected-error @-1 {{'x' consumed more than once}}
    x.x.doSomething3() // expected-note {{consumed here}}
    x.y.doSomething3()
    x.x.doSomething3() // expected-note {{consumed again here}}
  }
}

//////////////////////////////////
// Enum Containing Empty Struct //
//////////////////////////////////

enum MyEnum2: ~Copyable {
case first(EmptyStruct)
case second(String)
}

enum MyEnum: ~Copyable {
case first(EmptyStruct)
case second(String)
case third(MyEnum2)
}

func borrow(_ x: borrowing MyEnum) {}

func testMyEnum() {
  func test1(_ x: consuming MyEnum) {
    if case let .first(y) = consume x {
      _ = y
    }
  }

  func test1a(_ x: consuming MyEnum) { // expected-error {{'x' consumed more than once}}
    if case let .first(y) = consume x { // expected-note {{consumed here}}
      _ = consume x // expected-note {{consumed again here}}
      _ = y
    }
  }

  func test1b(_ x: consuming MyEnum) { // expected-error {{'x' consumed more than once}}
    if case let .first(y) = consume x { // expected-note {{consumed here}}
      _ = y
    }
    _ = consume x // expected-note {{consumed again here}}
  }

  func test2(_ x: consuming MyEnum) {
    if case let .third(.first(y)) = consume x {
      _ = y
    }
  }

  func test2a(_ x: consuming MyEnum) { // expected-error {{'x' consumed more than once}}
    if case let .third(.first(y)) = consume x { // expected-note {{consumed here}}
      _ = consume x // expected-note {{consumed again here}}
      _ = y
    }
  }

  func test2b(_ x: consuming MyEnum) { // expected-error {{'x' consumed more than once}}
    if case let .third(.first(y)) = consume x { // expected-note {{consumed here}}
      _ = y
    }
    _ = consume x // expected-note {{consumed again here}}
  }

  func test2c(_ x: consuming MyEnum) { // expected-error {{'x' used after consume}}
    if case let .third(.first(y)) = consume x { // expected-note {{consumed here}}
      _ = y
    }
    borrow(x) // expected-note {{used here}}
  }

  func test3(_ x: consuming MyEnum) {
    switch consume x {
    case let .first(y):
      _ = y
      break
    default:
      break
    }
  }

  func test3a(_ x: consuming MyEnum) { // expected-error {{'x' consumed more than once}}
    switch consume x { // expected-note {{consumed here}}
    case let .first(y):
      _ = y
      break
    default:
      break
    }
    _ = consume x // expected-note {{consumed again here}}
  }

  func test4(_ x: consuming MyEnum) {
    switch consume x {
    case let .third(.first(y)):
      _ = y
      break
    default:
      break
    }
  }

  func test4a(_ x: consuming MyEnum) { // expected-error {{'x' consumed more than once}}
    switch consume x { // expected-note {{consumed here}}
    case let .third(.first(y)):
      _ = y
      break
    default:
      break
    }
    _ = consume x // expected-note {{consumed again here}}
  }
}

////////////////////////
// MARK: Setter Tests //
////////////////////////

public class NonFinalMyClassWithMoveOnlyField {
    var moveOnlyVarStruct = NonTrivialStruct()
    let moveOnlyLetStruct = NonTrivialStruct()
    var moveOnlyVarProt = AddressOnlyProtocol()
    let moveOnlyLetProt = AddressOnlyProtocol()
}

//////////////////////
// MARK: Misc Tests //
//////////////////////

// For misc tests associated with specific radars.
func assignableButNotConsumableEndAccessImplicitLifetimeTest(_ x: MyClassWithMoveOnlyField) {
    if boolValue {
        x.moveOnlyVarStruct.nonTrivialStruct2 = NonTrivialStruct2()
    }
}
