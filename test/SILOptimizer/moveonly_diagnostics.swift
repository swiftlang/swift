// RUN: %target-swift-emit-sil -verify -enable-experimental-move-only %s

//////////////////
// Declarations //
//////////////////

@_moveOnly
public class Klass {
    var intField: Int
    var k: Klass?
    init() {
        k = Klass()
        intField = 5
    }
}

var boolValue: Bool { return true }

public func classUseMoveOnlyWithoutEscaping(_ x: Klass) {
}
public func classConsume(_ x: __owned Klass) {
}

@_moveOnly
public struct NonTrivialStruct {
    var k = Klass()
}

public func nonConsumingUseNonTrivialStruct(_ s: NonTrivialStruct) {}

@_moveOnly
public enum NonTrivialEnum {
    case first
    case second(Klass)
    case third(NonTrivialStruct)
}

public func nonConsumingUseNonTrivialEnum(_ e : NonTrivialEnum) {}

@_moveOnly
public final class FinalKlass {
    var k: Klass? = nil
}

///////////
// Tests //
///////////

/////////////////
// Class Tests //
/////////////////

public func classSimpleChainTest(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
               // expected-error @-1 {{'x2' consumed more than once}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    let k3 = x2 // expected-note {{consuming use}}
    let _ = k3
    classUseMoveOnlyWithoutEscaping(k2)
}

public func classSimpleChainArgTest(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    classUseMoveOnlyWithoutEscaping(k2)
}

public func classSimpleChainOwnedArgTest(_ x2: __owned Klass) {
    let y2 = x2
    let k2 = y2
    classUseMoveOnlyWithoutEscaping(k2)
}

public func classSimpleNonConsumingUseTest(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2)
}

public func classSimpleNonConsumingUseArgTest(_ x2: Klass) {
    classUseMoveOnlyWithoutEscaping(x2)
}

public func classSimpleNonConsumingUseOwnedArgTest(_ x2: __owned Klass) {
    classUseMoveOnlyWithoutEscaping(x2)
}

public func classMultipleNonConsumingUseTest(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2)
    classUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func classMultipleNonConsumingUseArgTest(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    classUseMoveOnlyWithoutEscaping(x2)
    classUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func classMultipleNonConsumingUseOwnedArgTest(_ x2: __owned Klass) {
    classUseMoveOnlyWithoutEscaping(x2)
    classUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func classUseAfterConsume(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2)
    classConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func classUseAfterConsumeArg(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    classUseMoveOnlyWithoutEscaping(x2)
    classConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func classUseAfterConsumeOwnedArg(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    classUseMoveOnlyWithoutEscaping(x2)
    classConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func classDoubleConsume(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consuming use}}
    classConsume(x2) // expected-note {{consuming use}}
    classConsume(x2) // expected-note {{consuming use}}
}

public func classDoubleConsumeArg(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    classConsume(x2) // expected-note {{consuming use}}
    classConsume(x2) // expected-note {{consuming use}}
}

public func classDoubleConsumeOwnedArg(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    classConsume(x2) // expected-note {{consuming use}}
    classConsume(x2) // expected-note {{consuming use}}
}

public func classLoopConsume(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
        classConsume(x2) // expected-note {{consuming use}}
    }
}

public func classLoopConsumeArg(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        classConsume(x2) // expected-note {{consuming use}}
    }
}

public func classLoopConsumeOwnedArg(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        classConsume(x2) // expected-note {{consuming use}}
    }
}

public func classDiamond(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    if boolValue {
        classConsume(x2)
    } else {
        classConsume(x2)
    }
}

public func classDiamondArg(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        classConsume(x2) // expected-note {{consuming use}}
    } else {
        classConsume(x2) // expected-note {{consuming use}}
    }
}

public func classDiamondOwnedArg(_ x2: __owned Klass) {
    if boolValue {
        classConsume(x2)
    } else {
        classConsume(x2)
    }
}

public func classDiamondInLoop(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
      if boolValue {
          classConsume(x2) // expected-note {{consuming use}}
      } else {
          classConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func classDiamondInLoopArg(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
      if boolValue {
          classConsume(x2) // expected-note {{consuming use}}
      } else {
          classConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func classDiamondInLoopOwnedArg(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          classConsume(x2) // expected-note {{consuming use}}
      } else {
          classConsume(x2) // expected-note {{consuming use}}
      }
    }
}

// TODO: We shouldn't be erroring on x3.
public func classAssignToVar1(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

// TODO: We shouldn't see a consuming use on x3.
public func classAssignToVar1Arg(_ x: Klass, _ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

// NOTE: print(x3) shouldn't be marked! This is most likely due to some form of
// load forwarding. We may need to make predictable mem opts more conservative
// with move only var.
public func classAssignToVar1OwnedArg(_ x: Klass, _ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func classAssignToVar2(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x3)
}

public func classAssignToVar2Arg(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x3)
}

public func classAssignToVar2OwnedArg(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x3)
}

// NOTE: print(x3) should not be marked.
public func classAssignToVar3(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    var x3 = x2
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

// NOTE: print(x3) is a bug.
public func classAssignToVar3Arg(_ x: Klass, _ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                            // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

// This is a bug around print(x3)
public func classAssignToVar3OwnedArg(_ x: Klass, _ x2: __owned Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func classAssignToVar4(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func classAssignToVar4Arg(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func classAssignToVar4OwnedArg(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func classAssignToVar5(_ x: Klass) {  // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    classUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func classAssignToVar5Arg(_ x: Klass, _ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                            // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    classUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func classAssignToVar5OwnedArg(_ x: Klass, _ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
                                                                         // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    classUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func classAccessAccessField(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2.k!)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.k!)
    }
}

public func classAccessAccessFieldArg(_ x2: Klass) {
    classUseMoveOnlyWithoutEscaping(x2.k!)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.k!)
    }
}

public func classAccessAccessFieldOwnedArg(_ x2: __owned Klass) {
    classUseMoveOnlyWithoutEscaping(x2.k!)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.k!)
    }
}

public func classAccessConsumeField(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    // Since a class is a reference type, we do not emit an error here.
    classConsume(x2.k!)
    for _ in 0..<1024 {
        classConsume(x2.k!)
    }
}

public func classAccessConsumeFieldArg(_ x2: Klass) {
    // Since a class is a reference type, we do not emit an error here.
    classConsume(x2.k!)
    for _ in 0..<1024 {
        classConsume(x2.k!)
    }
}

public func classAccessConsumeFieldOwnedArg(_ x2: __owned Klass) {
    // Since a class is a reference type, we do not emit an error here.
    classConsume(x2.k!)
    for _ in 0..<1024 {
        classConsume(x2.k!)
    }
}

extension Klass {
    func testNoUseSelf() { // expected-error {{'self' has guaranteed ownership but was consumed}}
        let x = self // expected-note {{consuming use}}
        let _ = x
    }
}

/////////////////
// Final Class //
/////////////////

public func finalClassUseMoveOnlyWithoutEscaping(_ x: FinalKlass) {
}
public func finalClassConsume(_ x: __owned FinalKlass) {
}

public func finalClassSimpleChainTest(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let y2 = x2
    let k2 = y2
    finalClassUseMoveOnlyWithoutEscaping(k2)
}

public func finalClassSimpleChainTestArg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let y2 = x2 // expected-note {{consuming use}}
    let k2 = y2
    finalClassUseMoveOnlyWithoutEscaping(k2)
}

public func finalClassSimpleChainTestOwnedArg(_ x2: __owned FinalKlass) {
    let y2 = x2
    let k2 = y2
    finalClassUseMoveOnlyWithoutEscaping(k2)
}

public func finalClassSimpleNonConsumingUseTest(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    finalClassUseMoveOnlyWithoutEscaping(x2)
}

public func finalClassSimpleNonConsumingUseTestArg(_ x2: FinalKlass) {
    finalClassUseMoveOnlyWithoutEscaping(x2)
}

public func finalClassSimpleNonConsumingUseTestOwnedArg(_ x2: __owned FinalKlass) {
    finalClassUseMoveOnlyWithoutEscaping(x2)
}

public func finalClassMultipleNonConsumingUseTest(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    finalClassUseMoveOnlyWithoutEscaping(x2)
    finalClassUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func finalClassMultipleNonConsumingUseTestArg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    finalClassUseMoveOnlyWithoutEscaping(x2)
    finalClassUseMoveOnlyWithoutEscaping(x2)
    print(x2) // expected-note {{consuming use}}
}

public func finalClassMultipleNonConsumingUseTestownedArg(_ x2: __owned FinalKlass) {
    finalClassUseMoveOnlyWithoutEscaping(x2)
    finalClassUseMoveOnlyWithoutEscaping(x2)
    print(x2)
}

public func finalClassUseAfterConsume(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    finalClassUseMoveOnlyWithoutEscaping(x2)
    finalClassConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func finalClassUseAfterConsumeArg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    finalClassUseMoveOnlyWithoutEscaping(x2)
    finalClassConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func finalClassUseAfterConsumeOwnedArg(_ x2: __owned FinalKlass) { // expected-error {{'x2' consumed more than once}}
    finalClassUseMoveOnlyWithoutEscaping(x2)
    finalClassConsume(x2) // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
}

public func finalClassDoubleConsume(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x  // expected-error {{'x2' consumed more than once}}
                // expected-note @-1 {{consuming use}}
    finalClassConsume(x2) // expected-note {{consuming use}}
    finalClassConsume(x2) // expected-note {{consuming use}}
}

public func finalClassDoubleConsumeArg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    finalClassConsume(x2) // expected-note {{consuming use}}
    finalClassConsume(x2) // expected-note {{consuming use}}
}

public func finalClassDoubleConsumeownedArg(_ x2: __owned FinalKlass) { // expected-error {{'x2' consumed more than once}}
    finalClassConsume(x2) // expected-note {{consuming use}}
    finalClassConsume(x2) // expected-note {{consuming use}}
}

public func finalClassLoopConsume(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
        finalClassConsume(x2) // expected-note {{consuming use}}
    }
}

public func finalClassLoopConsumeArg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
        finalClassConsume(x2) // expected-note {{consuming use}}
    }
}

public func finalClassLoopConsumeOwnedArg(_ x2: __owned FinalKlass) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
        finalClassConsume(x2) // expected-note {{consuming use}}
    }
}

public func finalClassDiamond(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    if boolValue {
        finalClassConsume(x2)
    } else {
        finalClassConsume(x2)
    }
}

public func finalClassDiamondArg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if boolValue {
        finalClassConsume(x2) // expected-note {{consuming use}}
    } else {
        finalClassConsume(x2) // expected-note {{consuming use}}
    }
}

public func finalClassDiamondOwnedArg(_ x2: __owned FinalKlass) {
    if boolValue {
        finalClassConsume(x2)
    } else {
        finalClassConsume(x2)
    }
}

public func finalClassDiamondInLoop(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    for _ in 0..<1024 {
      if boolValue {
          finalClassConsume(x2) // expected-note {{consuming use}}
      } else {
          finalClassConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func finalClassDiamondInLoopArg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    for _ in 0..<1024 {
      if boolValue {
          finalClassConsume(x2) // expected-note {{consuming use}}
      } else {
          finalClassConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func finalClassDiamondInLoopOwnedArg(_ x2: __owned FinalKlass) { // expected-error {{'x2' consumed more than once}}
    for _ in 0..<1024 {
      if boolValue {
          finalClassConsume(x2) // expected-note {{consuming use}}
      } else {
          finalClassConsume(x2) // expected-note {{consuming use}}
      }
    }
}

public func finalClassAssignToVar1(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAssignToVar1Arg(_ x: FinalKlass, _ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                                           // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAssignToVar1OwnedArg(_ x: FinalKlass, _ x2: __owned FinalKlass) { // expected-error {{'x2' consumed more than once}}
                                                                                        // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAssignToVar2(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    finalClassUseMoveOnlyWithoutEscaping(x3)
}

public func finalClassAssignToVar2Arg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    finalClassUseMoveOnlyWithoutEscaping(x3)
}

public func finalClassAssignToVar2OwnedArg(_ x2: __owned FinalKlass) { // expected-error {{'x2' consumed more than once}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    finalClassUseMoveOnlyWithoutEscaping(x3)
}

public func finalClassAssignToVar3(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    var x3 = x2
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAssignToVar3Arg(_ x: FinalKlass, _ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                                           // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAssignToVar3OwnedArg(_ x: FinalKlass, _ x2: __owned FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAssignToVar4(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func finalClassAssignToVar4Arg(_ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func finalClassAssignToVar4OwnedArg(_ x2: __owned FinalKlass) { // expected-error {{'x2' consumed more than once}}
    let x3 = x2 // expected-note {{consuming use}}
    print(x2) // expected-note {{consuming use}}
    print(x3)
}

public func finalClassAssignToVar5(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    finalClassUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAssignToVar5Arg(_ x: FinalKlass, _ x2: FinalKlass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                                           // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    finalClassUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAssignToVar5OwnedArg(_ x: FinalKlass, _ x2: __owned FinalKlass) { // expected-error {{'x2' consumed more than once}}
                                                                                        // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    finalClassUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func finalClassAccessField(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    classUseMoveOnlyWithoutEscaping(x2.k!)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.k!)
    }
}

public func finalClassAccessFieldArg(_ x2: FinalKlass) {
    classUseMoveOnlyWithoutEscaping(x2.k!)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.k!)
    }
}

public func finalClassAccessFieldOwnedArg(_ x2: __owned FinalKlass) {
    classUseMoveOnlyWithoutEscaping(x2.k!)
    for _ in 0..<1024 {
        classUseMoveOnlyWithoutEscaping(x2.k!)
    }
}

public func finalClassConsumeField(_ x: FinalKlass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}

    // No diagnostic here since class is a reference type and we are not copying
    // the class, we are copying its field.
    classConsume(x2.k!)
    for _ in 0..<1024 {
        classConsume(x2.k!)
    }
}

public func finalClassConsumeFieldArg(_ x2: FinalKlass) {
    // No diagnostic here since class is a reference type and we are not copying
    // the class, we are copying its field.
    classConsume(x2.k!)
    for _ in 0..<1024 {
        classConsume(x2.k!)
    }
}

public func finalClassConsumeFieldArg(_ x2: __owned FinalKlass) {
    // No diagnostic here since class is a reference type and we are not copying
    // the class, we are copying its field.
    classConsume(x2.k!)
    for _ in 0..<1024 {
        classConsume(x2.k!)
    }
}

//////////////////////
// Aggregate Struct //
//////////////////////

@_moveOnly
public struct KlassPair {
    var lhs: Klass
    var rhs: Klass
}

@_moveOnly
public struct AggStruct {
    var lhs: Klass
    var center: Int32
    var rhs: Klass
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
    var lhs: Klass
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
    case klass(Klass)
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
    print(x3) // expected-note {{consuming use}}
}

public func enumAssignToVar1Arg(_ x: EnumTy, _ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                             // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func enumAssignToVar1OwnedArg(_ x: EnumTy, _ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
                                                                          // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
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
    print(x3) // expected-note {{consuming use}}
}

public func enumAssignToVar3Arg(_ x: EnumTy, _ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                             // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func enumAssignToVar3OwnedArg(_ x: EnumTy, _ x2: __owned EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
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
    print(x3) // expected-note {{consuming use}}
}

public func enumAssignToVar5Arg(_ x: EnumTy, _ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
                                                             // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    enumUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func enumAssignToVar5OwnedArg(_ x: EnumTy, _ x2: __owned EnumTy) { // expected-error {{'x2' consumed more than once}}
                                                                          // expected-error @-1 {{'x' has guaranteed ownership but was consumed}}
    var x3 = x2 // expected-note {{consuming use}}
    // TODO: Need to mark this as the lifetime extending use. We fail
    // appropriately though.
    enumUseMoveOnlyWithoutEscaping(x2)
    x3 = x // expected-note {{consuming use}}
    print(x3) // expected-note {{consuming use}}
}

public func enumPatternMatchIfLet1(_ x: EnumTy) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x)
    }
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x)
    }
}

public func enumPatternMatchIfLet1Arg(_ x2: EnumTy) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x)
    }
    if case let .klass(x) = x2 { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x)
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

public func closureClassUseAfterConsume1(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let f = { // expected-note {{consuming use}}
        let x2 = x // expected-error {{'x2' consumed more than once}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f()
}

public func closureClassUseAfterConsume2(_ argX: Klass) {
    let f = { (_ x: Klass) in // expected-error {{'x' has guaranteed ownership but was consumed}}
        let x2 = x // expected-error {{'x2' consumed more than once}}
                   // expected-note @-1 {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f(argX)
}

public func closureClassUseAfterConsumeArg(_ argX: Klass) {
    // TODO: Fix this
    let f = { (_ x2: Klass) in // expected-error {{'x2' has guaranteed ownership but was consumed}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2) // expected-note {{consuming use}}
        print(x2) // expected-note {{consuming use}}
    }
    f(argX)
}

public func closureCaptureClassUseAfterConsume(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let f = {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    f()
}

public func closureCaptureClassUseAfterConsumeError(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    let f = { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    f()
    let x3 = x2 // expected-note {{consuming use}}
    let _ = x3
}

public func closureCaptureClassArgUseAfterConsume(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let f = { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    f()
}

public func closureCaptureClassOwnedArgUseAfterConsume(_ x2: __owned Klass) {
    let f = {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    f()
}

public func closureCaptureClassOwnedArgUseAfterConsume2(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    let f = { // expected-note {{consuming use}}
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    f()
    let x3 = x2 // expected-note {{consuming use}}
    let _ = x3
}

public func deferCaptureClassUseAfterConsume(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    print(x) // expected-note {{consuming use}}
}

public func deferCaptureClassUseAfterConsume2(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    // TODO: Defer error is b/c we have to lifetime extend x2 for the defer. The
    // use is a guaranteed use, so we don't emit an error on that use.
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    let x3 = x2 // expected-note {{consuming use}}
    let _ = x3
}

public func deferCaptureClassArgUseAfterConsume(_ x2: Klass) {
    classUseMoveOnlyWithoutEscaping(x2)
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    print("foo")
}

public func deferCaptureClassOwnedArgUseAfterConsume(_ x2: __owned Klass) {
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    print("foo")
}

public func deferCaptureClassOwnedArgUseAfterConsume2(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    defer {
        classUseMoveOnlyWithoutEscaping(x2)
        classConsume(x2)
        print(x2)
    }
    print(x2) // expected-note {{consuming use}}
}

public func closureAndDeferCaptureClassUseAfterConsume(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let f = {
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        print("foo")
    }
    f()
}

public func closureAndDeferCaptureClassUseAfterConsume2(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let f = {
        classConsume(x2)
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        print("foo")
    }
    f()
}

public func closureAndDeferCaptureClassUseAfterConsume3(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    let f = { // expected-note {{consuming use}}
        classConsume(x2)
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        print("foo")
    }
    f()
    classConsume(x2) // expected-note {{consuming use}}
}

public func closureAndDeferCaptureClassArgUseAfterConsume(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let f = { // expected-note {{consuming use}}
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        print("foo")
    }
    f()
}

public func closureAndDeferCaptureClassOwnedArgUseAfterConsume(_ x2: __owned Klass) {
    let f = {
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        print("foo")
    }
    f()
}

public func closureAndDeferCaptureClassOwnedArgUseAfterConsume2(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    let f = { // expected-note {{consuming use}}
        defer {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        print("foo")
    }
    f()
    print(x2) // expected-note {{consuming use}}
}

public func closureAndClosureCaptureClassUseAfterConsume(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-note {{consuming use}}
    let f = {
        let g = {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        g()
    }
    f()
}

public func closureAndClosureCaptureClassUseAfterConsume2(_ x: Klass) { // expected-error {{'x' has guaranteed ownership but was consumed}}
    let x2 = x // expected-error {{'x2' consumed more than once}}
               // expected-note @-1 {{consuming use}}
    let f = { // expected-note {{consuming use}}
        let g = {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        g()
    }
    f()
    print(x2) // expected-note {{consuming use}}
}


public func closureAndClosureCaptureClassArgUseAfterConsume(_ x2: Klass) { // expected-error {{'x2' has guaranteed ownership but was consumed}}
    let f = { // expected-note {{consuming use}}
        let g = {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        g()
    }
    f()
}

public func closureAndClosureCaptureClassOwnedArgUseAfterConsume(_ x2: __owned Klass) {
    let f = {
        let g = {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        g()
    }
    f()
}

public func closureAndClosureCaptureClassOwnedArgUseAfterConsume2(_ x2: __owned Klass) { // expected-error {{'x2' consumed more than once}}
    let f = { // expected-note {{consuming use}}
        let g = {
            classUseMoveOnlyWithoutEscaping(x2)
            classConsume(x2)
            print(x2)
        }
        g()
    }
    f()
    print(x2) // expected-note {{consuming use}}
}
