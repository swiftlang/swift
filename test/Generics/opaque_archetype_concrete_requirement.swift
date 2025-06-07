// RUN: %target-swift-frontend -typecheck -verify %s -target %target-swift-5.1-abi-triple -debug-generic-signatures -enable-requirement-machine-opaque-archetypes 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -target %target-swift-5.1-abi-triple -enable-requirement-machine-opaque-archetypes

protocol P1 {
  associatedtype T : P2
  associatedtype U
}

struct S_P1 : P1 {
  typealias T = S_P2
  typealias U = Int
}

protocol P2 {}

struct S_P2 : P2 {}

protocol P {
  associatedtype T

  var t: T { get }
}

struct DefinesOpaqueP1 : P {
  var t: some P1 {
    return S_P1()
  }
}

struct ConcreteHasP<T : P1, TT : P2, TU> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=ConcreteHasP
// CHECK-NEXT: Generic signature: <T, TT, TU where T == some P1, TT == (some P1).[P1]T, TU == (some P1).[P1]U>
extension ConcreteHasP where T == DefinesOpaqueP1.T, TT == T.T, TU == T.U {
  func checkSameType1(_ t: TT) -> DefinesOpaqueP1.T.T { return t }
  func checkSameType2(_ u: TU) -> DefinesOpaqueP1.T.U { return u }

  func checkSameType3(_ t: T.T) -> DefinesOpaqueP1.T.T { return t }
  func checkSameType4(_ u: T.U) -> DefinesOpaqueP1.T.U { return u }
}

struct G<T> {}

protocol HasP {
  associatedtype T : P1
  associatedtype U
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=HasP
// CHECK-NEXT: Generic signature: <Self where Self : HasP, Self.[HasP]T == some P1, Self.[HasP]U == G<(some P1).[P1]T>>
extension HasP where T == DefinesOpaqueP1.T, U == G<T.T> {
  func checkSameType1(_ t: T.T) -> DefinesOpaqueP1.T.T { return t }
  func checkSameType2(_ u: T.U) -> DefinesOpaqueP1.T.U { return u }
}
