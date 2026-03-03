// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -target %target-swift-5.1-abi-triple -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

import resilient_struct

protocol P {}
protocol Q: AnyObject {}

extension String: P {}
struct AddrOnly: P { var field: P }

class C: Q {}

// CHECK-LABEL: sil hidden {{.*}}11valueToAddr1xQr
func valueToAddr(x: String) -> some P {
  // CHECK: bb0([[ARG0:%.*]] : $*String, [[ARG1:%.*]] : @guaranteed $String):
  // CHECK: [[VALUE_COPY:%.*]] = copy_value [[ARG1]]
  // CHECK: store [[VALUE_COPY]] to [init] [[ARG0]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}10addrToAddr1xQr
func addrToAddr(x: AddrOnly) -> some P {
  // CHECK: bb0([[ARG0:%.*]] : $*AddrOnly, [[ARG1:%.*]] : $*AddrOnly):
  // CHECK: copy_addr [[ARG1]] to [init] [[ARG0]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}13genericAddrToE01xQr
func genericAddrToAddr<T: P>(x: T) -> some P {
  // CHECK: bb0([[ARG0:%.*]] : $*T, [[ARG1:%.*]] : $*T):
  // CHECK: copy_addr [[ARG1]] to [init] [[ARG0]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}12valueToValue1xQr
func valueToValue(x: C) -> some Q {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $C):
  // CHECK: [[VALUE_COPY:%.*]] = copy_value [[ARG]]
  // CHECK: return [[VALUE_COPY]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}13reabstraction1xQr
func reabstraction(x: @escaping () -> ()) -> some Any {
  // CHECK: bb0([[ARG0:%[0-9]+]] : 
  // CHECK: [[VALUE_COPY:%.*]] = copy_value [[ARG1]]
  // CHECK: [[REABSTRACT:%.*]] = function_ref @$sIeg_ytIegr_TR
  // CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[VALUE_COPY]])
  // CHECK: [[THUNK_CONV:%.*]] = convert_function [[THUNK]]
  // CHECK: store [[THUNK_CONV]] to [init] [[ARG0]]
  return x
}

protocol X {
  associatedtype A
  func foo() -> A
}

extension Int : P {}

extension ResilientInt : P {}

class K : P {}

func useClosure2(_ cl: () -> ()) {}

func useClosure(_ cl: @escaping () -> ()) {
  cl()
}

struct S : X {

  func foo() -> some P {
    return returnTrivial()
  }

  func returnTrivial() -> some P {
    return 1
  }

  func returnClass() -> some P {
    return K()
  }

  func returnResilient() -> some P {
    return ResilientInt(i: 1)
  }

  func testCapture() {
    var someP = returnTrivial()
    var someK = returnClass()
    var someR = returnResilient()
    useClosure {
      someP = self.returnTrivial()
      someK = self.returnClass()
      someR = self.returnResilient()
    }
    print(someP)
    print(someK)
    print(someR)
  }

  func testCapture2() {
    var someP = returnTrivial()
    var someK = returnClass()
    var someR = returnResilient()
    useClosure2 {
      someP = self.returnTrivial()
      someK = self.returnClass()
      someR = self.returnResilient()
    }
    print(someP)
    print(someK)
    print(someR)
  }

  func testCapture3() {
    let someP = returnTrivial()
    let someK = returnClass()
    let someR = returnResilient()
    useClosure {
      print(someP)
      print(someK)
      print(someR)
    }
  }

  func testCapture4() {
    let someP = returnTrivial()
    let someK = returnClass()
    let someR = returnResilient()
    useClosure {
      print(someP)
      print(someK)
      print(someR)
    }
  }
}

extension Optional : P { }

struct S2 : X {
  func foo() -> some P {
    let x : Optional = 1
    return x
  }
  func returnFunctionType() -> () -> A {
    return foo
  }
}

class Base {}
class Sub1 : Base {}
class Sub2 : Base {}

public class D {
   var cond = true
   // CHECK-LABEL: sil private [lazy_getter] [noinline] [ossa] @$s18opaque_result_type1DC1c33_C2C55A4BAF30C3244D4A165D48A91142LLQrvg
   // CHECK: bb3([[RET:%[0-9]+]] : @owned $Base):
   // CHECH:  return [[RET]]
   // CHECK: } // end sil function '$s18opaque_result_type1DC1c33_C2C55A4BAF30C3244D4A165D48A91142LLQrvg'
   private lazy var c: some Base = {
        let d = cond ? Sub1() : Sub2()
        return d
    }()
}

// CHECK-LABEL: sil [ossa] @$s18opaque_result_type10tupleAsAnyQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <@_opaqueReturnTypeOf("$s18opaque_result_type10tupleAsAnyQryF", 0) __> {
public func tupleAsAny() -> some Any {
// CHECK:      bb0(%0 : $*()):
// CHECK-NEXT:   %1 = tuple ()
// CHECK-NEXT:   return %1 : $()
  return ()
}
