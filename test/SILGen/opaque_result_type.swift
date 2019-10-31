// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -disable-availability-checking -emit-silgen %s | %FileCheck %s

import resilient_struct

protocol P {}
protocol Q: AnyObject {}

extension String: P {}
struct AddrOnly: P { var field: P }

class C: Q {}

// CHECK-LABEL: sil hidden {{.*}}11valueToAddr1xQr
func valueToAddr(x: String) -> some P {
  // CHECK: [[UNDERLYING:%.*]] = unchecked_addr_cast %0
  // CHECK: [[VALUE_COPY:%.*]] = copy_value %1
  // CHECK: store [[VALUE_COPY]] to [init] [[UNDERLYING]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}10addrToAddr1xQr
func addrToAddr(x: AddrOnly) -> some P {
  // CHECK: [[UNDERLYING:%.*]] = unchecked_addr_cast %0
  // CHECK: copy_addr %1 to [initialization] [[UNDERLYING]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}13genericAddrToE01xQr
func genericAddrToAddr<T: P>(x: T) -> some P {
  // CHECK: [[UNDERLYING:%.*]] = unchecked_addr_cast %0
  // CHECK: copy_addr %1 to [initialization] [[UNDERLYING]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}12valueToValue1xQr
func valueToValue(x: C) -> some Q {
  // CHECK: [[VALUE_COPY:%.*]] = copy_value %0
  // CHECK: [[CAST_TO_OPAQUE:%.*]] = unchecked_ref_cast [[VALUE_COPY]]
  // CHECK: return [[CAST_TO_OPAQUE]]
  return x
}

// CHECK-LABEL: sil hidden {{.*}}13reabstraction1xQr
func reabstraction(x: @escaping () -> ()) -> some Any {
  // CHECK: [[UNDERLYING:%.*]] = unchecked_addr_cast %0 : ${{.*}} to $*@callee_guaranteed () -> @out ()
  // CHECK: [[VALUE_COPY:%.*]] = copy_value %1
  // CHECK: [[VALUE_REABSTRACT:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[VALUE_COPY]])
  // CHECK: store [[VALUE_REABSTRACT]] to [init] [[UNDERLYING]]
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
