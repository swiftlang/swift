// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the resilient protocol library.
// RUN: %target-swift-frontend                              \
// RUN:     %t/Library.swift                                \
// RUN:     -emit-module                                    \
// RUN:     -enable-library-evolution                       \
// RUN:     -module-name Library                            \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -emit-module-path %t/Library.swiftmodule

// A client that may run before the feature (an old deployment target) binds the
// old (yield_once) witness slots; a client that cannot (a future deployment
// target) binds the new (yield_once_2) slots.
// RUN: %target-swift-emit-silgen                           \
// RUN:     %t/Client.swift                                 \
// RUN:     -target %target-swift-5.9-abi-triple            \
// RUN:     -module-name Client                             \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -I %t                                           \
// RUN: | %FileCheck %t/Client.swift --check-prefixes=CHECK,CHECK-OLD

// RUN: %target-swift-emit-silgen                           \
// RUN:     %t/Client.swift                                 \
// TODO: CoroutineAccessors: Change to %target-swift-x.y-abi-triple
// RUN:     -target %target-future-triple                   \
// RUN:     -module-name Client                             \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -I %t                                           \
// RUN: | %FileCheck %t/Client.swift --check-prefixes=CHECK,CHECK-NEW

// REQUIRES: swift_stable_abi
// REQUIRES: swift_feature_CoroutineAccessors

//--- Library.swift

// A resilient protocol whose property requirement is available before the
// feature.  Because the requirement predates the feature, its witness table
// carries both the old (read/modify) and the new
// (yielding_borrow/yielding_mutate) accessor slots.
public protocol ProtocolOld {
  @_borrowed var value: Int { get set }
  @_borrowed subscript(i: Int) -> Int { get set }
}

//--- Client.swift

import Library

// A conforming type provides witnesses for every required slot.  The
// conformance's witness table is additive: it lists both ABIs' slots
// regardless of the deployment target it is compiled for.
struct SOld: ProtocolOld {
  var _value: Int = 0
  var value: Int {
    yielding borrow {
      yield _value
    }
    yielding mutate {
      yield &_value
    }
  }
  subscript(i: Int) -> Int {
    yielding borrow {
      yield _value
    }
    yielding mutate {
      yield &_value
    }
  }
}

// Generic dispatch, read.  The witness slot the caller binds is chosen the same
// way as a direct access: by the caller's availability, falling back to its
// deployment target.
@_silgen_name("readGenericOld")
func readGenericOld<T: ProtocolOld>(_ x: T) -> Int {
// CHECK-LABEL: sil {{.*}}@readGenericOld : {{.*}} {
// CHECK-OLD:     witness_method {{.*}}#ProtocolOld.value!read
// CHECK-NEW:     witness_method {{.*}}#ProtocolOld.value!yielding_borrow
// CHECK-LABEL: } // end sil function 'readGenericOld'
  return x.value
}

// Generic dispatch, modify.
@_silgen_name("modifyGenericOld")
func modifyGenericOld<T: ProtocolOld>(_ x: inout T) {
// CHECK-LABEL: sil {{.*}}@modifyGenericOld : {{.*}} {
// CHECK-OLD:     witness_method {{.*}}#ProtocolOld.value!modify
// CHECK-NEW:     witness_method {{.*}}#ProtocolOld.value!yielding_mutate
// CHECK-LABEL: } // end sil function 'modifyGenericOld'
  x.value += 1
}

// Existential dispatch, read.
@_silgen_name("readExistentialOld")
func readExistentialOld(_ x: any ProtocolOld) -> Int {
// CHECK-LABEL: sil {{.*}}@readExistentialOld : {{.*}} {
// CHECK-OLD:     witness_method {{.*}}#ProtocolOld.value!read
// CHECK-NEW:     witness_method {{.*}}#ProtocolOld.value!yielding_borrow
// CHECK-LABEL: } // end sil function 'readExistentialOld'
  return x.value
}

// Existential dispatch, modify.
@_silgen_name("modifyExistentialOld")
func modifyExistentialOld(_ x: inout any ProtocolOld) {
// CHECK-LABEL: sil {{.*}}@modifyExistentialOld : {{.*}} {
// CHECK-OLD:     witness_method {{.*}}#ProtocolOld.value!modify
// CHECK-NEW:     witness_method {{.*}}#ProtocolOld.value!yielding_mutate
// CHECK-LABEL: } // end sil function 'modifyExistentialOld'
  x.value += 1
}

// A caller in a context that postdates the feature uses the new ABI even at an
// old deployment target.  Unlike the deployment-target fallback used for
// references with no source location, a member access through a protocol
// carries its reference location, so it is refined by the caller's own
// availability.
@available(SwiftStdlib 9999, *)
@_silgen_name("readGenericNew")
func readGenericNew<T: ProtocolOld>(_ x: T) -> Int {
// CHECK-LABEL: sil {{.*}}@readGenericNew : {{.*}} {
// CHECK:         witness_method {{.*}}#ProtocolOld.value!yielding_borrow
// CHECK-LABEL: } // end sil function 'readGenericNew'
  return x.value
}

@available(SwiftStdlib 9999, *)
@_silgen_name("modifyExistentialNew")
func modifyExistentialNew(_ x: inout any ProtocolOld) {
// CHECK-LABEL: sil {{.*}}@modifyExistentialNew : {{.*}} {
// CHECK:         witness_method {{.*}}#ProtocolOld.value!yielding_mutate
// CHECK-LABEL: } // end sil function 'modifyExistentialNew'
  x.value += 1
}

// Subscript requirements dispatch through the witness table the same way.
@_silgen_name("readSubscriptOld")
func readSubscriptOld<T: ProtocolOld>(_ x: T) -> Int {
// CHECK-LABEL: sil {{.*}}@readSubscriptOld : {{.*}} {
// CHECK-OLD:     witness_method {{.*}}#ProtocolOld.subscript!read
// CHECK-NEW:     witness_method {{.*}}#ProtocolOld.subscript!yielding_borrow
// CHECK-LABEL: } // end sil function 'readSubscriptOld'
  return x[0]
}

@_silgen_name("modifySubscriptOld")
func modifySubscriptOld(_ x: inout any ProtocolOld) {
// CHECK-LABEL: sil {{.*}}@modifySubscriptOld : {{.*}} {
// CHECK-OLD:     witness_method {{.*}}#ProtocolOld.subscript!modify
// CHECK-NEW:     witness_method {{.*}}#ProtocolOld.subscript!yielding_mutate
// CHECK-LABEL: } // end sil function 'modifySubscriptOld'
  x[0] += 1
}

@available(SwiftStdlib 9999, *)
@_silgen_name("readSubscriptNew")
func readSubscriptNew<T: ProtocolOld>(_ x: T) -> Int {
// CHECK-LABEL: sil {{.*}}@readSubscriptNew : {{.*}} {
// CHECK:         witness_method {{.*}}#ProtocolOld.subscript!yielding_borrow
// CHECK-LABEL: } // end sil function 'readSubscriptNew'
  return x[0]
}

// The conformance carries both ABIs' witness slots (emitted after the
// functions, so these checks come last).
// CHECK-LABEL: sil_witness_table {{.*}}SOld: ProtocolOld module Client {
// CHECK-DAG:     method #ProtocolOld.value!read:
// CHECK-DAG:     method #ProtocolOld.value!yielding_borrow:
// CHECK-DAG:     method #ProtocolOld.value!modify:
// CHECK-DAG:     method #ProtocolOld.value!yielding_mutate:
// CHECK-DAG:     method #ProtocolOld.subscript!read:
// CHECK-DAG:     method #ProtocolOld.subscript!yielding_borrow:
// CHECK-DAG:     method #ProtocolOld.subscript!modify:
// CHECK-DAG:     method #ProtocolOld.subscript!yielding_mutate:
// CHECK:       }
