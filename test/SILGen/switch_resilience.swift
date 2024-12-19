// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -I %t %s | %FileCheck %s

import resilient_struct

// CHECK-LABEL: sil hidden [ossa] @$s17switch_resilience29resilientTupleEltCaseEnumTestyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[STACK_SLOT:%.*]] = alloc_stack $Enum
//
// CHECK: bb1:
// CHECK:   [[VALUE:%.*]] = unchecked_take_enum_data_addr [[STACK_SLOT]] : $*Enum
// CHECK:   [[STACK_SLOT_COPY:%.*]] = alloc_stack [lexical] [var_decl] $(url: ResilientRef, void: ()), let, name "value"
// CHECK:   copy_addr [[VALUE]] to [init] [[STACK_SLOT_COPY]]
// CHECK:   cond_br {{%.*}}, bb2, bb3
//
// CHECK: bb2:
// CHECK: destroy_addr [[STACK_SLOT_COPY]]
// CHECK-NEXT: dealloc_stack [[STACK_SLOT_COPY]]
// CHECK-NEXT: destroy_addr [[VALUE]]
// CHECK-NEXT: dealloc_stack [[STACK_SLOT]]
// CHECK-NEXT: br bb4
//
// CHECK: bb3:
// CHECK-NEXT: destroy_addr [[STACK_SLOT_COPY]]
// CHECK-NEXT: dealloc_stack [[STACK_SLOT_COPY]]
// CHECK-NEXT: [[REPROJECT:%.*]] = tuple_element_addr [[VALUE]]
// CHECK: destroy_addr [[REPROJECT]]
// CHECK-NEXT: dealloc_stack [[STACK_SLOT]]
// CHECK: br bb4
//
// CHECK: } // end sil function '$s17switch_resilience29resilientTupleEltCaseEnumTestyyF'
func resilientTupleEltCaseEnumTest() {
  enum Enum {
  case first(url: ResilientRef, void: Void)
  }

  func getEnum() -> Enum {
    let url = ResilientRef(r: Referent())
    return .first(url: url, void: ())
  }
  func getBool() -> Bool { return false }
  func urlUser(_ u: ResilientRef) {}
  func kraken() {}

  switch getEnum() {
  case let .first(value) where getBool():
    urlUser(value.0)
  case .first:
    kraken()
  }
}
