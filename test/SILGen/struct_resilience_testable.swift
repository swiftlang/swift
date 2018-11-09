
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -enable-testing -emit-module-path=%t/resilient_struct.swiftmodule -enable-sil-ownership %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-emit-silgen -I %t -enable-sil-ownership %s | %FileCheck %s

@testable import resilient_struct

// CHECK-LABEL: sil @$s26struct_resilience_testable37takesResilientStructWithInternalFieldySi010resilient_A00eghI0VF : $@convention(thin) (@in_guaranteed ResilientWithInternalField) -> Int
// CHECK: [[COPY:%.*]] = alloc_stack $ResilientWithInternalField
// CHECK: copy_addr %0 to [initialization] [[COPY]] : $*ResilientWithInternalField
// CHECK: [[FN:%.*]] = function_ref @$s16resilient_struct26ResilientWithInternalFieldV1xSivg : $@convention(method) (@in_guaranteed ResilientWithInternalField) -> Int
// CHECK: [[RESULT:%.*]] = apply [[FN]]([[COPY]])
// CHECK: destroy_addr [[COPY]]
// CHECK: dealloc_stack [[COPY]]
// CHECK: return [[RESULT]]

public func takesResilientStructWithInternalField(_ s: ResilientWithInternalField) -> Int {
  return s.x
}
