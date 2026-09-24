// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -emit-module -module-name Lib -o %t %S/Inputs/opaque_readwrite_borrow_mutate.swift
// RUN: %target-swift-frontend -I %t -module-name main -emit-silgen %s | %FileCheck %s

// A ReadWrite access to a resilient property with borrow/mutate accessors uses
// the mutate accessor directly.

import Lib

// CHECK-LABEL: sil [ossa] @$s4main13mutateInPlaceyy3Lib7WrapperVzF :
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK-NOT: copy_addr
// CHECK:   [[MUTATE_FN:%.*]] = function_ref @$s3Lib7WrapperV5valueAA10NonTrivialVvz : $@convention(method) (@inout Wrapper) -> @inout NonTrivial
// CHECK:   [[INOUT_REF:%.*]] = apply [[MUTATE_FN]]([[ACCESS]]) : $@convention(method) (@inout Wrapper) -> @inout NonTrivial
// CHECK:   [[MUTATE_IN_PLACE_FN:%.*]] = function_ref @$s3Lib10NonTrivialV13mutateInPlaceyyF : $@convention(method) (@inout NonTrivial) -> ()
// CHECK:   apply [[MUTATE_IN_PLACE_FN]]([[INOUT_REF]]) : $@convention(method) (@inout NonTrivial) -> ()
// CHECK:   end_access [[ACCESS]]
// CHECK: }
public func mutateInPlace(_ w: inout Wrapper) {
  w.value.mutateInPlace()
}

// CHECK-LABEL: sil [ossa] @$s4main11assignValueyy3Lib7WrapperVzF :
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[MUTATE_FN:%.*]] = function_ref @$s3Lib7WrapperV5valueAA10NonTrivialVvz : $@convention(method) (@inout Wrapper) -> @inout NonTrivial
// CHECK:   [[INOUT_REF:%.*]] = apply [[MUTATE_FN]]([[ACCESS]]) : $@convention(method) (@inout Wrapper) -> @inout NonTrivial
// CHECK:   end_access [[ACCESS]]
// CHECK: }
public func assignValue(_ w: inout Wrapper) {
  w.value = NonTrivial()
}

// CHECK-LABEL: sil [ossa] @$s4main9readValueyy3Lib7WrapperVF :
// CHECK: bb0([[REG0:%.*]] : $*Wrapper):
// CHECK:   [[BORROW_FN:%.*]] = function_ref @$s3Lib7WrapperV5valueAA10NonTrivialVvb : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed_address NonTrivial
// CHECK:   apply [[BORROW_FN]]([[REG0]]) : $@convention(method) (@in_guaranteed Wrapper) -> @guaranteed_address NonTrivial
// CHECK: }
public func readValue(_ w: Wrapper) {
  _ = w.value
}

// CHECK-LABEL: sil [ossa] @$s4main15ncMutateInPlaceyy3Lib9NCWrapperVzF :
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper):
// CHECK-NOT: copy_addr
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown]
// CHECK:   [[MUTATE_FN:%.*]] = function_ref @$s3Lib9NCWrapperV5valueAA2NCVvz : $@convention(method) (@inout NCWrapper) -> @inout NC
// CHECK:   [[INOUT_REF:%.*]] = apply [[MUTATE_FN]]([[ACCESS]]) : $@convention(method) (@inout NCWrapper) -> @inout NC
// CHECK:   [[MUTATE_IN_PLACE_FN:%.*]] = function_ref @$s3Lib2NCV13mutateInPlaceyyF : $@convention(method) (@inout NC) -> ()
// CHECK:   end_access [[ACCESS]]
// CHECK: }
public func ncMutateInPlace(_ w: inout NCWrapper) {
  w.value.mutateInPlace()
}

// CHECK-LABEL: sil [ossa] @$s4main11ncReadValueyy3Lib9NCWrapperVF :
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper):
// CHECK:   [[BORROW_FN:%.*]] = function_ref @$s3Lib9NCWrapperV5valueAA2NCVvb : $@convention(method) (@in_guaranteed NCWrapper) -> @guaranteed_address NC
// CHECK:   apply [[BORROW_FN]]
// CHECK: }
public func ncReadValue(_ w: borrowing NCWrapper) {
  _ = w.value
}
