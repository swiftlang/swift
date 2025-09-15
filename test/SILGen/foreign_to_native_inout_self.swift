// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -import-objc-header %S/Inputs/foreign_to_native_inout_self_helper.h | %FileCheck %s

protocol FakeIterator {
  mutating func next()
}

extension MyIterator : FakeIterator {}

// CHECK-LABEL: sil shared [serialized] [thunk] [ossa] @$sSo10MyIteratora4nextyyFTO : $@convention(method) (@inout MyIterator) -> () {
// CHECK: bb0(%0 : $*MyIterator):
// CHECK: [[FN:%.*]] = function_ref @MyIteratorNext : $@convention(c) (@inout MyIterator) -> ()
// CHECK:   apply [[FN]](%0) : $@convention(c) (@inout MyIterator) -> ()
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()
// CHECK: }
