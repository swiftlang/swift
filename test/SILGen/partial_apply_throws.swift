// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

class C {
  func foo() throws { }
}

_ = C.foo

// CHECK-LABEL: sil private [ossa] @$s20partial_apply_throwsyyKcAA1CCcfu_ : $@convention(thin) (@guaranteed C) -> @owned @callee_guaranteed () -> @error any Error {

// CHECK-LABEL: sil private [ossa] @$s20partial_apply_throwsyyKcAA1CCcfu_yyKcfu0_ : $@convention(thin) (@guaranteed C) -> @error any Error {
// CHECK: [[FN:%.*]] = class_method %0 : $C, #C.foo : (C) -> () throws -> (), $@convention(method) (@guaranteed C) -> @error any Error
// CHECK: try_apply [[FN]](%0)
// CHECK: return
// CHECK: throw
