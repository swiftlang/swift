// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck %s

typealias Int = Builtin.Int64

// CHECK: sil hidden @_TF13capture_inout8localFooFT1xRBi64__T_
// CHECK: bb0([[X_INOUT:%.*]] : $*Builtin.Int64):
// CHECK-NOT: alloc_box
// CHECK:   [[FUNC:%.*]] = function_ref [[CLOSURE:@.*]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
// CHECK:   apply [[FUNC]]([[X_INOUT]])
// CHECK: }
// CHECK: sil shared [[CLOSURE]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
func localFoo(x: inout Int) {
  func bar() -> Int {
    return x
  }
  bar()
}

// CHECK: sil hidden @_TF13capture_inout7anonFooFT1xRBi64__T_
// CHECK: bb0([[X_INOUT:%.*]] : $*Builtin.Int64):
// CHECK-NOT: alloc_box
// CHECK:   [[FUNC:%.*]] = function_ref [[CLOSURE:@.*]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
// CHECK:   apply [[FUNC]]([[X_INOUT]])
// CHECK: }
// CHECK: sil shared [[CLOSURE]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
func anonFoo(x: inout Int) {
  { return x }()
}
