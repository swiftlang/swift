// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-stdlib -emit-silgen %s | %FileCheck %s

typealias Int = Builtin.Int64

// CHECK: sil hidden @_T013capture_inout8localFooyBi64_z1x_tF
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

// CHECK: sil hidden @_T013capture_inout7anonFooyBi64_z1x_tF
// CHECK: bb0([[X_INOUT:%.*]] : $*Builtin.Int64):
// CHECK-NOT: alloc_box
// CHECK:   [[FUNC:%.*]] = function_ref [[CLOSURE:@.*]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
// CHECK:   apply [[FUNC]]([[X_INOUT]])
// CHECK: }
// CHECK: sil shared [[CLOSURE]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
func anonFoo(x: inout Int) {
  { return x }()
}
