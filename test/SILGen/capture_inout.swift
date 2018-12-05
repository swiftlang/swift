// RUN: %target-swift-emit-silgen -enable-sil-ownership -parse-stdlib %s | %FileCheck %s

typealias Int = Builtin.Int64

// CHECK: sil hidden @$s13capture_inout8localFoo1xyBi64_z_tF
// CHECK: bb0([[X_INOUT:%.*]] : $*Builtin.Int64):
// CHECK-NOT: alloc_box
// CHECK:   [[FUNC:%.*]] = function_ref [[CLOSURE:@.*]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
// CHECK:   apply [[FUNC]]([[X_INOUT]])
// CHECK: }
// CHECK: sil private [[CLOSURE]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
func localFoo(x: inout Int) {
  func bar() -> Int {
    return x
  }
  bar()
}

// CHECK: sil hidden @$s13capture_inout7anonFoo1xyBi64_z_tF
// CHECK: bb0([[X_INOUT:%.*]] : $*Builtin.Int64):
// CHECK-NOT: alloc_box
// CHECK:   [[FUNC:%.*]] = function_ref [[CLOSURE:@.*]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
// CHECK:   apply [[FUNC]]([[X_INOUT]])
// CHECK: }
// CHECK: sil private [[CLOSURE]] : $@convention(thin) (@inout_aliasable Builtin.Int64) -> Builtin.Int64
func anonFoo(x: inout Int) {
  { return x }()
}
