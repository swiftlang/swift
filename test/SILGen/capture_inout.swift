// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s

typealias Int = Builtin.Int64

// CHECK: sil hidden @_TF13capture_inout3foo
// CHECK: bb0([[X_INOUT:%.*]] : $*Builtin.Int64):
// CHECK:   [[X_LOCAL:%.*]] = alloc_box $Builtin.Int64
// CHECK:   [[FUNC:%.*]] = function_ref [[CLOSURE:@.*]] : $@thin (@owned Builtin.NativeObject, @inout Builtin.Int64) -> Builtin.Int64
// CHECK:   partial_apply [[FUNC]]([[X_LOCAL]]#0, [[X_LOCAL]]#1)
// CHECK: }
// CHECK: sil shared [[CLOSURE]] : $@thin (@owned Builtin.NativeObject, @inout Builtin.Int64) -> Builtin.Int64
func foo(inout x: Int) -> () -> Int {
  func bar() -> Int {
    return x
  }
  return bar
}
