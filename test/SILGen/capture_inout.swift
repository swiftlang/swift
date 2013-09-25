// RUN: %swift -emit-silgen %s | FileCheck %s

// CHECK: sil @_T13capture_inout3fooFT1xRSi_FT_Si
// CHECK: bb0([[X_INOUT:%.*]] : $*Int64):
// CHECK:   [[X_LOCAL:%.*]] = alloc_box $Int64
// CHECK:   [[FUNC:%.*]] = function_ref [[CLOSURE:@.*]] : $[thin] ((), (Builtin.ObjectPointer, [inout] Int64)) -> Int64
// CHECK:   partial_apply [[FUNC]]([[X_LOCAL]]#0, [[X_LOCAL]]#1)
// CHECK: }
// CHECK: sil internal [[CLOSURE]] : $[thin] ((), (Builtin.ObjectPointer, [inout] Int64)) -> Int64
func foo(x : [inout] Int) -> () -> Int {
  func bar() -> Int {
    x += 1
    return x
  }
  return bar
}
