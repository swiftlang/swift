// RUN: %target-swift-frontend -emit-sil %s | FileCheck %s

// Make sure that we are able to inline try-apply instructions.

@transparent
func foo() throws -> Int {
  return 999
}

// CHECK-LABLE: _TF12throw_inline3fooFzT_Si
// CHECK: %0 = integer_literal $Builtin.Int64, 999
// CHECK: %1 = struct $Int (%0 : $Builtin.Int64)
// CHECK: return %1 : $Int
func bar() throws  -> Int {
  return foo()
}

