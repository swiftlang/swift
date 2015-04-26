// RUN: %target-swift-frontend -emit-sil %s | FileCheck %s

// Make sure that we are able to inline try-apply instructions.

@transparent
public func foo() throws -> Int32 {
  return 999
}

// CHECK-LABEL: _TF12throw_inline3foo
// CHECK: %0 = integer_literal $Builtin.Int32, 999
// CHECK: %1 = struct $Int32 (%0 : $Builtin.Int32)
// CHECK: return %1 : $Int32
func bar() throws  -> Int32 {
  return foo()
}

