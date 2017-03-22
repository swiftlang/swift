// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

// Make sure that we are able to inline try-apply instructions.

@_transparent
public func foo() throws -> Int32 {
  return 999
}

// CHECK-LABEL: _T012throw_inline3foos5Int32VyKF
// CHECK: debug_value undef : $Error, var, name "$error", argno 1
// CHECK: %1 = integer_literal $Builtin.Int32, 999
// CHECK: %2 = struct $Int32 (%1 : $Builtin.Int32)
// CHECK: return %2 : $Int32
func bar() throws  -> Int32 {
  return try foo()
}

