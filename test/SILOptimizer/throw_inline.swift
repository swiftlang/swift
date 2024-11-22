// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

// Make sure that we are able to inline try-apply instructions.

@_transparent
public func foo() throws -> Int32 {
  return 999
}

// CHECK-LABEL: $s12throw_inline3foos5Int32VyKF
// CHECK: debug_value undef : $any Error, var, name "$error", argno 1
// CHECK: %1 = integer_literal $Builtin.Int32, 999
// CHECK: %[[POS:.*]] = struct $Int32 (%1 : $Builtin.Int32)
// CHECK: return %[[POS]] : $Int32
func bar() throws  -> Int32 {
  return try foo()
}

