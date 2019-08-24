// RUN: %target-typecheck-verify-swift -typecheck -debug-constraints %s > %t.dump 2>&1 
// RUN: %FileCheck %s < %t.dump

// Make sure that the type checker doesn't initially try to solve the appendLiteral and 
// appendInterpolation calls. Specifically, we check that the string literal
// has been assigned a type variable, but the calls inside the body have not.

// CHECK: ---Initial constraints for the given expression---
// CHECK: (interpolated_string_literal_expr type='$T
// CHECK-NOT: (call_expr implicit type='$T
// CHECK: ---Solution---

// We also check that the type checker did not need to evaluate any 
// DefaultStringInterpolation overloads in the initial expression.

// CHECK-NOT: ---Solution---
// CHECK: Overload choices:
// CHECK-NOT: Swift.(file).DefaultStringInterpolation.append
// CHECK: Constraint restrictions:

_ = "\(1), \(2), \(3), \(4)"
