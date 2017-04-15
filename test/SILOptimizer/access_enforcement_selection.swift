// RUN: %target-swift-frontend -enforce-exclusivity=checked -Onone -emit-sil -parse-as-library %s -Xllvm -debug-only=access-enforcement-selection 2>&1 | %FileCheck %s
// REQUIRES: asserts

// This is a source-level test because it helps bring up the entire -Onone pipeline with the access markers.

public func takesInout(_ i: inout Int) {
  i = 42
}
// CHECK-LABEL: Access Enforcement Selection in _T028access_enforcement_selection10takesInoutySizF
// CHECK: Static Access: %{{.*}} = begin_access [modify] [static] %0 : $*Int

// Helper taking a basic, no-escape closure.
func takeClosure(_: ()->Int) {}


// Generate an alloc_stack that escapes into a closure.
public func captureStack() -> Int {
  // Use a `var` so `x` isn't treated as a literal.
  var x = 3
  takeClosure { return x }
  return x
}
// CHECK-LABEL: Access Enforcement Selection in _T028access_enforcement_selection12captureStackSiyF
// Static access for `return x`.
// CHECK: Static Access: %{{.*}} = begin_access [read] [static] %0 : $*Int

// The access inside the closure is dynamic, until we have the logic necessary to
// prove that no other closures are passed to `takeClosure` that may write to
// `x`.
//
// CHECK-LABEL: Access Enforcement Selection in _T028access_enforcement_selection12captureStackSiyFSiycfU_
// CHECK: Dynamic Access: %{{.*}} = begin_access [read] [dynamic] %0 : $*Int

// Generate a closure in which the @inout_aliasing argument
// escapes to an @inout function `bar`.
public func recaptureStack() -> Int {
  var x = 3
  takeClosure { takesInout(&x); return x }
  return x
}
// CHECK-LABEL: Access Enforcement Selection in _T028access_enforcement_selection14recaptureStackSiyF
//
// Static access for `return x`.
// CHECK: Static Access:   %10 = begin_access [read] [static] %0 : $*Int

// CHECK-LABEL: Access Enforcement Selection in _T028access_enforcement_selection14recaptureStackSiyFSiycfU_
//
// The first [modify] access inside the closure must be dynamic. It enforces the
// @inout argument.
// CHECK: Dynamic Access: %{{.*}} = begin_access [modify] [dynamic] %0 : $*Int
//
// The second [read] access is only dynamic because the analysis isn't strong
// enough to prove otherwise. Same as `captureStack` above.
//
// CHECK: Dynamic Access: %{{.*}} = begin_access [read] [dynamic] %0 : $*Int

public func undefStack(i: Int) -> Int {
  _preconditionFailure("unreachable")
  var x = 3
  if i != 0 {
    x = 42
  }
  return x
}
// CHECK-LABEL: Access Enforcement Selection in _T028access_enforcement_selection10undefStackS2i1i_tF
// CHECK: Static Access:   %{{.*}} = begin_access [modify] [static] undef : $*Int
// CHECK: Static Access:   %{{.*}} = begin_access [read] [static] undef : $*Int
