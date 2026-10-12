// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -parse-as-library -module-name main -target %target-future-triple -enable-experimental-feature NoncopyableCasting %s | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting

// Regression test: when SimplifyAllocStack replaced an existential alloc_stack
// with its concrete (here: generic) type, it retyped the debug_value of the
// alloc_stack but left a `debug_value undef` of the same variable with the
// existential type. This failed SIL verification with
//
//   SIL verification failed: Two variables with different type but same scope
//
// SILGen emits the `debug_value undef` when the `consuming` parameter `x` is
// consumed. After the existential specializer created the specialization of
// `check`, the parameter is stored into an `alloc_stack $any P & ~Copyable`,
// which is then optimized to an `alloc_stack $τ_0_0`.

public protocol P: ~Copyable {}

public struct S: P, ~Copyable {
  public init() {}
}

@inline(never)
func check(_ x: consuming any P & ~Copyable) -> Bool {
  if case is any P & ~Copyable = x { return true }
  return false
}

// Two call sites through a `@inline(never)` boundary make the existential
// specializer run on `check` (`Tf4e_n` is its mangling).
// CHECK-LABEL: sil [noinline] @$s4main6driverySbSiF :
// CHECK:         function_ref @{{.*}}Tf4e_n
// CHECK:       } // end sil function
@inline(never)
public func driver(_ n: Int) -> Bool {
  if n > 0 { return check(S()) }
  return check(S())
}
