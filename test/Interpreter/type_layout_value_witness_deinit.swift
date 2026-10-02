// RUN: %target-run-simple-swift(-O -enable-experimental-feature RawLayout -enable-builtin-module) | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: swift_feature_RawLayout
// XFAIL: swift_test_mode_optimize_none_with_opaque_values
// UNSUPPORTED: back_deployment_runtime

// A type's own deinit, and only that deinit, must run when it is destroyed
// through a value witness table generated from type layouts (at -O).

import Builtin

// These nested types have the same type layout entries, since they share
// Outer's archetype. If the type layout cache didn't distinguish them, one
// would end up calling another's deinit.
struct Outer<T: ~Copyable>: ~Copyable {
  @_rawLayout(like: T, movesAsLike)
  struct RawLayoutWithDeinit: ~Copyable {
    init(_ value: consuming T) {
      unsafe UnsafeMutablePointer<T>(Builtin.addressOfRawLayout(self))
        .initialize(to: value)
    }
    deinit {
      print("RawLayoutWithDeinit.deinit")
      unsafe UnsafeMutablePointer<T>(Builtin.addressOfRawLayout(self))
        .deinitialize(count: 1)
    }
  }

  struct WithDeinit: ~Copyable {
    var x: T
    init(_ x: consuming T) { self.x = x }
    deinit { print("WithDeinit.deinit") }
  }

  struct WithoutDeinit: ~Copyable {
    var x: T
    init(_ x: consuming T) { self.x = x }
  }
}

// Destroy through the value witness table rather than a specialized path.
@_optimize(none) @inline(never)
func drop<T: ~Copyable>(_ x: consuming T) { _ = consume x }

drop(Outer<Int>.RawLayoutWithDeinit(0))
// CHECK: RawLayoutWithDeinit.deinit
drop(Outer<Int>.WithDeinit(0))
// CHECK-NEXT: WithDeinit.deinit
drop(Outer<Int>.WithoutDeinit(0))
print("done")
// CHECK-NEXT: done
