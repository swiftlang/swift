// RUN: %target-swift-frontend -O -emit-sil -parse-as-library %s | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: swift_in_compiler

// String literals are not completely constant folded in SIL for ptrsize=32 which fails `deadClassInstance()`.
// This is no problem as LLVM can complete the constant folding.
// UNSUPPORTED: PTRSIZE=32

protocol E {
  func f() -> Bool
}

protocol P {
  associatedtype A = Int
}

public struct X : P, E {
  func f() -> Bool { return true }
}

func g<T : P>(_ x : T) -> Bool {
  if let y = x as? E { return y.f() }
  return false
}

// Check that this function can be completely constant folded and no alloc_stack remains.

// CHECK-LABEL: sil @$s10dead_alloc0A10AllocStackySbAA1XVF :
// CHECK:      bb0({{.*}}):
// CHECK-NEXT:   debug_value
// CHECK-NEXT:   integer_literal
// CHECK-NEXT:   struct
// CHECK-NEXT:   return
// CHECK-NEXT: } // end sil function '$s10dead_alloc0A10AllocStackySbAA1XVF'
public func deadAllocStack(_ x: X) -> Bool {
  return g(x)
}

public class C<T> {
    let x: String = "123"
}

// CHECK-LABEL: sil @$s10dead_alloc0A13ClassInstanceyyF :
// CHECK:       bb0:
// CHECK-NEXT:    tuple
// CHECK-NEXT:    return
// CHECK-NEXT:  } // end sil function '$s10dead_alloc0A13ClassInstanceyyF'
public func deadClassInstance() {
    let _ = C<Int>()
}

// CHECK-LABEL: sil @$s10dead_alloc0A13ManagedBufferyyF :
// CHECK:       bb0:
// CHECK-NEXT:    debug_value undef
// CHECK-NEXT:    tuple
// CHECK-NEXT:    return
// CHECK-NEXT:  } // end sil function '$s10dead_alloc0A13ManagedBufferyyF'
public func deadManagedBuffer() -> () {
  _ = ManagedBuffer<Void, Void>.create(minimumCapacity: 1, makingHeaderWith: { _ in () })
}
