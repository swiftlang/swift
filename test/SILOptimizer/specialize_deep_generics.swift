// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -emit-sil %s | %FileCheck %s

// Check that this Church Numerals inspired example does not hang
// a compiler in the generic specializer.
//
// rdar://problem/21260480

protocol Nat {
  init()
  // static stored properties are not supported in generic structs.
  var val : Int32 {get}
}

struct Zero : Nat { var val : Int32 = 0 }

struct PlusOne<X : Nat> : Nat {
  var val : Int32 = X().val + 1
}

// Compiler used to keep performing the generic specialization of
// computeNat for increasingly deeply nested bound generic types
// like PlusOne<PlusOne<....<PlusOne<Zero>>>
func computeNat<T: Nat>(_ v : Int32, _ t: T) -> Int32 {
  if v == 0 {
    return t.val
  }
  return computeNat(v - 1, PlusOne<T>())
}

// CHECK-LABEL: sil @_T024specialize_deep_generics14testComputeNats5Int32VyF
public func testComputeNat() -> Int32 {
 return computeNat(8, Zero())
}

