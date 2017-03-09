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

// Check that compiler does not hang producing very wide tuples during
// specialization.
@inline(never)
func computeTuple<T>(t: T) {
  computeTuple(t: (t, t))
}

// CHECK-LABEL: sil @_T024specialize_deep_generics16testComputeTupleyyF
public func testComputeTuple() {
  computeTuple(t: 0)
}

// Check that compiler does not hang producing very deep metatypes.
@inline(never)
public func computeMetatype<T>(t: T) {
  computeMetatype(t: T.self)
}

// CHECK-LABEL: sil @_T024specialize_deep_generics19testComputeMetatypeyyF
public func testComputeMetatype() {
  computeMetatype(t: 0)
}

// Check that compiler does not hang producing very deep function types.
@inline(never)
public func computeFunctionType<T>(t: [T]) {
  computeFunctionType(t: [{ t[0] }])
}

public func testComputeFunctionType() {
  computeFunctionType(t: [0])
}
