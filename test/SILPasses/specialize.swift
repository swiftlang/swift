// RUN: %swift -O3 -sil-inline-threshold 0 -emit-sil %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.


struct XXX<T> {
  init(t : T) {m_t = t}
  mutating
  func foo(`t : T) -> Int {m_t = t; return 4}
  var m_t : T
}

func exp1() {
  var II = XXX<Int>(t: 5)
  print(II.foo(t: 4))
}
//CHECK: exp1
//CHECK-NOT: apply
//CHECK: [[CTOR:%[0-9]+]] = function_ref @_TTSSi___TFV10specialize3XXXCU__fMGS0_Q__FT1tQ__GS0_Q__
//CHECK: apply [[CTOR]]
//CHECK: [[PRINT:%[0-9]+]] = function_ref @_TFSs5printFSiT_ 
//CHECK: [[FOO:%[0-9]+]] = function_ref @_TTSSi___TFV10specialize3XXX3fooU__fRGS0_Q__FT1tQ__Si
//CHECK: apply [[FOO]]
//CHECK: apply [[PRINT]]
//CHECK: return

func exp2() {
 var II8 = XXX<UInt8>(t: UInt8(5))
 print(II8.foo(t: UInt8(4)))
}


// Make sure we can specialize partial applies.
// CHECK-LABEL: sil @_TF10specialize24specializePartialAppliesFT_VSs5UInt8 : $@thin () -> UInt8 {
// CHECK: function_ref @_TTSVSs5UInt8___TF10specialize17getGenericClosureU__FT1tQ__FT_Q_
// CHECK: function_ref @_TTSVSs5UInt8___TF10specialize10useClosureU__FT3funFT_Q__Q_
func useClosure<T>(`fun : () -> T) -> T {
  return fun()
}

func getGenericClosure<T>(`t : T) -> (() -> T){
  func tmp() -> T {
    return t
  }
  return tmp
}

func specializePartialApplies() -> UInt8 {
  var i = UInt8(5)
  return useClosure(getGenericClosure(t: i))
}

// Make sure that we specialize XXX twice (for Int and UInt8):

// CHECK-DAG: sil shared @_TTSSi___TFV10specialize3XXXCU__fMGS0_Q__FT1tQ__GS0_Q__
// CHECK-DAG: sil shared @_TTSVSs5UInt8___TFV10specialize3XXXCU__fMGS0_Q__FT1tQ__GS0_Q__
// CHECK-DAG: sil shared @_TTSSi___TFV10specialize3XXX3fooU__fRGS0_Q__FT1tQ__Si
// CHECK-DAG: sil shared @_TTSVSs5UInt8___TFV10specialize3XXX3fooU__fRGS0_Q__FT1tQ__Si
// CHECK-DAG: sil shared @_TTSVSs5UInt8___TF10specialize10useClosureU__FT3funFT_Q__Q_
// CHECK-DAG: sil shared @_TTSVSs5UInt8___TF10specialize17getGenericClosureU__FT1tQ__FT_Q_
