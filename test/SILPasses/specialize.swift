// RUN: %swift -O3 -emit-sil %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.


struct XXX<T> {
  init(t : T) {m_t = t}
  func foo(t : T) -> Int {m_t = t; return 4}
  var m_t : T
}

var II = XXX<Int>(5)
print(II.foo(4))

//CHECK: top_level_code
//CHECK-NOT: apply
//CHECK: [[CTOR:%[0-9]+]] = function_ref @_TV10specialize3XXXCU__fMGS0_Q__FT1tQ__GS0_Q___spec0
//CHECK: apply [[CTOR]]
//CHECK: [[PRINT:%[0-9]+]] = function_ref @_TSs5printFT3valSi_T_ : $@thin (Int64) -> () // user: %18
//CHECK: [[FOO:%[0-9]+]] = function_ref @_TV10specialize3XXX3fooU__fRGS0_Q__FT1tQ__Si_spec0
//CHECK: apply [[FOO]]
//CHECK: apply [[PRINT]]
//CHECK-NOT: apply
//CHECK: return

//CHECK: sil internal @_TV10specialize3XXXCU__fMGS0_Q__FT1tQ__GS0_Q___spec0
//CHECK: sil internal @_TV10specialize3XXX3fooU__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-NOT: sil internal @_TV
