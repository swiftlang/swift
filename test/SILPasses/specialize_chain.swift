// RUN: %swift -O3 -emit-sil %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

struct YYY<T> {
  init(t : T) {m_t = t}
  func AAA9(t : T) -> Int { return AAA8(t)}
  func AAA8(t : T) -> Int { return AAA7(t)}
  func AAA7(t : T) -> Int { return AAA6(t)}
  func AAA6(t : T) -> Int { return AAA5(t)}
  func AAA5(t : T) -> Int { return AAA4(t)}
  func AAA4(t : T) -> Int { return AAA3(t)}
  func AAA3(t : T) -> Int { return AAA2(t)}
  func AAA2(t : T) -> Int { return AAA1(t)}
  func AAA1(t : T) -> Int { return AAA0(t)}
  func AAA0(t : T) -> Int { return foo(t)}
  func foo(t : T) -> Int {m_t = t; return 4}
  var m_t : T
}

var II = YYY<Int>(5)
print(II.AAA9(4))

//CHECK: top_level_code
//CHECK: function_ref @_TV16specialize_chain3YYYCU__fMGS0_Q__FT1tQ__GS0_Q__
//CHECK: function_ref @_TV16specialize_chain3YYYCU__fMGS0_Q__FT1tQ__GS0_Q___spec0
//CHECK: function_ref @_TV16specialize_chain3YYY4AAA9U__fRGS0_Q__FT1tQ__Si
//CHECK: return
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA9U__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA8U__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA7U__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA6U__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA5U__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA4U__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA3U__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA2U__fRGS0_Q__FT1tQ__Si_spec0
//CHECK-DAG: sil internal @_TV16specialize_chain3YYY4AAA1U__fRGS0_Q__FT1tQ__Si_spec0
