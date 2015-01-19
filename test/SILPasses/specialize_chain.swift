// RUN: %target-swift-frontend -disable-func-sig-opts -O -sil-inline-threshold 0 -emit-sil -primary-file %s | FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

struct YYY<T> {
  init(t : T) {m_t = t}
  mutating
  func AAA9(#t : T) -> Int { return AAA8(t: t)}
  mutating
  func AAA8(#t : T) -> Int { return AAA7(t: t)}
  mutating
  func AAA7(#t : T) -> Int { return AAA6(t: t)}
  mutating
  func AAA6(#t : T) -> Int { return AAA5(t: t)}
  mutating
  func AAA5(#t : T) -> Int { return AAA4(t: t)}
  mutating
  func AAA4(#t : T) -> Int { return AAA3(t: t)}
  mutating
  func AAA3(#t : T) -> Int { return AAA2(t: t)}
  mutating
  func AAA2(#t : T) -> Int { return AAA1(t: t)}
  mutating
  func AAA1(#t : T) -> Int { return AAA0(t: t)}
  mutating
  func AAA0(#t : T) -> Int { return foo(t: t)}
  mutating
  func foo(#t : T) -> Int {m_t = t; return 4}
  var m_t : T
}

func exp1() {
  var II = YYY<Int>(t: 5)
  print(II.AAA9(t: 4))
}
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA9U__fRGS0_Q__FT1tQ__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA8U__fRGS0_Q__FT1tQ__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA7U__fRGS0_Q__FT1tQ__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA6U__fRGS0_Q__FT1tQ__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA5U__fRGS0_Q__FT1tQ__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA4U__fRGS0_Q__FT1tQ__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA3U__fRGS0_Q__FT1tQ__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA2U__fRGS0_Q__FT1tQ__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA1U__fRGS0_Q__FT1tQ__Si
//CHECK: exp1
//CHECK: function_ref @_TTSg5Si___TFV16specialize_chain3YYYCU__fMGS0_Q__FT1tQ__GS0_Q__
//CHECK: function_ref @_TTSg5Si___TFV16specialize_chain3YYY4AAA9U__fRGS0_Q__FT1tQ__Si
//CHECK: return
