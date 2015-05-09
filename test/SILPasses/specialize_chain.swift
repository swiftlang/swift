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
  print(II.AAA9(t: 4), appendNewline: false)
}
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA9urfRGS0_q__FT1tq__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA8urfRGS0_q__FT1tq__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA7urfRGS0_q__FT1tq__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA6urfRGS0_q__FT1tq__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA5urfRGS0_q__FT1tq__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA4urfRGS0_q__FT1tq__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA3urfRGS0_q__FT1tq__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA2urfRGS0_q__FT1tq__Si
//CHECK: sil shared @_TTSg5Si___TFV16specialize_chain3YYY4AAA1urfRGS0_q__FT1tq__Si
//CHECK: exp1
//CHECK: function_ref @_TTSg5Si___TFV16specialize_chain3YYYCurfMGS0_q__FT1tq__GS0_q__
//CHECK: function_ref @_TTSg5Si___TFV16specialize_chain3YYY4AAA9urfRGS0_q__FT1tq__Si
//CHECK: return
