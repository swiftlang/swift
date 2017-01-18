// RUN: %target-swift-frontend  -O -Xllvm -sil-disable-pass="Function Signature Optimization" -emit-sil -primary-file %s | %FileCheck %s

// We can't deserialize apply_inst with subst lists. When radar://14443304
// is fixed then we should convert this test to a SIL test.

struct YYY<T> {
  @inline(never)
  init(t : T) {m_t = t}
  @inline(never) mutating
  func AAA9(t t : T) -> Int { return AAA8(t: t) }
  @inline(never) mutating
  func AAA8(t t : T) -> Int { return AAA7(t: t) }
  @inline(never) mutating
  func AAA7(t t : T) -> Int { return AAA6(t: t) }
  @inline(never) mutating
  func AAA6(t t : T) -> Int { return AAA5(t: t) }
  @inline(never) mutating
  func AAA5(t t : T) -> Int { return AAA4(t: t) }
  @inline(never) mutating
  func AAA4(t t : T) -> Int { return AAA3(t: t) }
  @inline(never) mutating
  func AAA3(t t : T) -> Int { return AAA2(t: t) }
  @inline(never) mutating
  func AAA2(t t : T) -> Int { return AAA1(t: t) }
  @inline(never) mutating
  func AAA1(t t : T) -> Int { return AAA0(t: t) }
  @inline(never) mutating
  func AAA0(t t : T) -> Int { return foo(t: t) }
  @inline(never) mutating
  func foo(t t : T) -> Int { m_t = t; return 4 }
  var m_t : T
}

func exp1() {
  var II = YYY<Int>(t: 5)
  print(II.AAA9(t: 4), terminator: "")
}
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA9
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA8
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA7
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA6
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA5
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA4
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA3
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA2
//CHECK: sil shared [noinline] @_TTSg5Si___TFV16specialize_chain3YYY4AAA1
//CHECK: exp1
//CHECK: function_ref @_TTSg5Si___TFV16specialize_chain3YYYC
//CHECK: function_ref @_TTSg5Si___TFV16specialize_chain3YYY4AAA9
//CHECK: return
