// RUN: %swift -sil-inline-threshold 0 -O3 -emit-sil %s | FileCheck %s

struct XXX<T> {
  init(t : T) {m_t = t}
  mutating
  func foo(t : T) -> Int {m_t = t; return 4}
  var m_t : T
}

extension XXX {
  @transparent mutating
  func bar(x : T) { self.m_t = x}
}

func exp1() {
  var J = XXX<Int>(t: 4)
  J.bar(3)
}
//Make sure that we are able to specialize the extension 'bar'
//CHECK: sil shared [transparent] @_TTSSi___TFV14specialize_ext3XXX3barU__fRGS0_Q__FQ_T_

