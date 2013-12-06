// RUN: %swift -O3 -emit-sil %s | FileCheck %s

struct XXX<T> {
  init(t : T) {m_t = t}
  func foo(t : T) -> Int {m_t = t; return 4}
  var m_t : T
}

extension XXX {
  @transparent
  func bar(x : T) { self.m_t = x}
}

var J = XXX<Int>(4)
J.bar(3)

//Make sure that we are able to specialize the extension 'bar'
//CHECK: sil internal [transparent] @_TV14specialize_ext3XXX3barU__fRGS0_Q__FT1xQ__T__spec0

