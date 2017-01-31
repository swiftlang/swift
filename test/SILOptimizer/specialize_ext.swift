// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests  -O -emit-sil -primary-file %s | %FileCheck %s

struct XXX<T> {
  init(t : T) {m_t = t}
  mutating
  func foo(_ t : T) -> Int {m_t = t; return 4}
  var m_t : T
}

extension XXX {
  @inline(never)
  mutating
  func bar(_ x : T) { self.m_t = x}
}

public func exp1() {
  var J = XXX<Int>(t: 4)
  J.bar(3)
}
// Make sure that we are able to specialize the extension 'bar'
//CHECK: sil shared [noinline] @_T014specialize_ext3XXXV3bar{{[_0-9a-zA-Z]*}}FSi_Tg5
