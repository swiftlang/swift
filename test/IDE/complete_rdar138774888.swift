// RUN: %batch-code-completion

// rdar://138774888 - Make sure we don't crash

class C1 {}
extension C1 {
  func foo() {}
}
protocol P1 where Self: C1 {}

func bar(_ x: P1) {
  x.#^COMPLETE1^#
  // COMPLETE1: Decl[InstanceMethod]/CurrNominal:   foo()[#Void#]; name=foo()
}

class C2<T> {}
extension C2 where T == String {
  func foo() {}
}
extension C2 where T == Int {
  func bar() {}
}

protocol P2 where Self: C2<Int> {}

func bar(_ x: P2) {
  x.#^COMPLETE2^#
  // COMPLETE2: Decl[InstanceMethod]/CurrNominal:   bar()[#Void#]; name=bar()
}
