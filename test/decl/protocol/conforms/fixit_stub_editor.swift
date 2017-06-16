// RUN: %target-typecheck-verify-swift -diagnostics-editor-mode

/*
protocol P1 {
  @available(iOS, unavailable)
  func foo1()
  func foo2()
}

protocol P2 {
  func bar1()
  func bar2()
}

class C1 : P1, P2 {}

protocol P3 {
  associatedtype T1
  associatedtype T2
  associatedtype T3
}

protocol P4 : P3 {
  associatedtype T4 = T1
  associatedtype T5 = T2
  associatedtype T6 = T3
}

class C2 : P4 {}
*/
