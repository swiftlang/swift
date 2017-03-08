protocol P1 {
  func foo()
}
protocol P2 {
  func bar()
}
class C1: P1, P2 {}

// RUN: %sourcekitd-test -req=sema %s -- %s > %t.response
// RUN: diff -u %s.response %t.response
