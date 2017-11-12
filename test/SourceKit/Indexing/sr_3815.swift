// RUN: %sourcekitd-test -req=index %s -- -serialize-diagnostics-path %t.dia %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

protocol P {
  typealias Index = Int
  func f()
}

struct S : P {
  typealias Index = Int

  func f() {}
}
