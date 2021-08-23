// RUN: %sourcekitd-test -req=index %s -- -Xfrontend -serialize-diagnostics-path -Xfrontend %t.dia %s -Xfrontend -disable-implicit-concurrency-module-import | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

protocol P {
  typealias Index = Int
  func f()
}

struct S : P {
  typealias Index = Int

  func f() {}
}
