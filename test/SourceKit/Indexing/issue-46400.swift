// RUN: %sourcekitd-test -req=index %s -- -Xfrontend -serialize-diagnostics-path -Xfrontend %t.dia %s -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

// https://github.com/apple/swift/issues/46400

protocol P {
  typealias Index = Int
  func f()
}

struct S : P {
  typealias Index = Int

  func f() {}
}
