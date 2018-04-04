// Checks that we don't crash
// RUN: %sourcekitd-test -req=cursor -pos=10:5 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=cursor -pos=11:11 %s -- %s | %FileCheck --check-prefix=CHECK2 %s
// CHECK: source.lang.swift.ref.class
// CHECK2: source.lang.swift.ref.function.constructor

class Bar<T> {
  class Inner {}
  func foo() {
    Inner()
    Inner.init()
  }
}
