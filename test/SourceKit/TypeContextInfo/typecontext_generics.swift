struct S<T> {
  func foo<U>(x: U) {}
  func bar<V>(x: S<V>) {}
  func baz(x: Self) {}

  func test() {
    foo(x: )
    bar(x: )
    self.baz(x: )
  }

  func test2<X>(x: X) {
    var value = S<X>()
    value.foo(x: )
    value.bar(x: )
    value.baz(x: )
  }

  static var instance: Self = S<T>()
}

// RUN: %sourcekitd-test -req=typecontextinfo -pos=7:12 %s -- %s > %t.response.1
// RUN: %diff -u %s.response.1 %t.response.1
// RUN: %sourcekitd-test -req=typecontextinfo -pos=8:12 %s -- %s > %t.response.2
// RUN: %diff -u %s.response.2 %t.response.2
// RUN: %sourcekitd-test -req=typecontextinfo -pos=9:17 %s -- %s > %t.response.3
// RUN: %diff -u %s.response.3 %t.response.3

// RUN: %sourcekitd-test -req=typecontextinfo -pos=14:18 %s -- %s > %t.response.4
// RUN: %diff -u %s.response.4 %t.response.4
// RUN: %sourcekitd-test -req=typecontextinfo -pos=15:18 %s -- %s > %t.response.5
// RUN: %diff -u %s.response.5 %t.response.5
// RUN: %sourcekitd-test -req=typecontextinfo -pos=16:18 %s -- %s > %t.response.6
// RUN: %diff -u %s.response.6 %t.response.6
