// RUN: %complete-test -raw -tok=INIT_NAME %s | %FileCheck %s -check-prefix=INIT_NAME
// RUN: %complete-test -raw -tok=METHOD_NAME %s | %FileCheck %s -check-prefix=METHOD_NAME

struct S {
  init(a: Int, b: Int, _ c: Int) {}
  init(_ a: Int, _ b: Int) {}
  func foo1(_ a: Int, _ b: Int, _ c: Int, _ d: Int..., _ e: inout Int) {}
  func foo2(a a: Int, b: Int, c: Int, d: Int..., e: inout Int) {}
}

func test01() {
  S(#^INIT_NAME^#)
}
// INIT_NAME: key.name: "a:b::"

func test02(_ x: S) {
  x.#^METHOD_NAME^#
}
// METHOD_NAME: key.name: "foo1(:::::)"
// METHOD_NAME: key.name: "foo2(a:b:c:d:e:)"
