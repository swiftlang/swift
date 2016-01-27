// RUN: %complete-test -raw -tok=INIT_NAME %s | FileCheck %s -check-prefix=INIT_NAME
// RUN: %complete-test -raw -tok=METHOD_NAME %s | FileCheck %s -check-prefix=METHOD_NAME

struct S {
  init(a: Int, b: Int, _ c: Int) {}
  init(_ a: Int, _ b: Int) {}
  func foo1(a: Int, _ b: Int, _ c: Int) {}
  func foo2(a a: Int, b: Int, c: Int) {}
}

func test01() {
  S(#^INIT_NAME^#)
}
// INIT_NAME: key.name: "a:b::)"

func test02(x: S) {
  x.#^METHOD_NAME^#
}
// METHOD_NAME: key.name: "foo1(:::)"
// METHOD_NAME: key.name: "foo2(a:b:c:)"
