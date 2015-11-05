// RUN: %complete-test -raw -tok=INIT_NAME %s | FileCheck %s -check-prefix=INIT_NAME
// RUN: %complete-test -raw -tok=METHOD_NAME %s | FileCheck %s -check-prefix=METHOD_NAME

struct S {
  init(a: Int, b: Int) {}
  func foo(a: Int, b: Int) {}
}

func test01() {
  S(#^INIT_NAME^#)
}
// INIT_NAME: key.name: "a:b:)"

func test02(x: S) {
  x.#^METHOD_NAME^#
}
// METHOD_NAME: key.name: "foo(a:b:)"
