struct S {
  init(foo: Int) {}
}
func takeS(_: S, other: Int) {}

func test() {
  takeS(S(, other: 2)
}

// RUN: %sourcekitd-test -req=complete -pos=7:11 %s -- %s | %FileCheck %s
// RUN: %sourcekitd-test -req=complete.open -pos=7:11 %s -- %s | %FileCheck %s

// CHECK: key.kind: source.lang.swift.decl.function.constructor
// CHECK-NEXT: key.name: "foo:"
