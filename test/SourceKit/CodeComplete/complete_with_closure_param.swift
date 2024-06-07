typealias MyFnTy = Int -> Int
class C {
  func foo(_ x: Int ->Int) {}
  func foo2(_ x: MyFnTy) {}
}

C().

// RUN: %sourcekitd-test -req=complete -pos=7:5 %s -- %s | %FileCheck %s

// CHECK:      key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-NEXT: key.name: "foo(:)",
// CHECK-NEXT: key.description: "foo(x: (Int) -> Int)",
// CHECK-NEXT: key.typename: "Void",
// CHECK: key.sourcetext: "foo(<#T##x: (Int) -> Int##(Int) -> Int#>)"

// CHECK:      key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-NEXT: key.name: "foo2(:)",
// CHECK-NEXT: key.description: "foo2(x: (Int) -> Int)",
// CHECK: key.sourcetext: "foo2(<#T##x: (Int) -> Int##(Int) -> Int#>)"
