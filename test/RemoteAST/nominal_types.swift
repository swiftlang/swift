// RUN: %target-swift-remoteast-test %s | FileCheck %s

// XFAIL: linux

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

printType(Int.self)
// CHECK: Int

struct A {}
printType(A.self)
// CHECK: A

struct B {
  struct Foo {}
}
printType(B.Foo.self)
// CHECK: B.Foo

extension B {
  struct Bar {}
}
printType(B.Bar.self)
// CHECK: B.Bar

class C {
}
printType(C.self)
// CHECK: C

class D : C {
}
printType(D.self)
// CHECK: D

class E {
  struct Foo {}
}
printType(E.Foo.self)
// CHECK: E.Foo

struct F {
  class Foo {}
}
printType(F.Foo.self)
// CHECK: F.Foo

enum G {
  case Gwhatever

  struct Foo {}
}
printType(G.self)
// CHECK: G
printType(G.Foo.self)
// CHECK: G.Foo
