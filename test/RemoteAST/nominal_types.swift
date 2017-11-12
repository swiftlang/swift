// RUN: %target-swift-remoteast-test %s | %FileCheck %s

// REQUIRES: swift-remoteast-test

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
  struct Bar<T> {}
}
printType(G.self)
// CHECK: G
printType(G.Foo.self)
// CHECK: G.Foo
printType(G.Bar<A>.self)
// CHECK: G.Bar<A>

struct H<T, U> {
  struct Foo {}
  struct Bar<V, W> {}
}
printType(H<A,A>.self)
// CHECK: H<A, A>
printType(H<B.Foo, H<B, A>>.self)
// CHECK: H<B.Foo, H<B, A>>
printType(H<B, B>.Foo.self)
// CHECK: H<B, B>.Foo
printType(H<A, B>.Bar<B, A>.self)
// CHECK: H<A, B>.Bar<B, A>

class I<T> {}
printType(I<Int>.self)
// CHECK: I<Int>

protocol J {}

printType(J.self)
// CHECK: found type: J
printType(J.Protocol.self)
// CHECK: found type: J.Protocol
printType(J.Type.self)
// CHECK: found type: J.Type

protocol K {}
typealias JK = J & K
typealias KJ = K & J
printType(JK.self)
// CHECK: found type: J & K
printType(KJ.self)
// CHECK: found type: J & K
