// RUN: %target-swift-remoteast-test %s | FileCheck %s

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

struct A {
  struct Foo {}
}
printType(A.Foo.self)

printType(Int.self)
// CHECK: Int

struct Foo {}
printType(Foo.self)
// CHECK: Foo

struct Bar {
  struct Foo {}
}
printType(Bar.Foo.self)
// CHECK: Bar.Foo

extension Bar {
  struct Baz {}
}
printType(Bar.Baz.self)
// CHECK: Bar.Baz
