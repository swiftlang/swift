// RUN: %target-swift-frontend %use_no_opaque_pointers -emit-ir %s -module-name x | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s -module-name x

// rdar://problem/40078863 - witness signatures get adjusted and the
// ProtocolConformances accompanying them did not, resulting in an extra witness
// table parameter.

protocol Foo {
    func bar()
}
extension Foo {
    func bar() {}
}
class Box<T> {}
extension Box: Foo where T: Foo {}
// CHECK-LABEL: define internal swiftcc void @"$s1x3BoxCyqd__GAA3FooA2aEP3baryyFTW"(%T1x3BoxC.0** noalias nocapture swiftself dereferenceable({{[48]}}) %0, %swift.type* %Self, i8** %SelfWitnessTable)
