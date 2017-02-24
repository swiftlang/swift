// RUN: %target-typecheck-verify-swift -typecheck %s -verify -swift-version 4

func needsSameType<T>(_: T.Type, _: T.Type) {}

protocol Foo {}
func needsFoo<T: Foo>(_: T.Type) {}

protocol Foo2 {}
func needsFoo2<T: Foo2>(_: T.Type) {}

extension Int: Foo, Foo2 {}
extension Float: Foo {}

protocol Conforms {
    associatedtype T where T: Foo
}
func needsConforms<X: Conforms>(_: X.Type) {
    needsFoo(X.T.self)
}
struct ConcreteConforms: Conforms { typealias T = Int }
struct ConcreteConforms2: Conforms { typealias T = Int }
struct ConcreteConformsNonFoo2: Conforms { typealias T = Float }

protocol NestedConforms {
    associatedtype U where U: Conforms, U.T: Foo2

    func foo(_: U)
}
extension NestedConforms { func foo(_: U) {} }
func needsNestedConforms<X: NestedConforms>(_: X.Type) {
    needsConforms(X.U.self)
    needsFoo(X.U.T.self)
    needsFoo2(X.U.T.self)
}
struct ConcreteNestedConforms: NestedConforms {
    typealias U = ConcreteConforms
}
struct ConcreteNestedConformsInfer: NestedConforms {
    func foo(_: ConcreteConforms) {}
}
struct BadConcreteNestedConforms: NestedConforms {
    // expected-error@-1 {{type 'ConcreteConformsNonFoo2.T' (aka 'Float') does not conform to protocol 'Foo2'}}
    typealias U = ConcreteConformsNonFoo2
}
struct BadConcreteNestedConformsInfer: NestedConforms {
    // expected-error@-1 {{type 'ConcreteConformsNonFoo2.T' (aka 'Float') does not conform to protocol 'Foo2'}}
    func foo(_: ConcreteConformsNonFoo2) {}
}

protocol NestedConformsDefault {
    associatedtype U = ConcreteConforms where U: Conforms, U.T: Foo2
}
struct ConcreteNestedConformsDefaultDefaulted: NestedConformsDefault {}
struct ConcreteNestedConformsDefault: NestedConformsDefault {
    typealias U = ConcreteConforms2
}
func needsNestedConformsDefault<X: NestedConformsDefault>(_: X.Type) {
    needsConforms(X.U.self)
    needsFoo(X.U.T.self)
    needsFoo2(X.U.T.self)
}

protocol NestedSameType {
    associatedtype U: Conforms where U.T == Int

    func foo(_: U)
}
extension NestedSameType { func foo(_: U) {} }
func needsNestedSameType<X: NestedSameType>(_: X.Type) {
    needsConforms(X.U.self)
    needsSameType(X.U.T.self, Int.self)
}
struct BadConcreteNestedSameType: NestedSameType {
    // expected-error@-1 {{'NestedSameType' requires the types 'ConcreteConformsNonFoo2.T' (aka 'Float') and 'Int' be equivalent}}
    // expected-note@-2 {{requirement specified as 'Self.U.T' == 'Int' [with Self = BadConcreteNestedSameType]}}
    typealias U = ConcreteConformsNonFoo2
}
struct BadConcreteNestedSameTypeInfer: NestedSameType {
    // expected-error@-1 {{'NestedSameType' requires the types 'ConcreteConformsNonFoo2.T' (aka 'Float') and 'Int' be equivalent}}
    // expected-note@-2 {{requirement specified as 'Self.U.T' == 'Int' [with Self = BadConcreteNestedSameTypeInfer]}}
    func foo(_: ConcreteConformsNonFoo2) {}
}

protocol NestedSameTypeDefault {
    associatedtype U: Conforms = ConcreteConforms where U.T == Int

    func foo(_: U)
}
extension NestedSameTypeDefault { func foo(_: U) {} }
func needsNestedSameTypeDefault<X: NestedSameTypeDefault>(_: X.Type) {
    needsConforms(X.U.self)
    needsSameType(X.U.T.self, Int.self)
}
struct ConcreteNestedSameTypeDefaultDefaulted: NestedSameTypeDefault {}
struct ConcreteNestedSameTypeDefault: NestedSameTypeDefault {
    typealias U = ConcreteConforms2
}
struct ConcreteNestedSameTypeDefaultInfer: NestedSameTypeDefault {
    func foo(_: ConcreteConforms2) {}
}
