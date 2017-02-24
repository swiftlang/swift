// RUN: %target-typecheck-verify-swift -typecheck %s -verify -swift-version 4

func needsSameType<T>(_: T.Type, _: T.Type) {}

protocol Foo {}
func needsFoo<T: Foo>(_: T.Type) {}

protocol Foo2 {}
func needsFoo2<T: Foo2>(_: T.Type) {}

extension Int: Foo {}

protocol Conforms {
    associatedtype T where T: Foo
}
func needsConforms<X: Conforms>(_: X.Type) {
    needsFoo(X.T.self)
}
struct ConcreteConforms: Conforms { typealias T = Int }
struct ConcreteConforms2: Conforms { typealias T = Int }

protocol NestedConforms {
    associatedtype U where U: Conforms, U.T: Foo2
}
func needsNestedConforms<X: NestedConforms>(_: X.Type) {
    needsConforms(X.U.self)
    needsFoo(X.U.T.self)
    needsFoo2(X.U.T.self)
}
struct ConcreteNestedConforms: NestedConforms {
    typealias U = ConcreteConforms
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
}
func needsNestedSameType<X: NestedSameType>(_: X.Type) {
    needsConforms(X.U.self)
    needsSameType(X.U.T.self, Int.self)
}

protocol NestedSameTypeDefault {
    associatedtype U: Conforms = ConcreteConforms where U.T == Int
}
func needsNestedSameTypeDefault<X: NestedSameTypeDefault>(_: X.Type) {
    needsConforms(X.U.self)
    needsSameType(X.U.T.self, Int.self)
}
struct ConcreteNestedSameTypeDefaultDefaulted: NestedSameTypeDefault {}
struct ConcreteNestedSameTypeDefault: NestedSameTypeDefault {
    typealias U = ConcreteConforms2
}
