// RUN: %target-typecheck-verify-swift -typecheck %s -verify -swift-version 4

func needsSameType<T>(_: T.Type, _: T.Type) {}

protocol Foo {}
func needsFoo<T: Foo>(_: T.Type) {}

protocol Foo2 {}
func needsFoo2<T: Foo2>(_: T.Type) {}

extension Int: Foo, Foo2 {}
extension Float: Foo {}

protocol Parent {
    associatedtype T
}
func needsParent<X: Parent>(_: X.Type) {}
struct ConcreteParent: Parent { typealias T = Int }
struct ConcreteParent2: Parent { typealias T = Int }
struct ConcreteParentNonFoo2: Parent { typealias T = Float }

protocol SecondParent {
    associatedtype U
}
struct ConcreteSecondParent: SecondParent { typealias U = Int }
struct ConcreteSecondParent2: SecondParent { typealias U = Int }
struct ConcreteSecondParentNonFoo2: SecondParent { typealias U = Float }

protocol Conforms: Parent where T: Foo {}
extension Conforms { func foo(_: T) {} }
func needsConforms<X: Conforms>(_: X.Type) {
    needsFoo(X.T.self)
}
struct ConcreteConforms: Conforms {
    typealias T = Int
}
struct BadConcreteConforms: Conforms {
    // expected-error@-1 {{type 'BadConcreteConforms.T' (aka 'String') does not conform to protocol 'Foo'}}
    typealias T = String
}

protocol SameType: Parent, SecondParent where T == U { }
func needsSameTypeProtocol<X: SameType>(_: X.Type) {
    needsSameType(X.T.self, X.U.self)
}
struct ConcreteSameType: SameType {
    typealias T = Int
    typealias U = Int
}
struct BadConcreteSameType: SameType {
    typealias T = Int
    typealias U = Float
}

protocol NestedConforms: SecondParent where U: Parent, U.T: Foo2 {}
func needsNestedConforms<X: NestedConforms>(_: X.Type) {
    needsParent(X.U.self)
    needsFoo2(X.U.T.self)
}
struct ConcreteNestedConforms: NestedConforms {
    typealias U = ConcreteParent
}
// FIXME: this should hit "type 'BadConcreteNestedConforms.U.T' (aka 'Float')
// does not conform to protocol 'Foo2'", but the SubstitutionMap used in
// ensureRequirementsAreSatified can't find the conformance of Self.U: Parent
// because it only looks for the conformances implied directly by a protocol's
// associated types. SubstitutionMap should use the requirement
// signature/conformance access path.
struct BadConcreteNestedConforms: NestedConforms {
    typealias U = ConcreteParentNonFoo2
}
