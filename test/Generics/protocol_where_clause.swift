// RUN: %target-typecheck-verify-swift -swift-version 4 -print-ast %s | %FileCheck %s

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
struct BadConcreteSameType: SameType { // expected-error{{'SameType' requires the types 'BadConcreteSameType.T' (aka 'Int') and 'BadConcreteSameType.U' (aka 'Float') be equivalent}}
	// expected-note@-1{{requirement specified as 'Self.T' == 'Self.U' [with Self = BadConcreteSameType]}}
  typealias T = Int
  typealias U = Float
}

// <rdar://problem/31041997> Some where-clause requirements aren't checked on conforming types
protocol NestedConforms: SecondParent where U: Parent, U.T: Foo2 {}
func needsNestedConforms<X: NestedConforms>(_: X.Type) {
  needsParent(X.U.self)
  needsFoo2(X.U.T.self)
}
struct ConcreteNestedConforms: NestedConforms {
  typealias U = ConcreteParent
}
struct BadConcreteNestedConforms: NestedConforms {
  // expected-error@-1 {{type 'ConcreteParentNonFoo2.T' (aka 'Float') does not conform to protocol 'Foo2'}}
  typealias U = ConcreteParentNonFoo2
}


// SR4693:
protocol P1 {}
struct X: P1 {}
struct Y<T: P1> {}

protocol P2 {
  associatedtype AT
}
protocol P3: P2 where AT == Y<X> {}


// SR-10831:
struct G<T> {}

protocol SR10831_P1 {
  associatedtype A
  associatedtype B
  associatedtype C
}

protocol SR10831_P2: SR10831_P1 where A == G<G<C?>> {}
protocol SR10831_P3: SR10831_P2 where B == Int, C == G<B> {}

struct SR10831: SR10831_P3 { // OK
  // CHECK: typealias A = G<G<G<Int>?>>
  // CHECK: typealias B = Int
  // CHECK: typealias C = G<Int>
}


// SR-11671:
protocol SR11671_P1 {
  associatedtype A
  associatedtype B
}
protocol SR11671_P2: SR11671_P1 where A == Self {}
protocol SR11671_P3: SR11671_P2 where B == G<Self?> {}

struct SR11671_S1: SR11671_P3 { // OK
  // CHECK: typealias A = SR11671_S1
  // CHECK: typealias B = G<SR11671_S1?>
}

struct SR11671_S2<T, U>: SR11671_P3 { // OK
  // CHECK: typealias A = SR11671_S2<T, U>
  // CHECK: typealias B = G<SR11671_S2<T, U>?>
}
