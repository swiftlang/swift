// RUN: %target-typecheck-verify-swift -swift-version 4

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
    associatedtype U where U: Conforms, U.T: Foo2 // expected-note{{protocol requires nested type 'U'}}

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
// expected-error@-1 {{type 'BadConcreteNestedConforms' does not conform to protocol 'NestedConforms'}}
// expected-error@-2 {{type 'ConcreteConformsNonFoo2.T' (aka 'Float') does not conform to protocol 'Foo2'}}
    typealias U = ConcreteConformsNonFoo2
}
struct BadConcreteNestedConformsInfer: NestedConforms {
    // expected-error@-1 {{type 'BadConcreteNestedConformsInfer' does not conform to protocol 'NestedConforms'}}
    // expected-note@-2{{add stubs for conformance}}
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
    associatedtype U: Conforms where U.T == Int // expected-note{{protocol requires nested type 'U'}}

    func foo(_: U)
}
extension NestedSameType { func foo(_: U) {} }
func needsNestedSameType<X: NestedSameType>(_: X.Type) {
    needsConforms(X.U.self)
    needsSameType(X.U.T.self, Int.self)
}
struct BadConcreteNestedSameType: NestedSameType {
// expected-error@-1 {{type 'BadConcreteNestedSameType' does not conform to protocol 'NestedSameType'}}
// expected-error@-2 {{'NestedSameType' requires the types 'ConcreteConformsNonFoo2.T' (aka 'Float') and 'Int' be equivalent}}
// expected-note@-3 {{requirement specified as 'Self.U.T' == 'Int' [with Self = BadConcreteNestedSameType]}}

    typealias U = ConcreteConformsNonFoo2
}
struct BadConcreteNestedSameTypeInfer: NestedSameType {
    // expected-error@-1 {{type 'BadConcreteNestedSameTypeInfer' does not conform to protocol 'NestedSameType'}}
    // expected-note@-2{{add stubs for conformance}}
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

protocol Inherits: NestedConforms {
    associatedtype X: Conforms where X.T == U.T

    func bar(_: X)
}
extension Inherits { func bar(_: X) {} }
func needsInherits<X: Inherits>(_: X.Type) {
    needsConforms(X.U.self)
    needsConforms(X.X.self)
    needsSameType(X.U.T.self, X.X.T.self)
}
struct ConcreteInherits: Inherits {
    typealias U = ConcreteConforms
    typealias X = ConcreteConforms
}
struct ConcreteInheritsDiffer: Inherits {
    typealias U = ConcreteConforms
    typealias X = ConcreteConforms2
}

struct BadConcreteInherits: Inherits {
// expected-error@-1 {{type 'BadConcreteInherits' does not conform to protocol 'Inherits'}}
// expected-error@-2 {{'Inherits' requires the types 'ConcreteConforms.T' (aka 'Int') and 'ConcreteConformsNonFoo2.T' (aka 'Float') be equivalent}}
// expected-note@-3 {{requirement specified as 'Self.U.T' == 'Self.X.T' [with Self = BadConcreteInherits]}}
    typealias U = ConcreteConforms
    typealias X = ConcreteConformsNonFoo2
}

struct X { }

protocol P {
	associatedtype P1 where P1 == X
	associatedtype P2 where P2 == P1, P2 == X
}

// Lookup of same-named associated types aren't ambiguous in this context.
protocol P1 {
  associatedtype A
}

protocol P2: P1 {
  associatedtype A
  associatedtype B where A == B
}

protocol P3: P1 {
  associatedtype A
}

protocol P4 {
  associatedtype A
}

protocol P5: P3, P4 {
  associatedtype B where B == A?
}

// Associated type inference should account for where clauses.
protocol P6 {
  associatedtype A
}

struct X1 { }

struct X2 { }

struct Y1 : P6 {
  typealias A = X1
}

struct Y2 : P6 {
  typealias A = X2
}

protocol P7 {
  associatedtype B: P6 // expected-note{{ambiguous inference of associated type 'B': 'Y1' vs. 'Y2'}}
  associatedtype C: P6 where B.A == C.A

  func getB() -> B
  func getC() -> C
}

struct Z1 : P7 {
  func getB() -> Y1 { return Y1() }
  func getB() -> Y2 { return Y2() }

  func getC() -> Y1 { return Y1() }
}

func testZ1(z1: Z1) {
  let _: Z1.C = Y1()
}


struct Z2 : P7 { // expected-error{{type 'Z2' does not conform to protocol 'P7'}}
  func getB() -> Y1 { return Y1() } // expected-note{{matching requirement 'getB()' to this declaration inferred associated type to 'Y1'}}
  func getB() -> Y2 { return Y2() } // expected-note{{matching requirement 'getB()' to this declaration inferred associated type to 'Y2'}}

  func getC() -> Y1 { return Y1() }
  func getC() -> Y2 { return Y2() }
}
