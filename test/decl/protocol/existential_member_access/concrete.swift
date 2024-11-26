// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

struct Struct<T> {
  class Nested {}
  struct NestedGeneric<U> {}
}
class Class<T> {}

// Test that a reference to a 'Self'-rooted dependent member type does not
// affect the ability to reference a protocol member on an existential when
// it is *fully* concrete.
protocol ConcreteAssocTypes {
  associatedtype A1 where A1 == Struct<Self>
  associatedtype A2 where A2 == (Bool, Self)
  associatedtype A3 where A3 == any Class<A4> & ConcreteAssocTypes
  associatedtype A4

  func method1(_: A1)
  func method2() -> Struct<A2>
  func method3(_: A3)

  var property1: A1 { get }
  var property2: A2 { get }
  var property3: A3 { get }

  subscript(subscript1 _: A3) -> Bool { get }
  subscript(subscript2 _: Bool) -> A1 { get }
  subscript(subscript3 _: A2) -> Bool { get }

  associatedtype A5 where A5 == Bool
  associatedtype A6 where A6 == any ConcreteAssocTypes
  associatedtype A7 where A7 == A8.A5
  associatedtype A8: ConcreteAssocTypes

  func method4(_: Struct<A5>, _: A6.Type, _: () -> A5) -> any Class<Struct<A7>.Nested> & ConcreteAssocTypes

  var property4: (Struct<A5>, A6.Type, () -> A5) -> any Class<Struct<A7>.Nested> & ConcreteAssocTypes { get }

  subscript(subscript4 _: Struct<A5>, _: A6.Type, _: () -> A5) -> any Class<Struct<A7>.Nested> & ConcreteAssocTypes { get }
}
do {
  let exist: any ConcreteAssocTypes

  let _ = exist.method1 // expected-error {{member 'method1' cannot be used on value of type 'any ConcreteAssocTypes'; consider using a generic constraint instead}}
  let _ = exist.method2 // expected-error {{member 'method2' cannot be used on value of type 'any ConcreteAssocTypes'; consider using a generic constraint instead}}
  let _ = exist.method3 // expected-error {{member 'method3' cannot be used on value of type 'any ConcreteAssocTypes'; consider using a generic constraint instead}}
  let _ = exist.property1 // expected-error {{member 'property1' cannot be used on value of type 'any ConcreteAssocTypes'; consider using a generic constraint instead}}
  // Covariant 'Self' erasure works in conjunction with concrete associated types.
  let _: (Bool, any ConcreteAssocTypes) = exist.property2 // ok
  let _ = exist.property3 // expected-error {{member 'property3' cannot be used on value of type 'any ConcreteAssocTypes'; consider using a generic constraint instead}}
  let _ = exist[subscript1: false] // expected-error {{member 'subscript' cannot be used on value of type 'any ConcreteAssocTypes'; consider using a generic constraint instead}}
  let _ = exist[subscript2: false] // expected-error {{member 'subscript' cannot be used on value of type 'any ConcreteAssocTypes'; consider using a generic constraint instead}}
  let _ = exist[subscript3: false] // expected-error {{member 'subscript' cannot be used on value of type 'any ConcreteAssocTypes'; consider using a generic constraint instead}}

  let _: (
    Struct<Bool>, (any ConcreteAssocTypes).Type, () -> Bool
  ) -> any Class<Struct<Bool>.Nested> & ConcreteAssocTypes = exist.method4

  let _: (
    Struct<Bool>, (any ConcreteAssocTypes).Type, () -> Bool
  ) -> any Class<Struct<Bool>.Nested> & ConcreteAssocTypes = exist.property4

  let _: any Class<Struct<Bool>.Nested> & ConcreteAssocTypes =
  exist[
    subscript4: Struct<Bool>(), (any ConcreteAssocTypes).self, { true }
  ]
}

protocol ConcreteAssocTypeComposition1 {
  associatedtype A
  func method(_: A)
}
protocol ConcreteAssocTypeComposition2 where A == Bool {
  associatedtype A
}
do {
  let exist: any ConcreteAssocTypeComposition1 & ConcreteAssocTypeComposition2
  exist.method(true) // ok
}

// Edge case: an associated type can be resolved through a class conformance.
class Class1Simple: ConcreteAssocTypeThroughClass {
  typealias A = Bool
}
class Class1Generic<A>: ConcreteAssocTypeThroughClass {
}
protocol ConcreteAssocTypeThroughClass {
  associatedtype A
}
protocol ConcreteAssocTypeThroughClassRefined: ConcreteAssocTypeThroughClass {
  func method(_: A)
}
extension ConcreteAssocTypeThroughClassRefined {
  func test(arg1: any ConcreteAssocTypeThroughClassRefined & Class1Generic<Self>,
            arg2: any ConcreteAssocTypeThroughClassRefined & Class1Simple) {
    arg1.method(self) // ok
    arg2.method(true) // ok
  }
}

protocol ConcreteAssocTypeCollision1 where A == Bool {
  associatedtype A
  func method(_: A)
}
protocol ConcreteAssocTypeCollision2 where A == Never {
  associatedtype A
}
do {
  let exist: any ConcreteAssocTypeCollision1 & ConcreteAssocTypeCollision2
  // FIXME: Should 'A' be ambiguous here?
  exist.method(true)
}

class BadConformanceClass: CompositionBrokenClassConformance_a {}
// expected-error@-1 {{type 'BadConformanceClass' does not conform to protocol 'CompositionBrokenClassConformance_a'}}
// expected-note@-2 {{add stubs for conformance}}
protocol CompositionBrokenClassConformance_a {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
}
protocol CompositionBrokenClassConformance_b: CompositionBrokenClassConformance_a {
  func method(_: A)
}
do {
  // FIXME: Should GenericSignature::getConcreteType return the null type instead
  // of the error type here for Self.A, despite the broken conformance?
  let exist: any CompositionBrokenClassConformance_b & BadConformanceClass
  exist.method(false) // expected-error {{type of expression is ambiguous without a type annotation}}
}
