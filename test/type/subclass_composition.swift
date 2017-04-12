// RUN: %target-typecheck-verify-swift -enable-experimental-subclass-existentials

protocol P1 {
  typealias DependentInConcreteConformance = Self
}

class Base<T> : P1 {
  typealias DependentClass = T

  func classSelfReturn() -> Self {}
}

protocol P2 {
  typealias FullyConcrete = Int
  typealias DependentProtocol = Self

  func protocolSelfReturn() -> Self
}

typealias BaseAndP2<T> = Base<T> & P2
typealias BaseIntAndP2 = BaseAndP2<Int>

class Derived : Base<Int>, P2 {
  func protocolSelfReturn() -> Self {}
}

class Other : Base<Int> {}

typealias OtherAndP2 = Other & P2

protocol P3 : class {}

//
// If a class conforms to a protocol concretely, the resulting protocol
// composition type should be equivalent to the class type.
//
// FIXME: Not implemented yet.
//

func alreadyConforms<T>(_: Base<T>) {}
func alreadyConforms<T>(_: Base<T> & P1) {}
func alreadyConforms<T>(_: Base<T> & AnyObject) {}
func alreadyConforms<T>(_: Base<T> & P1 & AnyObject) {}

func alreadyConforms(_: P3) {}
func alreadyConforms(_: P3 & AnyObject) {}

// SE-0156 stipulates that a composition can contain multiple classes, as long
// as they are all the same.
func basicDiagnostics(
  _: Base<Int> & Base<Int>,
  _: Base<Int> & Derived, // expected-error{{protocol-constrained type cannot contain class 'Derived' because it already contains class 'Base<Int>'}}

  // Invalid typealias case
  _: Derived & OtherAndP2, // expected-error{{protocol-constrained type cannot contain class 'Other' because it already contains class 'Derived'}}

  // Valid typealias case
  _: OtherAndP2 & P3) {}

// Test that subtyping behaves as you would expect.
func basicSubtyping(
  base: Base<Int>,
  baseAndP1: Base<Int> & P1,
  baseAndP2: Base<Int> & P2,
  baseAndP2AndAnyObject: Base<Int> & P2 & AnyObject,
  baseAndAnyObject: Base<Int> & AnyObject,
  derived: Derived,
  derivedAndP2: Derived & P2,
  derivedAndP3: Derived & P3,
  derivedAndAnyObject: Derived & AnyObject,
  p1AndAnyObject: P1 & AnyObject,
  p2AndAnyObject: P2 & AnyObject) {

  // Errors
  let _: Base & P2 = base // expected-error {{value of type 'Base<Int>' does not conform to specified type 'Base & P2'}}
  let _: Base<Int> & P2 = base // expected-error {{value of type 'Base<Int>' does not conform to specified type 'Base<Int> & P2'}}
  let _: P3 = baseAndP1 // expected-error {{value of type 'Base<Int> & P1' does not conform to specified type 'P3'}}
  let _: P3 = baseAndP2 // expected-error {{value of type 'Base<Int> & P2' does not conform to specified type 'P3'}}
  let _: Derived = baseAndP1 // expected-error {{cannot convert value of type 'Base<Int> & P1' to specified type 'Derived'}}
  let _: Derived = baseAndP2 // expected-error {{cannot convert value of type 'Base<Int> & P2' to specified type 'Derived'}}
  let _: Derived & P2 = baseAndP2 // expected-error {{value of type 'Base<Int> & P2' does not conform to specified type 'Derived & P2'}}

  // No-ops
  let _: Base & P1 = base
  let _: Base<Int> & P1 = base
  let _: Base & AnyObject = base
  let _: Base<Int> & AnyObject = base
  let _: Derived & AnyObject = derived

  // Erasing superclass constraint
  let _: P1 = baseAndP1
  let _: P1 & AnyObject = baseAndP1
  let _: P1 = derived
  let _: P1 & AnyObject = derived
  let _: AnyObject = baseAndP1
  let _: AnyObject = baseAndP2
  let _: AnyObject = derived
  let _: AnyObject = derivedAndP2
  let _: AnyObject = derivedAndP3
  let _: AnyObject = derivedAndAnyObject

  // Erasing conformance constraint
  let _: Base = baseAndP1
  let _: Base<Int> = baseAndP1
  let _: Base = derivedAndP3
  let _: Base<Int> = derivedAndP3
  let _: Derived = derivedAndP2
  let _: Derived = derivedAndAnyObject

  // Upcasts
  let _: Base & P2 = derived
  let _: Base<Int> & P2 = derived
  let _: Base & P2 & AnyObject = derived
  let _: Base<Int> & P2 & AnyObject = derived
  let _: Base & P3 = derivedAndP3
  let _: Base<Int> & P3 = derivedAndP3

  // Calling methods with Self return
  let _: Base & P2 = baseAndP2.classSelfReturn()
  let _: Base<Int> & P2 = baseAndP2.classSelfReturn()
  let _: Base & P2 = baseAndP2.protocolSelfReturn()
  let _: Base<Int> & P2 = baseAndP2.protocolSelfReturn()
}

//
// Looking up member types of subclass existentials.
//

func dependentMemberTypes<T : BaseIntAndP2>(
  _: T.DependentInConcreteConformance,
  _: T.DependentProtocol,
  _: T.DependentClass,
  _: T.FullyConcrete,

  _: BaseIntAndP2.DependentInConcreteConformance, // FIXME expected-error {{}}
  _: BaseIntAndP2.DependentProtocol, // expected-error {{typealias 'DependentProtocol' can only be used with a concrete type or generic parameter base}}
  _: BaseIntAndP2.DependentClass,
  _: BaseIntAndP2.FullyConcrete) {}

func conformsToAnyObject<T : AnyObject>(_: T) {}
func conformsToP1<T : P1>(_: T) {}
func conformsToP2<T : P2>(_: T) {}
func conformsToBaseIntAndP2<T : Base<Int> & P2>(_: T) {}
// expected-note@-1 4 {{in call to function 'conformsToBaseIntAndP2'}}

func conformsToBaseIntAndP2WithWhereClause<T>(_: T) where T : Base<Int> & P2 {}
// expected-note@-1 2 {{in call to function 'conformsToBaseIntAndP2WithWhereClause'}}

class FakeDerived : Base<String>, P2 {
  func protocolSelfReturn() -> Self { return self }
}

//
// Metatype subtyping.
//

func metatypeSubtyping(
  base: Base<Int>.Type,
  derived: Derived.Type,
  derivedAndAnyObject: (Derived & AnyObject).Type,
  baseIntAndP2: (Base<Int> & P2).Type,
  baseIntAndP2AndAnyObject: (Base<Int> & P2 & AnyObject).Type) {

  // Erasing conformance constraint
  let _: Base<Int>.Type = baseIntAndP2
  let _: Base<Int>.Type = baseIntAndP2AndAnyObject
  let _: Derived.Type = derivedAndAnyObject
  let _: BaseAndP2<Int>.Type = baseIntAndP2AndAnyObject

  // Upcast
  let _: BaseAndP2<Int>.Type = derived
  let _: BaseAndP2<Int>.Type = derivedAndAnyObject

  // Erasing superclass constraint
  let _: P2.Type = baseIntAndP2
  let _: P2.Type = derived
  let _: P2.Type = derivedAndAnyObject
  let _: (P2 & AnyObject).Type = derived
  let _: (P2 & AnyObject).Type = derivedAndAnyObject
}

// There's a code path in CSApply that's hard to hit.
func takesBase<T>(_: Base<T>) {}
func takesBaseMetatype<T>(_: Base<T>.Type) {}

func takesBaseIntAndP2(x: Base<Int> & P2) {
  takesBase(x)
}

func takesBaseIntAndP2Metatype(x: (Base<Int> & P2).Type) {
  takesBaseMetatype(x)
}

//
// Conformance relation.
//

func conformsTo<T1 : P2, T2 : Base<Int> & P2>(
  anyObject: AnyObject,
  p1: P1,
  p2: P2,
  p3: P3,
  base: Base<Int>,
  badBase: Base<String>,
  derived: Derived,
  fakeDerived: FakeDerived,
  p2Archetype: T1,
  baseAndP2Archetype: T2) {

  // FIXME: Uninformative diagnostics

  // Errors
  conformsToAnyObject(p1)
  // expected-error@-1 {{cannot invoke 'conformsToAnyObject' with an argument list of type '(P1)'}}
  // expected-note@-2 {{expected an argument list of type '(T)'}}

  conformsToP1(p1)
  // expected-error@-1 {{cannot invoke 'conformsToP1' with an argument list of type '(P1)'}}
  // expected-note@-2 {{expected an argument list of type '(T)'}}

  conformsToBaseIntAndP2(base)
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  conformsToBaseIntAndP2(badBase)
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  conformsToBaseIntAndP2(fakeDerived)
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  conformsToBaseIntAndP2WithWhereClause(fakeDerived)
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  conformsToBaseIntAndP2(p2Archetype)
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  conformsToBaseIntAndP2WithWhereClause(p2Archetype)
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  // Good
  conformsToAnyObject(anyObject)
  conformsToAnyObject(baseAndP2Archetype)
  conformsToP1(derived)
  conformsToP1(baseAndP2Archetype)
  conformsToP2(derived)
  conformsToP2(baseAndP2Archetype)
  conformsToBaseIntAndP2(derived)
  conformsToBaseIntAndP2(baseAndP2Archetype)
  conformsToBaseIntAndP2WithWhereClause(derived)
  conformsToBaseIntAndP2WithWhereClause(baseAndP2Archetype)
}

//
// Protocols with superclass-constrained Self -- not supported yet.
//

protocol ProtoConstraintsSelfToClass where Self : Base<Int> {}

protocol ProtoRefinesClass : Base<Int> {} // FIXME expected-error {{}}
protocol ProtoRefinesClassAndProtocolAlias : BaseIntAndP2 {} // FIXME expected-error {{}}
protocol ProtoRefinesClassAndProtocolDirect : Base<Int> & P2 {} // FIXME expected-error 2 {{}}
protocol ProtoRefinesClassAndProtocolExpanded : Base<Int>, P2 {} // FIXME expected-error {{}}

class ClassConformsToClassProtocolBad1 : ProtoConstraintsSelfToClass {}
// expected-error@-1 {{'ProtoConstraintsSelfToClass' requires that 'ClassConformsToClassProtocolBad1' inherit from 'Base<Int>'}}
// expected-note@-2 {{requirement specified as 'Self' : 'Base<Int>' [with Self = ClassConformsToClassProtocolBad1]}}
class ClassConformsToClassProtocolGood1 : Derived, ProtoConstraintsSelfToClass {}

class ClassConformsToClassProtocolBad2 : ProtoRefinesClass {} // FIXME
class ClassConformsToClassProtocolGood2 : Derived, ProtoRefinesClass {}

// Subclass existentials inside inheritance clauses
class CompositionInClassInheritanceClauseAlias : BaseIntAndP2 {
  func protocolSelfReturn() -> Self { return self }
  func asBase() -> Base<Int> { return self }
  // FIXME expected-error@-1 {{}}
}

class CompositionInClassInheritanceClauseDirect : Base<Int> & P2 {
  // expected-error@-1 {{protocol-constrained type is neither allowed nor needed here}}
  func protocolSelfReturn() -> Self { return self }
  func asBase() -> Base<Int> { return self }
}

protocol CompositionInAssociatedTypeInheritanceClause {
  associatedtype A : BaseIntAndP2
  // FIXME expected-error@-1 {{}}
}
