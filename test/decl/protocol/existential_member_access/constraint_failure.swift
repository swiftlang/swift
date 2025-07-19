// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

class Class<T> {}

// A protocol member accessed with an existential value might have generic
// constraints that require the ability to spell an opened archetype in order
// to be satisfied. Such are
// - superclass requirements, when the object is a non-'Self'-rooted type
//   parameter, and the subject is dependent on 'Self', e.g. U : G<Self.A>
// - same-type requirements, when one side is dependent on 'Self', and the
//   other is a non-'Self'-rooted type parameter, e.g. U.Element == Self.
//
// Because opened archetypes are not part of the surface language, these
// constraints render the member inaccessible.
//
// Note: 'Self'-rooted type parameters that are invalid in the context of the
// existential base type are ignored -- the underlying requirement failure is
// considered a more pressing issue.
protocol UnfulfillableGenericRequirements {
  associatedtype A
}
extension UnfulfillableGenericRequirements {
  func method1() where A : Class<Self> {}
  func method2() where A: Sequence, A.Element == Self {}
  func method3<U>(_: U) -> U {}
  func method4<U>(_: U) where U : Class<Self.A> {}
  // expected-note@-1 3 {{where 'U' = 'Bool'}}
  func method5<U>(_: U) where U: Sequence, Self == U.Element {}
  // expected-note@-1 {{where 'U' = 'Bool'}}

  // expected-note@+1 2 {{where 'U' = 'Bool'}}
  func method6<U>(_: U) where U: UnfulfillableGenericRequirements,
                              A: Sequence, A.Element: Sequence,
                              U.A == A.Element.Element {}
  func method7<U>(_: U) where U: UnfulfillableGenericRequirements & Class<Self> {}

  func method8<U>(_: U) where U == Self.A {}
}
do {
  let exist: any UnfulfillableGenericRequirements

  exist.method1() // expected-error {{instance method 'method1()' requires that 'Self.A' inherit from 'Class<Self>'}}
  exist.method2()
  // expected-error@-1 {{instance method 'method2()' requires the types 'Self' and 'Self.A.Element' be equivalent}}
  // expected-error@-2 {{instance method 'method2()' requires that 'Self.A' conform to 'Sequence'}}
  _ = exist.method3(false) // ok
  exist.method4(false)
  // expected-error@-1 {{instance method 'method4' requires that 'Bool' inherit from 'Class<Self.A>'}}
  // expected-error@-2 {{member 'method4' cannot be used on value of type 'any UnfulfillableGenericRequirements'; consider using a generic constraint instead}}
  // expected-note@-3 {{member 'method4' refrences 'Self.A', which cannot be resolved on type 'any UnfulfillableGenericRequirements'}}
  exist.method5(false)
  // expected-error@-1 {{instance method 'method5' requires that 'Bool' conform to 'Sequence'}}
  // expected-error@-2 {{member 'method5' cannot be used on value of type 'any UnfulfillableGenericRequirements'; consider using a generic constraint instead}}
  // expected-note@-3 {{member 'method5' refrences 'Self', which cannot be resolved on type 'any UnfulfillableGenericRequirements'}}

  exist.method7(false)
  // expected-error@-1 {{instance method 'method7' requires that 'U' conform to 'UnfulfillableGenericRequirements'}}
  // expected-error@-2 {{member 'method7' cannot be used on value of type 'any UnfulfillableGenericRequirements'; consider using a generic constraint instead}}
  // expected-note@-3 {{member 'method7' refrences 'Self', which cannot be resolved on type 'any UnfulfillableGenericRequirements'}}

  exist.method8(false)
  // expected-error@-1 {{member 'method8' cannot be used on value of type 'any UnfulfillableGenericRequirements'; consider using a generic constraint instead}}
  // expected-note@-2 {{member 'method8' refrences 'Self.A', which cannot be resolved on type 'any UnfulfillableGenericRequirements'}}
}

// Make sure this also works in a generic context!
struct G<X, Y, Z> {
  func doIt() {
    let exist: any UnfulfillableGenericRequirements

    exist.method8(false)
    // expected-error@-1 {{member 'method8' cannot be used on value of type 'any UnfulfillableGenericRequirements'; consider using a generic constraint instead}}
    // expected-note@-2 {{member 'method8' refrences 'Self.A', which cannot be resolved on type 'any UnfulfillableGenericRequirements'}}
  }
}
protocol UnfulfillableGenericRequirementsDerived1: UnfulfillableGenericRequirements where A == Bool {}
protocol UnfulfillableGenericRequirementsDerived2: UnfulfillableGenericRequirements where A == Class<Self> {}
do {
  // Test that 'Self' dependencies are computed relative to the base type.
  let exist1: any UnfulfillableGenericRequirementsDerived1
  let exist2: any UnfulfillableGenericRequirementsDerived2

  exist1.method4(false)
  // expected-error@-1 {{instance method 'method4' requires that 'Bool' inherit from 'Class<Self.A>}}
  exist2.method4(false)
  // expected-error@-1 {{member 'method4' cannot be used on value of type 'any UnfulfillableGenericRequirementsDerived2'; consider using a generic constraint instead}}
  // expected-error@-2 {{instance method 'method4' requires that 'Bool' inherit from 'Class<Self.A>'}}
  // expected-note@-3 {{member 'method4' refrences 'Self.A', which cannot be resolved on type 'any UnfulfillableGenericRequirementsDerived2'}}
}
protocol UnfulfillableGenericRequirementsDerived3: UnfulfillableGenericRequirements where A: Sequence, A.Element: Sequence {}
do {
  // Test that 'Self'-rooted type parameters that are invalid in the context of
  // the existential base type are ignored.
  let exist1: any UnfulfillableGenericRequirements
  let exist2: any UnfulfillableGenericRequirementsDerived3

  exist1.method6(false)
  // expected-error@-1 {{instance method 'method6' requires that 'Self.A.Element' conform to 'Sequence'}}
  // expected-error@-2 {{instance method 'method6' requires that 'Self.A' conform to 'Sequence'}}
  // expected-error@-3 {{instance method 'method6' requires that 'Bool' conform to 'UnfulfillableGenericRequirements'}}
  exist2.method6(false)
  // expected-error@-1 {{member 'method6' cannot be used on value of type 'any UnfulfillableGenericRequirementsDerived3'; consider using a generic constraint instead}}
  // expected-error@-2 {{instance method 'method6' requires that 'Bool' conform to 'UnfulfillableGenericRequirements'}}
  // expected-note@-3 {{member 'method6' refrences 'Self.A', which cannot be resolved on type 'any UnfulfillableGenericRequirementsDerived3'}}
}

// Test that we don't determine existential availability based on type
// parameters that are invalid in the context of the existential base type --
// the requirement failure is a more pressing issue.
protocol InvalidTypeParameters {
  associatedtype A
}
extension InvalidTypeParameters {
  func method1() -> A.A where A: InvalidTypeParameters {}
  func method2(_: A.A) where A: InvalidTypeParameters {}
  func method3(_: A.A, _: A) where A: InvalidTypeParameters {}
}
do {
  let exist: any InvalidTypeParameters

  exist.method1() // expected-error {{instance method 'method1()' requires that 'Self.A' conform to 'InvalidTypeParameters'}}
  exist.method2(false) // expected-error {{instance method 'method2' requires that 'Self.A' conform to 'InvalidTypeParameters'}}
  exist.method3(false, false) // expected-error {{instance method 'method3' requires that 'Self.A' conform to 'InvalidTypeParameters'}}
  // expected-error@-1 {{member 'method3' cannot be used on value of type 'any InvalidTypeParameters'; consider using a generic constraint instead}}
  // expected-note@-2 {{member 'method3' refrences 'Self.A', which cannot be resolved on type 'any InvalidTypeParameters'}}
}

protocol GenericRequirementFailures {
  associatedtype A
}
extension GenericRequirementFailures where A == Never {
  func method1() {}
  func method2() -> Self {}
  func method3(_: A) {}
}
extension GenericRequirementFailures where A: GenericRequirementFailures {
  func method4() {}
}
do {
  let exist: any GenericRequirementFailures

  exist.method1() // expected-error {{referencing instance method 'method1()' on 'GenericRequirementFailures' requires the types 'Self.A' and 'Never' be equivalent}}
  exist.method2() // expected-error {{referencing instance method 'method2()' on 'GenericRequirementFailures' requires the types 'Self.A' and 'Never' be equivalent}}
  exist.method3(false) // expected-error {{referencing instance method 'method3' on 'GenericRequirementFailures' requires the types 'Self.A' and 'Never' be equivalent}}
  // expected-error@-1 {{member 'method3' cannot be used on value of type 'any GenericRequirementFailures'; consider using a generic constraint instead}}
  // expected-note@-2 {{member 'method3' refrences 'Self.A', which cannot be resolved on type 'any GenericRequirementFailures'}}
  exist.method4() // expected-error {{referencing instance method 'method4()' on 'GenericRequirementFailures' requires that 'Self.A' conform to 'GenericRequirementFailures'}}
}
protocol GenericRequirementFailuresDerived: GenericRequirementFailures where A: GenericRequirementFailures {}
do {
  let exist: any GenericRequirementFailuresDerived
  exist.method4() // ok
}
