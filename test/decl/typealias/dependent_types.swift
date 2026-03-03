// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype Assoc = Self
}

struct X : P {
}

class Y<T: P> {
  typealias Assoc = T.Assoc
}

func f<T: P>(_ x: T, y: Y<T>.Assoc) {
}

protocol P1 {
  associatedtype A = Int
}

struct X1<T> : P1 {
  init(_: X1.A) {
  }
}

struct GenericStruct<T> { // expected-note 3{{generic struct 'GenericStruct' declared here}}
  typealias Alias = T
  typealias MetaAlias = T.Type

  typealias Concrete = Int
  typealias ReferencesConcrete = Concrete

  func methodOne() -> Alias.Type {}
  func methodTwo() -> MetaAlias {}

  func methodOne() -> Alias.BadType {}
  // expected-error@-1 {{'BadType' is not a member type of type 'dependent_types.GenericStruct<T>.Alias'}}
  func methodTwo() -> MetaAlias.BadType {}
  // expected-error@-1 {{'BadType' is not a member type of type 'dependent_types.GenericStruct<T>.MetaAlias'}}

  var propertyOne: Alias.BadType
  // expected-error@-1 {{'BadType' is not a member type of type 'dependent_types.GenericStruct<T>.Alias' (aka 'T')}}
  var propertyTwo: MetaAlias.BadType
  // expected-error@-1 {{'BadType' is not a member type of type 'dependent_types.GenericStruct<T>.MetaAlias' (aka 'T.Type')}}
}

// This was accepted in Swift 3.0 and sort of worked... but we can't
// implement it correctly. In Swift 3.1 it triggered an assert.
// Make sure it's banned now with a proper diagnostic.

func foo() -> Int {}
func metaFoo() -> Int.Type {}

let _: GenericStruct.Alias = foo()
// expected-error@-1 {{reference to generic type 'GenericStruct' requires arguments in <...>}}
let _: GenericStruct.MetaAlias = metaFoo()
// expected-error@-1 {{reference to generic type 'GenericStruct' requires arguments in <...>}}

// ... but if the typealias has a fully concrete underlying type,
// we are OK.
let _: GenericStruct.Concrete = foo()

let _: GenericStruct.ReferencesConcrete = foo()
// expected-error@-1 {{reference to generic type 'GenericStruct' requires arguments in <...>}}

class SuperG<T, U> {
  typealias Composed = (T, U)
  typealias Concrete = Int
}

class SubG<T> : SuperG<T, T> { }

typealias SubGX<T> = SubG<T?>

func checkSugar(gs: SubGX<Int>.Composed) {
  let i4: Int = gs // expected-error{{cannot convert value of type 'SubGX<Int>.Composed' (aka '(Optional<Int>, Optional<Int>)') to specified type 'Int'}}
}

// https://github.com/swiftlang/swift/issues/82160

let x1: SuperG.Concrete = 123
let x2: SubG.Concrete = 123

func f1() -> SuperG.Concrete {
  return 123
}

func f2() -> SubG.Concrete {
  return 123
}