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

struct GenericStruct<T> {
  typealias Alias = T
  typealias MetaAlias = T.Type

  func methodOne() -> Alias.Type {}
  func methodTwo() -> MetaAlias {}

  func methodOne() -> Alias.BadType {}
  // expected-error@-1 {{'BadType' is not a member type of 'GenericStruct.Alias'}}
  func methodTwo() -> MetaAlias.BadType {}
  // expected-error@-1 {{'BadType' is not a member type of 'GenericStruct.MetaAlias'}}

  var propertyOne: Alias.BadType
  // expected-error@-1 {{'BadType' is not a member type of 'T'}}
  var propertyTwo: MetaAlias.BadType
  // expected-error@-1 {{'BadType' is not a member type of 'T.Type'}}
}
