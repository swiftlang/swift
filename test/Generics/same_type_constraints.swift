// RUN: %swift -parse %s -verify

protocol Fooable {
  typealias Foo

  var foo: Foo { get }
}

protocol Barrable {
  typealias Bar: Fooable
  var bar: Bar { get }
}

struct X {}
struct Y: Fooable {
  typealias Foo = X
  var foo: X { return X() }
}
struct Z: Barrable {
  typealias Bar = Y
  var bar: Y { return Y() }
}

protocol TestSameTypeRequirement {
  func foo<F1: Fooable where F1.Foo == X>(f: F1)
}
struct SatisfySameTypeRequirement : TestSameTypeRequirement {
  func foo<F2: Fooable where F2.Foo == X>(f: F2) {}
}

protocol TestSameTypeAssocTypeRequirement {
  typealias Assoc
  func foo<F1: Fooable where F1.Foo == Assoc>(f: F1)
}
struct SatisfySameTypeAssocTypeRequirement : TestSameTypeAssocTypeRequirement {
  typealias Assoc = X
  func foo<F2: Fooable where F2.Foo == X>(f: F2) {}
}
struct SatisfySameTypeAssocTypeRequirementDependent<T>
  : TestSameTypeAssocTypeRequirement
{
  typealias Assoc = T
  func foo<F3: Fooable where F3.Foo == T>(f: F3) {}
}
