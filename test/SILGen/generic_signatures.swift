// RUN: %target-swift-frontend -emit-silgen -parse-stdlib %s

protocol P {
  associatedtype Assoc
}

protocol Q {
  associatedtype Assoc1
  associatedtype Assoc2
}

struct G<T> {}
class C {}

func a<T>(x: T) {}
func b<T: P>(x: G<T>, y: T.Assoc) {}
func c<T where T: P>(x: T, y: T.Assoc) {}
func d<T: P, U: P & Q>(x: T, y: U) {}
func e<T, U where T: P, U: P, U: Q>(x: T, y: U) {}
// FIXME: Same-type constraints expose a typechecker bug.
// <rdar://problem/15730168>
func f<T: Q where T.Assoc1 == T.Assoc2>(x: T) {}
func g<T where T: Q, T.Assoc1 == T.Assoc2>(x: T) {}
func h<T: P, U where T.Assoc == U>(x: T) {}
func i<T: P where T.Assoc: Q, T.Assoc.Assoc1 == T.Assoc.Assoc2>(x: T) {}
func j<T: C>(_: T) {}
func k<T where T: C>(_: T) {}
func l<T: C where T: P>(_: T) {}
func m<T: P where T.Assoc: C>(_: T) {}

struct Foo<V> {
  func z() {}

  func a<T>(x: T) {}
  func b<T: P>(x: G<T>, y: T.Assoc) {}
  func c<T where T: P>(x: T, y: T.Assoc) {}
  func d<T: P, U: P & Q>(x: T, y: U) {}
  func e<T, U where T: P, U: P, U: Q>(x: T, y: U) {}
  func f<T: Q where T.Assoc1 == T.Assoc2>(x: T) {}
  func g<T where T: Q, T.Assoc1 == T.Assoc2>(x: T) {}
  func h<T: P, U where T.Assoc == U>(x: T) {}
  func i<T: P where T.Assoc: Q, T.Assoc.Assoc1 == T.Assoc.Assoc2>(x: T) {}
  func j<T: C>(_: T) {}
  func k<T where T: C>(_: T) {}
  func l<T: C where T: P>(_: T) {}
  func m<T: P where T.Assoc: C>(_: T) {}
}

// Test that we handle interface type lowering when accessing a dependent
// member of a dependent member that substitutes to a type parameter.
// <rdar://problem/16257259>
protocol Fooable {
  associatedtype Foo
}

protocol Barrable {
  associatedtype Bar: Fooable

  func bar(_: Bar) -> Bar.Foo
}

struct FooBar<T: Fooable>: Barrable {
  typealias Bar = T

  func bar(_ x: T) -> T.Foo { }
}


// Test that associated types can be constrained to concrete types

func concreteJungle<T where T : Fooable, T.Foo == C>(_: T) -> T.Foo {
  return C()
}

func concreteJungle<T where T : Fooable, T.Foo == C>(_: T, t: T.Foo) -> C {
  let c: C = t
  return c
}

func concreteJungle<T where T : Fooable, T.Foo == C>(_: T, f: @escaping (T.Foo) -> C) -> T.Foo {
  let ff: (C) -> T.Foo = f
  return ff(C())
}
