// RUN: %target-swift-emit-silgen -enable-sil-ownership -parse-stdlib %s

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
func c<T>(x: T, y: T.Assoc) where T: P {}
func d<T: P, U: P & Q>(x: T, y: U) {}
func e<T, U>(x: T, y: U) where T: P, U: P, U: Q {}
// FIXME: Same-type constraints expose a typechecker bug.
// <rdar://problem/15730168>
func f<T: Q>(x: T) where T.Assoc1 == T.Assoc2 {}
func g<T>(x: T) where T: Q, T.Assoc1 == T.Assoc2 {}
func h<T: P, U>(x: T) where T.Assoc == U {}
func i<T: P>(x: T) where T.Assoc: Q, T.Assoc.Assoc1 == T.Assoc.Assoc2 {}
func j<T: C>(_: T) {}
func k<T>(_: T) where T: C {}
func l<T: C>(_: T) where T: P {}
func m<T: P>(_: T) where T.Assoc: C {}

struct Foo<V> {
  func z() {}

  func a<T>(x: T) {}
  func b<T: P>(x: G<T>, y: T.Assoc) {}
  func c<T>(x: T, y: T.Assoc) where T: P {}
  func d<T: P, U: P & Q>(x: T, y: U) {}
  func e<T, U>(x: T, y: U) where T: P, U: P, U: Q {}
  func f<T: Q>(x: T) where T.Assoc1 == T.Assoc2 {}
  func g<T>(x: T) where T: Q, T.Assoc1 == T.Assoc2 {}
  func h<T: P, U>(x: T) where T.Assoc == U {}
  func i<T: P>(x: T) where T.Assoc: Q, T.Assoc.Assoc1 == T.Assoc.Assoc2 {}
  func j<T: C>(_: T) {}
  func k<T>(_: T) where T: C {}
  func l<T: C>(_: T) where T: P {}
  func m<T: P>(_: T) where T.Assoc: C {}
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

func concreteJungle<T>(_: T) -> T.Foo where T : Fooable, T.Foo == C {
  return C()
}

func concreteJungle<T>(_: T, t: T.Foo) -> C where T : Fooable, T.Foo == C {
  let c: C = t
  return c
}

func concreteJungle<T>(_: T, f: @escaping (T.Foo) -> C) -> T.Foo where T : Fooable, T.Foo == C {
  let ff: (C) -> T.Foo = f
  return ff(C())
}
