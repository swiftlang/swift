// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

protocol Fooable {
  associatedtype Foo

  var foo: Foo { get }
}

protocol Barrable {
  associatedtype Bar: Fooable
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
  func foo<F1: Fooable>(_ f: F1) where F1.Foo == X
}
struct SatisfySameTypeRequirement : TestSameTypeRequirement {
  func foo<F2: Fooable>(_ f: F2) where F2.Foo == X {}
}

func test1<T: Fooable>(_ fooable: T) -> X where T.Foo == X {
  return fooable.foo
}

struct NestedConstraint<T> {
  func tFoo<U: Fooable>(_ fooable: U) -> T where U.Foo == T {
    return fooable.foo
  }
}

func test2<T: Fooable, U: Fooable>(_ t: T, u: U) -> (X, X)
  where T.Foo == X, U.Foo == T.Foo {
  return (t.foo, u.foo)
}

func test2a<T: Fooable, U: Fooable>(_ t: T, u: U) -> (X, X)
  where T.Foo == X, T.Foo == U.Foo {
  return (t.foo, u.foo)
}

func test3<T: Fooable, U: Fooable>(_ t: T, u: U) -> (X, X)
  where T.Foo == X, U.Foo == X, T.Foo == U.Foo {
  return (t.foo, u.foo)
}

func fail1<
  T: Fooable, U: Fooable
>(_ t: T, u: U) -> (X, Y)
  where T.Foo == X, U.Foo == Y, T.Foo == U.Foo { // expected-error{{generic parameter 'T.Foo' cannot be equal to both 'X' and 'Y'}}
  return (t.foo, u.foo)
}

func fail2<
  T: Fooable, U: Fooable
>(_ t: T, u: U) -> (X, Y)
  where T.Foo == U.Foo, T.Foo == X, U.Foo == Y { // expected-error{{generic parameter 'T.Foo' cannot be equal to both 'X' and 'Y'}}
  return (t.foo, u.foo) // expected-error{{cannot convert return expression of type 'X' to return type 'Y'}}
}

func test4<T: Barrable>(_ t: T) -> Y where T.Bar == Y {
  return t.bar
}

func fail3<T: Barrable>(_ t: T) -> X
  where T.Bar == X { // expected-error {{'X' does not conform to required protocol 'Fooable'}}
  return t.bar // expected-error{{cannot convert return expression of type 'T.Bar' to return type 'X'}}
}

func test5<T: Barrable>(_ t: T) -> X where T.Bar.Foo == X {
  return t.bar.foo
}

func test6<T: Barrable>(_ t: T) -> (Y, X) where T.Bar == Y {
  return (t.bar, t.bar.foo)
}

func test7<T: Barrable>(_ t: T) -> (Y, X) where T.Bar == Y, T.Bar.Foo == X {
  return (t.bar, t.bar.foo)
}

func fail4<T: Barrable>(_ t: T) -> (Y, Z)
  where
  T.Bar == Y,
  T.Bar.Foo == Z { // expected-error{{generic parameter 'T.Bar.Foo' cannot be equal to both 'X' and 'Z'}}
  return (t.bar, t.bar.foo) // expected-error{{cannot convert return expression of type 'X' to return type 'Z'}}
}

// TODO: repeat diagnostic
func fail5<T: Barrable>(_ t: T) -> (Y, Z)
  where
  T.Bar.Foo == Z,
  T.Bar == Y { // expected-error 2{{generic parameter 'T.Bar.Foo' cannot be equal to both 'Z' and 'X'}}
  return (t.bar, t.bar.foo) // expected-error{{cannot convert return expression of type 'X' to return type 'Z'}}
}

func test8<T: Fooable>(_ t: T) where T.Foo == X, T.Foo == Y {} // expected-error{{generic parameter 'T.Foo' cannot be equal to both 'X' and 'Y'}}

func testAssocTypeEquivalence<T: Fooable>(_ fooable: T) -> X.Type
  where T.Foo == X {
  return T.Foo.self
}

func fail6<T>(_ t: T) -> Int where T == Int { // expected-error 2{{same-type requirement makes generic parameter 'T' non-generic}}
  return t
}

func test8<T: Barrable, U: Barrable>(_ t: T, u: U) -> (Y, Y, X, X)
  where T.Bar == Y, U.Bar.Foo == X, T.Bar == U.Bar {
  return (t.bar, u.bar, t.bar.foo, u.bar.foo)
}

func test8a<T: Barrable, U: Barrable>(_ t: T, u: U) -> (Y, Y, X, X)
  where
  T.Bar == Y, U.Bar.Foo == X, U.Bar == T.Bar {
  return (t.bar, u.bar, t.bar.foo, u.bar.foo)
}

// rdar://problem/19137463
func rdar19137463<T>(_ t: T) where T.a == T {} // expected-error{{'a' is not a member type of 'T'}}
rdar19137463(1)


struct Brunch<U : Fooable> where U.Foo == X {}

struct BadFooable : Fooable {
  typealias Foo = DoesNotExist // expected-error{{use of undeclared type 'DoesNotExist'}}
  var foo: Foo { while true {} }
}

func bogusInOutError(d: inout Brunch<BadFooable>) {}

// Some interesting invalid cases that used to crash
protocol P {
  associatedtype A
  associatedtype B
}

struct Q : P {
  typealias A = Int
  typealias B = Int
}

struct S1<T : P> {
  func foo<X, Y>(x: X, y: Y) where X == T.A, Y == T.B {
    print(X.self)
    print(Y.self)
    print(x)
    print(y)
  }
}
S1<Q>().foo(x: 1, y: 2)

struct S2<T : P> where T.A == T.B {
  func foo<X, Y>(x: X, y: Y) where X == T.A, Y == T.B {  // expected-error 2{{same-type requirement makes generic parameters 'X' and 'Y' equivalent}}
    print(X.self)
    print(Y.self)
    print(x)
    print(y)
  }
}
S2<Q>().foo(x: 1, y: 2)

struct S3<T : P> {
  func foo<X, Y>(x: X, y: Y) where X == T.A, Y == T.A {} // expected-error 2{{same-type requirement makes generic parameters 'X' and 'Y' equivalent}}
}
S3<Q>().foo(x: 1, y: 2)

// Secondaries can be equated OK, even if we're imposing
// new conformances onto an outer secondary

protocol PPP {}

protocol PP {
  associatedtype A : PPP
}

struct SSS : PPP {}
struct SS : PP { typealias A = SSS }

struct QQ : P {
  typealias A = SSS
  typealias B = Int
}

struct S4<T : P> {
  func foo<X : PP>(x: X) where X.A == T.A {
    print(x)
    print(X.self)
  }
}

S4<QQ>().foo(x: SS())

// rdar://problem/29333056 - allow deeper matching of same-type constraints.
struct X1<A, B> { }

protocol P1 {
  associatedtype Assoc
}

func structuralSameType1<A: P1, B: P1, T, U, V, W>(_: A, _: B, _: T, _: U, _: V, _: W)
  where A.Assoc == X1<T, U>, B.Assoc == X1<V, W>, A.Assoc == B.Assoc { }
// expected-error@-1{{same-type requirement makes generic parameters 'T' and 'V' equivalent}}
// expected-error@-2{{same-type requirement makes generic parameters 'T' and 'V' equivalent}}
// expected-error@-3{{same-type requirement makes generic parameters 'U' and 'W' equivalent}}
// expected-error@-4{{same-type requirement makes generic parameters 'U' and 'W' equivalent}}

typealias Tuple2<T, U> = (T, U)

func structuralSameType2<A: P1, B: P1, T, U, V, W>(_: A, _: B, _: T, _: U, _: V, _: W)
  where A.Assoc == Tuple2<T, U>, B.Assoc == Tuple2<V, W>, A.Assoc == B.Assoc { }
// expected-error@-1{{same-type requirement makes generic parameters 'T' and 'V' equivalent}}
// expected-error@-2{{same-type requirement makes generic parameters 'T' and 'V' equivalent}}
// expected-error@-3{{same-type requirement makes generic parameters 'U' and 'W' equivalent}}
// expected-error@-4{{same-type requirement makes generic parameters 'U' and 'W' equivalent}}


protocol P2 {
  associatedtype Assoc1
  associatedtype Assoc2
}

func structuralSameTypeRecursive1<T: P2, U>(_: T, _: U)
  where T.Assoc1 == Tuple2<T.Assoc1, U> // expected-error 2{{same-type constraint 'T.Assoc1' == '(T.Assoc1, U)' is recursive}}
{ }


// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected error produced: generic parameter τ_0_0.Bar.Foo cannot be equal to both 'Y.Foo' (aka 'X') and 'Z'
