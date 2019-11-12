// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

protocol Fooable {
  associatedtype Foo // expected-note{{protocol requires nested type 'Foo'; do you want to add it?}}

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
	// expected-warning@-1{{redundant same-type constraint 'U.Foo' == 'X'}}
	// expected-note@-2{{same-type constraint 'T.Foo' == 'X' written here}}
  return (t.foo, u.foo)
}

func fail1<
  T: Fooable, U: Fooable
>(_ t: T, u: U) -> (X, Y)
  where T.Foo == X, U.Foo == Y, T.Foo == U.Foo { // expected-error{{'U.Foo' cannot be equal to both 'Y' and 'X'}}
  // expected-note@-1{{same-type constraint 'T.Foo' == 'X' written here}}
  return (t.foo, u.foo) // expected-error{{cannot convert return expression of type '(X, X)' to return type '(X, Y)'}}
}

func fail2<
  T: Fooable, U: Fooable
>(_ t: T, u: U) -> (X, Y)
  where T.Foo == U.Foo, T.Foo == X, U.Foo == Y { // expected-error{{'U.Foo' cannot be equal to both 'Y' and 'X'}}
  // expected-note@-1{{same-type constraint 'T.Foo' == 'X' written here}}
  return (t.foo, u.foo) // expected-error{{cannot convert return expression of type '(X, X)' to return type '(X, Y)'}}
}

func test4<T: Barrable>(_ t: T) -> Y where T.Bar == Y {
  return t.bar
}

func fail3<T: Barrable>(_ t: T) -> X
  where T.Bar == X { // expected-error {{'X' does not conform to required protocol 'Fooable'}}
  return t.bar // expected-error{{cannot convert return expression of type 'T.Bar' }}
}

func test5<T: Barrable>(_ t: T) -> X where T.Bar.Foo == X {
  return t.bar.foo
}

func test6<T: Barrable>(_ t: T) -> (Y, X) where T.Bar == Y {
  return (t.bar, t.bar.foo)
}

func test7<T: Barrable>(_ t: T) -> (Y, X) where T.Bar == Y, T.Bar.Foo == X {
	// expected-warning@-1{{redundant same-type constraint 'T.Bar.Foo' == 'X'}}
        // expected-note@-2{{same-type constraint 'T.Bar.Foo' == 'Y.Foo' (aka 'X') implied here}}
  return (t.bar, t.bar.foo)
}

func fail4<T: Barrable>(_ t: T) -> (Y, Z)
  where
  T.Bar == Y, // expected-note{{same-type constraint 'T.Bar.Foo' == 'Y.Foo' (aka 'X') implied here}}
  T.Bar.Foo == Z { // expected-error{{'T.Bar.Foo' cannot be equal to both 'Z' and 'Y.Foo' (aka 'X')}}
  return (t.bar, t.bar.foo) // expected-error{{cannot convert return expression of type '(Y, X)' to return type '(Y, Z)'}}
}

func fail5<T: Barrable>(_ t: T) -> (Y, Z)
  where
  T.Bar.Foo == Z, // expected-note{{same-type constraint 'T.Bar.Foo' == 'Z' written here}}
  T.Bar == Y { // expected-error{{'T.Bar.Foo' cannot be equal to both 'Y.Foo' (aka 'X') and 'Z'}}
  return (t.bar, t.bar.foo) // expected-error{{cannot convert return expression of type '(Y, X)' to return type '(Y, Z)'}}
}

func test8<T: Fooable>(_ t: T)
  where T.Foo == X, // expected-note{{same-type constraint 'T.Foo' == 'X' written here}}
  T.Foo == Y {} // expected-error{{'T.Foo' cannot be equal to both 'Y' and 'X'}}


func testAssocTypeEquivalence<T: Fooable>(_ fooable: T) -> X.Type
  where T.Foo == X {
  return T.Foo.self
}

func fail6<T>(_ t: T) -> Int where T == Int { // expected-error{{same-type requirement makes generic parameter 'T' non-generic}}
  return t
}

func test8<T: Barrable, U: Barrable>(_ t: T, u: U) -> (Y, Y, X, X)
  where T.Bar == Y, // expected-note{{same-type constraint 'U.Bar.Foo' == 'Y.Foo' (aka 'X') implied here}}
        U.Bar.Foo == X, T.Bar == U.Bar { // expected-warning{{redundant same-type constraint 'U.Bar.Foo' == 'X'}}
  return (t.bar, u.bar, t.bar.foo, u.bar.foo)
}

func test8a<T: Barrable, U: Barrable>(_ t: T, u: U) -> (Y, Y, X, X)
  where
  T.Bar == Y, // expected-note{{same-type constraint 'U.Bar.Foo' == 'Y.Foo' (aka 'X') implied here}}
  U.Bar.Foo == X, U.Bar == T.Bar { // expected-warning{{redundant same-type constraint 'U.Bar.Foo' == 'X'}}
  return (t.bar, u.bar, t.bar.foo, u.bar.foo)
}

func test8b<T: Barrable, U: Barrable>(_ t: T, u: U)
  where U.Bar.Foo == X, // expected-warning{{redundant same-type constraint 'U.Bar.Foo' == 'X'}}
        T.Bar == Y, // expected-note{{same-type constraint 'U.Bar.Foo' == 'Y.Foo' (aka 'X') implied here}}
        T.Bar == U.Bar {
}

// rdar://problem/19137463
func rdar19137463<T>(_ t: T) where T.a == T {} // expected-error{{'a' is not a member type of 'T'}}
rdar19137463(1)


struct Brunch<U : Fooable> where U.Foo == X {}

struct BadFooable : Fooable { // expected-error{{type 'BadFooable' does not conform to protocol 'Fooable'}}
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
  func foo<X, Y>(x: X, y: Y) where X == T.A, Y == T.B {  // expected-error{{same-type requirement makes generic parameters 'X' and 'Y' equivalent}}
    print(X.self)
    print(Y.self)
    print(x)
    print(y)
  }
}
S2<Q>().foo(x: 1, y: 2)

struct S3<T : P> {
  func foo<X, Y>(x: X, y: Y) where X == T.A, Y == T.A {} // expected-error{{same-type requirement makes generic parameters 'X' and 'Y' equivalent}}
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
// expected-error@-2{{same-type requirement makes generic parameters 'U' and 'W' equivalent}}

typealias Tuple2<T, U> = (T, U)

func structuralSameType2<A: P1, B: P1, T, U, V, W>(_: A, _: B, _: T, _: U, _: V, _: W)
  where A.Assoc == Tuple2<T, U>, B.Assoc == Tuple2<V, W>, A.Assoc == B.Assoc { }
// expected-error@-1{{same-type requirement makes generic parameters 'T' and 'V' equivalent}}
// expected-error@-2{{same-type requirement makes generic parameters 'U' and 'W' equivalent}}

func structuralSameType3<T, U, V, W>(_: T, _: U, _: V, _: W)
  where X1<T, U> == X1<V, W> { }
// expected-error@-1{{same-type requirement makes generic parameters 'T' and 'V' equivalent}}
// expected-error@-2{{same-type requirement makes generic parameters 'U' and 'W' equivalent}}
// expected-warning@-3{{neither type in same-type constraint ('X1<T, U>' or 'X1<V, W>') refers to a generic parameter or associated type}}

protocol P2 {
  associatedtype Assoc1
  associatedtype Assoc2
}

func structuralSameTypeRecursive1<T: P2, U>(_: T, _: U)
  where T.Assoc1 == Tuple2<T.Assoc1, U> // expected-error{{same-type constraint 'T.Assoc1' == '(T.Assoc1, U)' is recursive}}
{ }

protocol P3 {
}

protocol P4 {
  associatedtype A
}

func test9<T>(_: T) where T.A == X, T: P4, T.A: P3 { } // expected-error{{same-type constraint type 'X' does not conform to required protocol 'P3'}}

// Same-type constraint conflict through protocol where clauses.
protocol P5 where Foo1 == Foo2 {
  associatedtype Foo1
  associatedtype Foo2
}

protocol P6 {
  associatedtype Bar: P5
}

struct X5a {}

struct X5b { }

func test9<T: P6, U: P6>(_ t: T, u: U)
  where T.Bar.Foo1 == X5a, // expected-note{{same-type constraint 'T.Bar.Foo1' == 'X5a' written here}}
        U.Bar.Foo2 == X5b, // expected-error{{'U.Bar.Foo2' cannot be equal to both 'X5b' and 'X5a'}}
        T.Bar == U.Bar {
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected error produced: generic parameter τ_0_0.Bar.Foo cannot be equal to both 'Y.Foo' (aka 'X') and 'Z'

func testMetatypeSameType<T, U>(_ t: T, _ u: U)
  where T.Type == U.Type { }
// expected-error@-1{{same-type requirement makes generic parameters 'T' and 'U' equivalent}}
// expected-warning@-2{{neither type in same-type constraint ('T.Type' or 'U.Type') refers to a generic parameter or associated type}}

func testSameTypeCommutativity1<U, T>(_ t: T, _ u: U)
  where T.Type == U { } // Equivalent to U == T.Type
// expected-error@-1{{same-type requirement makes generic parameter 'U' non-generic}}

func testSameTypeCommutativity2<U, T: P1>(_ t: T, _ u: U)
  where U? == T.Assoc { } // Ok, equivalent to T.Assoc == U?

func testSameTypeCommutativity3<U, T: P1>(_ t: T, _ u: U)
  where (U) -> () == T.Assoc { } // Ok, equivalent to T.Assoc == (U) -> ()

func testSameTypeCommutativity4<U, T>(_ t: T, _ u: U)
  where (U) -> () == T { } // Equivalent to T == (U) -> ()
// expected-error@-1{{same-type requirement makes generic parameter 'T' non-generic}}

func testSameTypeCommutativity5<U, T: P1>(_ t: T, _ u: U)
  where PPP & P3 == T.Assoc { } // Ok, equivalent to T.Assoc == PPP & P3

// FIXME: Error emitted twice.
func testSameTypeCommutativity6<U, T: P1>(_ t: T, _ u: U)
  where U & P3 == T.Assoc { } // Equivalent to T.Assoc == U & P3
// expected-error@-1 2 {{non-protocol, non-class type 'U' cannot be used within a protocol-constrained type}}

// rdar;//problem/46848889
struct Foo<A: P1, B: P1, C: P1> where A.Assoc == B.Assoc, A.Assoc == C.Assoc {}

struct Bar<A: P1, B: P1> where A.Assoc == B.Assoc {
  func f<C: P1>(with other: C) -> Foo<A, B, C> where A.Assoc == C.Assoc {
    // expected-note@-1 {{previous same-type constraint 'B.Assoc' == 'C.Assoc' inferred from type here}}
    // expected-warning@-2 {{redundant same-type constraint 'A.Assoc' == 'C.Assoc'}}
    fatalError()
  }
}

protocol P7 {
  associatedtype A
  static func fn(args: A)
}

class R<T>: P7 where T: P7, T.A == T.Type { // expected-note {{'T' declared as parameter to type 'R'}}
  typealias A = T.Type
  static func fn(args: T.Type) {}
}

R.fn(args: R.self) // expected-error {{generic parameter 'T' could not be inferred}}
// expected-note@-1 {{explicitly specify the generic arguments to fix this issue}}
