// RUN: %swift -parse -verify %s

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

func test1<T: Fooable where T.Foo == X>(fooable: T) -> X {
  return fooable.foo
}

struct NestedConstraint<T> {
  func tFoo<U: Fooable where U.Foo == T>(fooable: U) -> T {
    // TODO: The type-checker refuses to consider U.Foo == T here...
    return fooable.foo // expected-error{{does not type-check}}
  }
}

func test2<
  T: Fooable, U: Fooable
  where
  T.Foo == X, U.Foo == T.Foo
>(t: T, u: U) -> (X, X) {
  return (t.foo, u.foo)
}

func test2a<
  T: Fooable, U: Fooable
  where
  T.Foo == X, T.Foo == U.Foo
>(t: T, u: U) -> (X, X) {
  return (t.foo, u.foo)
}

func test3<
  T: Fooable, U: Fooable
  where
  T.Foo == X, U.Foo == X, T.Foo == U.Foo
>(t: T, u: U) -> (X, X) {
  return (t.foo, u.foo)
}

func fail1<
  T: Fooable, U: Fooable
  where
  T.Foo == X, U.Foo == Y, T.Foo == U.Foo // expected-error{{generic parameter Foo cannot be equal to both 'X' and 'Y'}}
>(t: T, u: U) -> (X, Y) {
  return (t.foo, u.foo)
}

func fail2<
  T: Fooable, U: Fooable
  where
  T.Foo == U.Foo, T.Foo == X, U.Foo == Y // expected-error{{generic parameter Foo cannot be equal to both 'X' and 'Y'}}
>(t: T, u: U) -> (X, Y) {
  return (t.foo, u.foo) // expected-error{{does not type-check}}
}

func test4<T: Barrable where T.Bar == Y>(t: T) -> Y {
  return t.bar
}

func fail3<
  T: Barrable
  where
  T.Bar == X // expected-error{{'X' does not conform to required protocol 'Fooable'}}
>(t: T) -> X {
  return t.bar // expected-error{{does not type-check}}
}

func test5<T: Barrable where T.Bar.Foo == X>(t: T) -> X {
  return t.bar.foo
}

func test6<T: Barrable where T.Bar == Y>(t: T) -> (Y, X) {
  return (t.bar, t.bar.foo)
}

func test7<T: Barrable where T.Bar == Y, T.Bar.Foo == X>(t: T) -> (Y, X) {
  return (t.bar, t.bar.foo)
}

func fail4<
  T: Barrable
  where
  T.Bar == Y,
  T.Bar.Foo == Z // expected-error{{generic parameter Foo cannot be equal to both 'X' and 'Z'}}
>(t: T) -> (Y, Z) {
  return (t.bar, t.bar.foo) // expected-error{{does not type-check}}
}

// TODO: repeat diagnostic
func fail5<
  T: Barrable
  where
  T.Bar.Foo == Z,
  T.Bar == Y // expected-error{{generic parameter Foo cannot be equal to both 'Z' and 'X'}} expected-error{{generic parameter Foo cannot be equal to both 'Z' and 'X'}}
>(t: T) -> (Y, Z) {
  return (t.bar, t.bar.foo) // expected-error{{does not type-check}}
}

func test8<T: Fooable where T.Foo == X, T.Foo == Y>(t: T) {} // expected-error{{generic parameter Foo cannot be equal to both 'X' and 'Y'}}

func testAssocTypeEquivalence<
  T: Fooable where T.Foo == X
>(fooable: T) -> X.Type {
  return T.Foo.self
}

func fail6<T where T == Int>(t: T) -> Int { // expected-error{{same-type requirement makes generic parameter 'T' non-generic}}
  return t // expected-error{{'T' is not convertible to 'Int'}}
}

func test8<
  T: Barrable, U: Barrable
  where
  T.Bar == Y, U.Bar.Foo == X, T.Bar == U.Bar
>(t: T, u: U) -> (Y, Y, X, X) {
  return (t.bar, u.bar, t.bar.foo, u.bar.foo)
}

func test8a<
  T: Barrable, U: Barrable
  where
  T.Bar == Y, U.Bar.Foo == X, U.Bar == T.Bar
>(t: T, u: U) -> (Y, Y, X, X) {
  return (t.bar, u.bar, t.bar.foo, u.bar.foo)
}

