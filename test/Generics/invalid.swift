// RUN: %target-typecheck-verify-swift

protocol he where A : B { // expected-error {{cannot find type 'A' in scope}}
  // expected-error@-1 {{cannot find type 'B' in scope}}

  associatedtype vav where A : B // expected-error{{cannot find type 'A' in scope}}
  // expected-error@-1 {{cannot find type 'B' in scope}}
}


struct Lunch<T> {
  struct Dinner<U> {

    var leftovers: T
    var transformation: (T) -> U
  }
}

class Deli<Spices> { // expected-note {{'Spices' declared as parameter to type 'Deli'}}
// expected-note@-1 {{arguments to generic parameter 'Spices' ('Pepper' and 'ChiliFlakes') are expected to be equal}}

  class Pepperoni {}
  struct Sausage {}
}

struct Pizzas<Spices> { // expected-note {{arguments to generic parameter 'Spices' ('ChiliFlakes' and 'Pepper') are expected to be equal}}
  class NewYork {
  }

  class DeepDish {
  }
}

class HotDog {
}

struct Pepper {}
struct ChiliFlakes {}

func eatDinnerConcrete(d: Pizzas<ChiliFlakes>.NewYork,
                       t: Deli<ChiliFlakes>.Pepperoni) {
}

func eatDinnerConcrete(d: Pizzas<Pepper>.DeepDish,
                       t: Deli<Pepper>.Pepperoni) {
}

func badDiagnostic1() {

  _ = Lunch<Pizzas<Pepper>.NewYork>.Dinner<HotDog>(
      leftovers: Pizzas<ChiliFlakes>.NewYork(),  // expected-error {{cannot convert parent type 'Pizzas<ChiliFlakes>' to expected type 'Pizzas<Pepper>'}}
      transformation: { _ in HotDog() })
}

func badDiagnostic2() {

  let firstCourse = Pizzas<ChiliFlakes>.NewYork()

  var dinner = Lunch<Pizzas<ChiliFlakes>.NewYork>.Dinner<HotDog>(
      leftovers: firstCourse,
      transformation: { _ in HotDog() })

  let topping = Deli<Pepper>.Pepperoni()

  eatDinnerConcrete(d: firstCourse, t: topping)
  // expected-error@-1 {{cannot convert parent type 'Deli<Pepper>' to expected type 'Deli<ChiliFlakes>'}}

}

// Real error is that we cannot infer the generic parameter from context

func takesAny(_ a: Any) {}

func badDiagnostic3() {
  takesAny(Deli.self) // expected-error {{generic parameter 'Spices' could not be inferred}}
  // expected-note@-1 {{explicitly specify the generic arguments to fix this issue}} {{16-16=<Any>}}
}

// Crash with missing nested type inside concrete type
class OuterGeneric<T> {
  class InnerGeneric<U> where U:OuterGeneric<T.NoSuchType> {
  // expected-error@-1 {{'NoSuchType' is not a member type of 'T'}}
    func method() {
      _ = method
    }
  }
}

// Crash with missing types in requirements.
protocol P1 {
  associatedtype A where A == ThisTypeDoesNotExist
  // expected-error@-1{{cannot find type 'ThisTypeDoesNotExist' in scope}}
  associatedtype B where ThisTypeDoesNotExist == B
  // expected-error@-1{{cannot find type 'ThisTypeDoesNotExist' in scope}}
  associatedtype C where ThisTypeDoesNotExist == ThisTypeDoesNotExist
  // expected-error@-1 2{{cannot find type 'ThisTypeDoesNotExist' in scope}}
}

// Diagnostic referred to the wrong type - <rdar://problem/33604221>

protocol E { associatedtype XYZ }

class P<N> {
  func q<A>(b:A) where A:E, N : A.XYZ { return }
  // expected-error@-1 {{type 'N' constrained to non-protocol, non-class type 'A.XYZ'}}
}

// SR-5579
protocol Foo {
    associatedtype Bar where Bar.Nonsense == Int // expected-error{{'Nonsense' is not a member type of 'Self.Bar'}}
}

protocol Wibble : Foo where Bar.EvenMoreNonsense == Int { } // expected-error{{'EvenMoreNonsense' is not a member type of 'Self.Bar'}}

// rdar://45271500 - failure to emit a diagnostic
enum Cat<A> {}
protocol Tail { associatedtype T }
struct Dog<B, C : Tail> where C.T == B {}
func foo<B, A>() -> Dog<B, Cat<A>> {}
// expected-error@-1 {{type 'Cat<A>' does not conform to protocol 'Tail'}}

// Tests for generic argument mismatch diagnosis
struct X<A> : Hashable {
  class Foo {}
  class Bar {}
}
// expected-note@-4 3 {{arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
// expected-note@-5 2 {{arguments to generic parameter 'A' ('Bool' and 'Int') are expected to be equal}}
// expected-note@-6 4 {{arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}

struct Y<A, B, C>{} // expected-note {{arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
// expected-note@-1 {{arguments to generic parameter 'C' ('Int' and 'Float') are expected to be equal}}

struct YieldValue {
  var property: X<Bool> {
    _read {
      yield X<Int>() // expected-error {{cannot convert value of type 'X<Int>' to expected yield type 'X<Bool>'}}
    }
  }
}

func multipleArguments(y: Y<Int, Int, Int>) {
  let _: Y<Bool, Int, Float> = y // expected-error {{cannot assign value of type 'Y<Int, Int, Int>' to type 'Y<Bool, Int, Float>'}}
}

func errorMessageVariants(x: X<Int>, x2: X<Bool> = X<Int>()) -> X<Bool> {
  // expected-error@-1 {{default argument value of type 'X<Int>' cannot be converted to type 'X<Bool>'}}
  let _: X<Bool> = x // expected-error {{cannot assign value of type 'X<Int>' to type 'X<Bool>'}}
  errorMessageVariants(x: x2, x2: x2) // expected-error {{cannot convert value of type 'X<Bool>' to expected argument type 'X<Int>'}}
  let _: X<Bool> = { return x }() // expected-error {{cannot convert value of type 'X<Int>' to closure result type 'X<Bool>'}}
  let _: [X<Bool>] = [x] // expected-error {{cannot convert value of type 'X<Int>' to expected element type 'X<Bool>'}}
  let _ = x as X<Bool> // expected-error {{cannot convert value of type 'X<Int>' to type 'X<Bool>' in coercion}}
  let _: X<Int>.Foo = X<Bool>.Foo() // expected-error {{cannot convert parent type 'X<Bool>' to expected type 'X<Int>'}}
  return x // expected-error {{cannot convert return expression of type 'X<Int>' to return type 'X<Bool>'}}
}
