// RUN: %target-typecheck-verify-swift

func bet() where A : B {} // expected-error {{'where' clause cannot be attached to a non-generic declaration}}

typealias gimel where A : B // expected-error {{'where' clause cannot be attached to a non-generic declaration}}
// expected-error@-1 {{expected '=' in type alias declaration}}

class dalet where A : B {} // expected-error {{'where' clause cannot be attached to a non-generic declaration}}

protocol he where A : B { // expected-error {{use of undeclared type 'A'}}
  // expected-error@-1 {{use of undeclared type 'B'}}

  associatedtype vav where A : B // expected-error{{use of undeclared type 'A'}}
  // expected-error@-1 {{use of undeclared type 'B'}}
}


struct Lunch<T> {
  struct Dinner<U> {

    var leftovers: T
    var transformation: (T) -> U
  }
}

class Deli<Spices> { // expected-note {{'Spices' declared as parameter to type 'Deli'}}

  class Pepperoni {}
  struct Sausage {}
}

struct Pizzas<Spices> { // expected-note {{generic parameter 'Spices' declared here}}
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
  // expected-error@-1 {{cannot convert parent type 'Deli<Pepper>' to expected type 'Deli<ChiliFlakes>', arguments to generic parameter 'Spices' ('Pepper' and 'ChiliFlakes') are expected to be equal}}

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
  // expected-error@-1{{use of undeclared type 'ThisTypeDoesNotExist'}}
  associatedtype B where ThisTypeDoesNotExist == B
  // expected-error@-1{{use of undeclared type 'ThisTypeDoesNotExist'}}
  associatedtype C where ThisTypeDoesNotExist == ThisTypeDoesNotExist
  // expected-error@-1 2{{use of undeclared type 'ThisTypeDoesNotExist'}}
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
// expected-note@-4 {{generic parameter 'A' declared here}}
// expected-note@-5 {{generic parameter 'A' declared here}}
// expected-note@-6 {{generic parameter 'A' declared here}}
// expected-note@-7 {{generic parameter 'A' declared here}}
// expected-note@-8 {{generic parameter 'A' declared here}}
// expected-note@-9 {{generic parameter 'A' declared here}}
// expected-note@-10 {{generic parameter 'A' declared here}}
// expected-note@-11 {{generic parameter 'A' declared here}}
// expected-note@-12 {{generic parameter 'A' declared here}}

struct Y<A, B, C>{} // expected-note {{generic parameter 'A' declared here}}
// expected-note@-1 {{generic parameter 'C' declared here}}

struct YieldValue {
  var property: X<Bool> {
    _read {
      yield X<Int>() // expected-error {{cannot convert value of type 'X<Int>' to expected yield type 'X<Bool>', arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
    }
  }
}

func multipleArguments(y: Y<Int, Int, Int>) {
  let _: Y<Bool, Int, Float> = y // expected-error {{cannot convert value of type 'Y<Int, Int, Int>' to 'Y<Bool, Int, Float>' in assignment, arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
  // expected-error@-1 {{cannot convert value of type 'Y<Int, Int, Int>' to 'Y<Bool, Int, Float>' in assignment, arguments to generic parameter 'C' ('Int' and 'Float') are expected to be equal}}
}

func errorMessageVariants(x: X<Int>, x2: X<Bool> = X<Int>()) -> X<Bool> {
  // expected-error@-1 {{default argument value of type 'X<Int>' cannot be converted to type 'X<Bool>'}}
  let _: X<Bool> = x // expected-error {{cannot convert value of type 'X<Int>' to 'X<Bool>' in assignment, arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
  errorMessageVariants(x: x2, x2: x2) // expected-error {{cannot convert 'X<Bool>' to expected argument type 'X<Int>', arguments to generic parameter 'A' ('Bool' and 'Int') are expected to be equal}}
  let _: X<Bool> = { return x }() // expected-error {{cannot convert value of type 'X<Int>' to closure result type 'X<Bool>', arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
  let _: [X<Bool>] = [x] // expected-error {{cannot convert value of type 'X<Int>' to expected element type 'X<Bool>', arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
  let _ = x as X<Bool> // expected-error {{cannot convert value of type 'X<Int>' to type 'X<Bool>' in coercion, arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
  let _: X<Int>.Foo = X<Bool>.Foo() // expected-error {{cannot convert parent type 'X<Bool>' to expected type 'X<Int>', arguments to generic parameter 'A' ('Bool' and 'Int') are expected to be equal}}
  return x // expected-error {{cannot convert return expression of type 'X<Int>' to return type 'X<Bool>', arguments to generic parameter 'A' ('Int' and 'Bool') are expected to be equal}}
}
