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

class Deli<Spices> {

  class Pepperoni {}
  struct Sausage {}
}

struct Pizzas<Spices> {
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
      leftovers: Pizzas<ChiliFlakes>.NewYork(),  // expected-error {{cannot convert value of type 'Pizzas<ChiliFlakes>.NewYork' to expected argument type 'Pizzas<Pepper>.NewYork'}}
      transformation: { _ in HotDog() })
}

func badDiagnostic2() {

  let firstCourse = Pizzas<ChiliFlakes>.NewYork()

  var dinner = Lunch<Pizzas<ChiliFlakes>.NewYork>.Dinner<HotDog>(
      leftovers: firstCourse,
      transformation: { _ in HotDog() })

  let topping = Deli<Pepper>.Pepperoni()

  eatDinnerConcrete(d: firstCourse, t: topping)
  // expected-error@-1 {{cannot invoke 'eatDinnerConcrete' with an argument list of type '(d: Pizzas<ChiliFlakes>.NewYork, t: Deli<Pepper>.Pepperoni)'}}
  // expected-note@-2 {{overloads for 'eatDinnerConcrete' exist with these partially matching parameter lists: (d: Pizzas<ChiliFlakes>.NewYork, t: Deli<ChiliFlakes>.Pepperoni), (d: Pizzas<Pepper>.DeepDish, t: Deli<Pepper>.Pepperoni)}}

}

// Real error is that we cannot infer the generic parameter from context

func takesAny(_ a: Any) {}

func badDiagnostic3() {
  takesAny(Deli.self) // expected-error {{argument type 'Deli<_>.Type' does not conform to expected type 'Any'}}
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
