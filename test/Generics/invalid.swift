// RUN: %target-typecheck-verify-swift

func bet() where A : B {} // expected-error {{'where' clause cannot be attached to a non-generic declaration}}

typealias gimel where A : B // expected-error {{'where' clause cannot be attached to a non-generic declaration}}
// expected-error@-1 {{expected '=' in typealias declaration}}

class dalet where A : B {} // expected-error {{'where' clause cannot be attached to a non-generic declaration}}

protocol he where A : B { // expected-error {{where clauses on protocols are fragile; use '-swift-version 4' to experiment.}}

  associatedtype vav where A : B // expected-error {{where clauses on associated types are fragile; use '-swift-version 4' to experiment.}}
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

  _ = Lunch<Pizzas<Pepper>.NewYork>.Dinner<HotDog>( // expected-error {{expression type 'Lunch<Pizzas<Pepper>.NewYork>.Dinner<HotDog>' is ambiguous without more context}}
      leftovers: Pizzas<ChiliFlakes>.NewYork(),
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
