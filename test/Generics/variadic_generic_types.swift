// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

struct TupleStruct<First, Rest...> {
  var first: First
  var rest: (Rest...)
}

func directAliases() {
  typealias Tuple<Ts...> = (Ts...)

  typealias Many<T, U, V, Ws...> = Tuple<T, U, V, Ws... >

  let _: Many<Int, String, Double, Void, Void, Void, Void> = 42 // expected-error {{cannot convert value of type 'Int' to specified type}}
}

func bindPrefix() {
  struct Bind<Prefix, U...> {}

  typealias TooFew0 = Bind<> // expected-error {{expected type}}
  typealias TooFew1 = Bind<String> // OK
  typealias TooFew2 = Bind<String, String> // OK
  typealias JustRight = Bind<String, String, String> // OK
  typealias Oversaturated = Bind<String, String, String, String, String, String, String, String> // OK
}

func bindSuffix() {
  struct Bind<U..., Suffix> {}

  typealias TooFew0 = Bind<> // expected-error {{expected type}}
  typealias TooFew1 = Bind<String> // OK
  typealias TooFew2 = Bind<String, String> // OK
  typealias JustRight = Bind<String, String, String> // OK
  typealias Oversaturated = Bind<String, String, String, String, String, String, String, String> // OK
}

func bindPrefixAndSuffix() {
  struct Bind<Prefix, U..., Suffix> {} // expected-note {{generic type 'Bind' declared here}}

  typealias TooFew0 = Bind<> // expected-error {{expected type}}
  typealias TooFew1 = Bind<String> // expected-error {{generic type 'Bind' specialized with too few type parameters (got 1, but expected at least 2)}}
  typealias TooFew2 = Bind<String, String> // OK
  typealias JustRight = Bind<String, String, String> // OK
  typealias Oversaturated = Bind<String, String, String, String, String, String, String, String> // OK
}
