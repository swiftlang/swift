// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func bindAll() {
  struct Bind<U...> {}

  typealias Zero = Bind< > // OK
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindPrefix() {
  struct Bind<Prefix, U...> {} // expected-note {{generic type 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 1)}}
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindSuffix() {
  struct Bind<U..., Suffix> {} // expected-note {{generic type 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 1)}}
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindPrefixAndSuffix() {
  struct Bind<Prefix, U..., Suffix> {} // expected-note 2{{generic type 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 2)}}
  typealias One = Bind<Int> // expected-error {{generic type 'Bind' specialized with too few type parameters (got 1, but expected at least 2)}}
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
  typealias Four = Bind<Int, String, Float, Bool> // OK
}

func bindAliasAll() {
  typealias Bind<U...> = (U...)

  typealias Zero = Bind< > // OK
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindAliasPrefix() {
  typealias Bind<Prefix, U...> = (Prefix, U...) // expected-note {{generic type 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 1)}}
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindAliasSuffix() {
  typealias Bind<U..., Suffix> = (U..., Suffix) // expected-note {{generic type 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 1)}}
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindAliasPrefixAndSuffix() {
  typealias Bind<Prefix, U..., Suffix> = (Prefix, U..., Suffix) // expected-note 2{{generic type 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 2)}}
  typealias One = Bind<Int> // expected-error {{generic type 'Bind' specialized with too few type parameters (got 1, but expected at least 2)}}
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
  typealias Four = Bind<Int, String, Float, Bool> // OK
}