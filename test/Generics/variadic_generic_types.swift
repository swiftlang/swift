// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

// Disallowed cases
struct MultiplePack<each T, each U> {} // expected-error {{generic type cannot declare more than one type pack}}
typealias MultiplePackAlias<each T, each U> = (repeat each T, repeat each U) // expected-error {{generic type cannot declare more than one type pack}}

// Temporary limitations
enum EnumWithPack<each T> { // expected-error {{enums cannot declare a type pack}}
  case cheddar
}

class ClassWithPack<each T> {}

struct OuterStruct<each T> {
  enum NestedEnum { // expected-error {{enums cannot declare a type pack}}
    case smokedGouda
  }

  class NestedClass {}
}

class BadInheritance1: ClassWithPack<Int> {} // expected-error {{cannot inherit from a generic class that declares a type pack}}
class BadInheritance2: OuterStruct<Int>.NestedClass {} // expected-error {{cannot inherit from a generic class that declares a type pack}}

// Type resolution of variadic type aliases
func bindAll() {
  struct Bind<each U> {}

  typealias Zero = Bind< > // OK
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindPrefix() {
  struct Bind<Prefix, each U> {} // expected-note {{generic struct 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 1)}}
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindSuffix() {
  struct Bind<each U, Suffix> {} // expected-note {{generic struct 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 1)}}
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindPrefixAndSuffix() {
  struct Bind<Prefix, each U, Suffix> {} // expected-note 2{{generic struct 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 2)}}
  typealias One = Bind<Int> // expected-error {{generic type 'Bind' specialized with too few type parameters (got 1, but expected at least 2)}}
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
  typealias Four = Bind<Int, String, Float, Bool> // OK
}

func bindAliasAll() {
  typealias Bind<each U> = (repeat each U)

  typealias Zero = Bind< > // OK
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindAliasPrefix() {
  typealias Bind<Prefix, each U> = (Prefix, repeat each U) // expected-note {{generic type alias 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 1)}}
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindAliasSuffix() {
  typealias Bind<each U, Suffix> = (repeat each U, Suffix) // expected-note {{generic type alias 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 1)}}
  typealias One = Bind<Int> // OK
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
}

func bindAliasPrefixAndSuffix() {
  typealias Bind<Prefix, each U, Suffix> = (Prefix, repeat each U, Suffix) // expected-note 2{{generic type alias 'Bind' declared here}}

  typealias Zero = Bind< > // expected-error {{generic type 'Bind' specialized with too few type parameters (got 0, but expected at least 2)}}
  typealias One = Bind<Int> // expected-error {{generic type 'Bind' specialized with too few type parameters (got 1, but expected at least 2)}}
  typealias Two = Bind<Int, String> // OK
  typealias Three = Bind<Int, String, Float> // OK
  typealias Four = Bind<Int, String, Float, Bool> // OK
}

func invalidPackExpansion<each X, each Y, Z>(x: repeat each X, y: repeat each Y, z: Z) {
  typealias A<T, each U> = (T, repeat each U) // expected-note 4{{generic type alias 'A' declared here}}
  typealias B<each T, U> = (repeat each T, U) // expected-note 4{{generic type alias 'B' declared here}}

  typealias One = A<repeat each X> // expected-error {{generic type 'A' specialized with mismatched type parameter pack}}
  typealias Two = A<repeat each X, repeat each Y> // expected-error {{generic type 'A' specialized with mismatched type parameter pack}}
  typealias Three = A<repeat each X, Z> // expected-error {{generic type 'A' specialized with mismatched type parameter pack}}
  typealias Four = A<repeat each X, repeat each Y, Z> // expected-error {{generic type 'A' specialized with mismatched type parameter pack}}

  typealias Five = B<repeat each X> // expected-error {{generic type 'B' specialized with mismatched type parameter pack}}
  typealias Six = B<repeat each X, repeat each Y> // expected-error {{generic type 'B' specialized with mismatched type parameter pack}}
  typealias Seven = B<Z, repeat each X> // expected-error {{generic type 'B' specialized with mismatched type parameter pack}}
  typealias Eight = B<Z, repeat each X, repeat each Y> // expected-error {{generic type 'B' specialized with mismatched type parameter pack}}
}

func packExpansionInScalarArgument<each T>(_: repeat each T) {
  typealias A<U> = U
  typealias One = A<repeat each T> // expected-error {{pack expansion 'repeat each T' can only appear in a function parameter list, tuple element, or generic argument of a variadic type}}
}

// Make sure we diagnose instead of silently dropping the same-type requirement
// https://github.com/apple/swift/issues/70432

struct WithPack<each T> {}

// FIXME: The diagnostics are misleading.

extension WithPack<Int, Int> {}
// expected-error@-1 {{same-element requirements are not yet supported}}

extension WithPack where (repeat each T) == (Int, Int) {}
// expected-error@-1 {{generic signature requires types '(repeat each T)' and '(Int, Int)' to be the same}}