// RUN: %target-typecheck-verify-swift

struct TupleStruct<First, @_typeSequence Rest> {
  var first: First
  var rest: (Rest...)
}

func debugPrint<@_typeSequence T>(_ items: T...)
  where T: CustomDebugStringConvertible
{
  /*for (item: T) in items {
    stdout.write(item.debugDescription)
  }*/
}

func max<@_typeSequence T>(_ values: T...) -> T?
  where T: Comparable
{
  return nil
}

func min<@_typeSequence T: Comparable>(_ values: T...) -> T? {
  return nil
}

func badParameter<T>(_ : @_typeSequence T) {} // expected-error {{attribute does not apply to type}}

func directAliases() {
  typealias Tuple<@_typeSequence Ts> = (Ts...)

  typealias Many<T, U, V, @_typeSequence Ws> = Tuple<T, U, V, Ws>

  let _: Many<Int, String, Double, Void, Void, Void, Void> = 42 // expected-error {{cannot convert value of type 'Int' to specified type}}
}

func bindPrefix() {
  struct Bind<Prefix, @_typeSequence U> {}

  typealias TooFew0 = Bind<> // expected-error {{expected type}}
  typealias TooFew1 = Bind<String> // OK
  typealias TooFew2 = Bind<String, String> // OK
  typealias JustRight = Bind<String, String, String> // OK
  typealias Oversaturated = Bind<String, String, String, String, String, String, String, String> // OK
}

func bindSuffix() {
  struct Bind<@_typeSequence U, Suffix> {}

  typealias TooFew0 = Bind<> // expected-error {{expected type}}
  typealias TooFew1 = Bind<String> // OK
  typealias TooFew2 = Bind<String, String> // OK
  typealias JustRight = Bind<String, String, String> // OK
  typealias Oversaturated = Bind<String, String, String, String, String, String, String, String> // OK
}

func bindPrefixAndSuffix() {
  struct Bind<Prefix, @_typeSequence U, Suffix> {} // expected-note {{generic type 'Bind' declared here}}

  typealias TooFew0 = Bind<> // expected-error {{expected type}}
  typealias TooFew1 = Bind<String> // expected-error {{generic type 'Bind' specialized with too few type parameters (got 1, but expected at least 2)}}
  typealias TooFew2 = Bind<String, String> // OK
  typealias JustRight = Bind<String, String, String> // OK
  typealias Oversaturated = Bind<String, String, String, String, String, String, String, String> // OK
}

func invalidPacks() {
  func monovariadic1() -> (String...) {} // expected-error {{cannot create expansion with non-variadic type 'String'}}
  func monovariadic2<T>() -> (T...) {} // expected-error 2 {{cannot create expansion with non-variadic type 'T'}}
  func monovariadic3<T, U>() -> (T, U...) {} // expected-error {{cannot create a variadic tuple}}
}
