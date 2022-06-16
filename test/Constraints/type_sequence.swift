// RUN: %target-typecheck-verify-swift -enable-experimental-variadic-generics

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

func call() {
  func multipleParameters<@_typeSequence T>(xs: T..., ys: T...) -> (T...) { return xs }
  // expected-note@-1 {{in call to function 'multipleParameters(xs:ys:)'}}
  _ = multipleParameters()
  // expected-error@-1 2 {{generic parameter 'T' could not be inferred}}
  let x: (String) = multipleParameters(xs: "", ys: "")
  let (one, two) = multipleParameters(xs: "", 5.0, ys: "", 5.0)
  multipleParameters(xs: "", 5.0, ys: 5.0, "") // expected-error {{type of expression is ambiguous without more context}}

  func multipleSequences<@_typeSequence T, @_typeSequence U>(xs: T..., ys: U...) -> (T...) { return ys }
  // expected-note@-1 {{in call to function 'multipleSequences(xs:ys:)'}}
  // expected-error@-2 {{cannot convert return expression of type 'U' to return type 'T'}}

  _ = multipleSequences()
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}
  // expected-error@-2 {{generic parameter 'U' could not be inferred}}
  _ = multipleSequences(xs: "", ys: "")
  _ = multipleSequences(xs: "", 5.0, ys: 5.0, "")
}

func contextualTyping() {
  func firsts<@_typeSequence T>(_ seqs: [T]...) -> (T?...) {
    fatalError()
  }

  let (_, _): (Int?, String?) = firsts([42], [""]) // OK
  let (_, _): (String?, String?) = firsts([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '(String?, String?)'}}
  let (_, _): ([Int], String?) = firsts([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '([Int], String?)'}}
  let (_, _, _): (String?, String?, Int) = firsts([42], [""]) // expected-error {{'(Int?, String?)' is not convertible to '(String?, String?, Int)', tuples have a different number of elements}}

  func dependent<@_typeSequence T>(_ seqs: Array<T>...) -> (Array<T>.Element?...) {
    fatalError()
  }

  let (_, _): (Int?, String?) = dependent([42], [""]) // OK
  let (_, _): (String?, String?) = dependent([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '(String?, String?)'}}
  let (_, _): ([Int], String?) = dependent([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '([Int], String?)'}}
  let (_, _, _): (String?, String?, Int) = dependent([42], [""]) // expected-error {{'(Int?, String?)' is not convertible to '(String?, String?, Int)', tuples have a different number of elements}}
}
