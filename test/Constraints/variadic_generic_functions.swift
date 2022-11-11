// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func debugPrint<T...>(_ items: T...)
  where T: CustomDebugStringConvertible
{
  /*for (item: T) in items {
    stdout.write(item.debugDescription)
  }*/
}

func max<T...>(_ values: T...) -> T?
  where T: Comparable
{
  return nil
}

func min<T...: Comparable>(_ values: T...) -> T? {
  return nil
}

func invalidPacks() {
  func monovariadic1() -> (String...) {} // expected-error {{variadic expansion 'String' must contain at least one variadic generic parameter}}
  func monovariadic2<T>() -> (T...) {} // expected-error {{variadic expansion 'T' must contain at least one variadic generic parameter}}
  func monovariadic3<T, U>() -> (T, U...) {} // expected-error {{variadic expansion 'U' must contain at least one variadic generic parameter}}
}

func call() {
  func multipleParameters<T...>(xs: T..., ys: T...) -> (T...) { return (_: xs) }
  multipleParameters()

  let x: (_: String) = multipleParameters(xs: "", ys: "")
  let (one, two) = multipleParameters(xs: "", 5.0, ys: "", 5.0)
  multipleParameters(xs: "", 5.0, ys: 5.0, "") // expected-error {{type of expression is ambiguous without more context}}

  func multipleSequences<T..., U...>(xs: T..., ys: U...) -> (T...) { return (_: ys) }
  // expected-error@-1 {{cannot convert return expression of type '(U...)' to return type '(T...)'}}

  multipleSequences()
  _ = multipleSequences(xs: "", ys: "")
  _ = multipleSequences(xs: "", 5.0, ys: 5.0, "")
}

func contextualTyping() {
  func firsts<T...>(_ seqs: [T]...) -> (T?...) {
    fatalError()
  }

  let (_, _): (Int?, String?) = firsts([42], [""]) // OK
  let (_, _): (String?, String?) = firsts([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '(String?, String?)'}}
  let (_, _): ([Int], String?) = firsts([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '([Int], String?)'}}
  let (_, _, _): (String?, String?, Int) = firsts([42], [""]) // expected-error {{'(Int?, String?)' is not convertible to '(String?, String?, Int)', tuples have a different number of elements}}

  func dependent<T...>(_ seqs: Array<T>...) -> (Array<T>.Element?...) {
    fatalError()
  }

  let (_, _): (Int?, String?) = dependent([42], [""]) // OK
  let (_, _): (String?, String?) = dependent([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '(String?, String?)'}}
  let (_, _): ([Int], String?) = dependent([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '([Int], String?)'}}
  let (_, _, _): (String?, String?, Int) = dependent([42], [""]) // expected-error {{'(Int?, String?)' is not convertible to '(String?, String?, Int)', tuples have a different number of elements}}
}
