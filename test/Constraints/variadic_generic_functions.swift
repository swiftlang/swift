// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func debugPrint<each T>(_ items: repeat each T)
  where each T: CustomDebugStringConvertible
{
  /*for (item: T) in items {
    stdout.write(item.debugDescription)
  }*/
}

func max<each T>(_ values: repeat each T) -> T?
  where each T: Comparable
{
  return nil
}

func min<each T: Comparable>(_ values: repeat each T) -> T? {
  return nil
}

func invalidPacks() {
  func monovariadic1() -> (each String) {} // expected-error {{'each' cannot be applied to non-pack type 'String'}}
  func monovariadic2<T>() -> (repeat T) {} // expected-error {{variadic expansion 'T' must contain at least one variadic generic parameter}}
  func monovariadic3<T, U>() -> (T, repeat U) {} // expected-error {{variadic expansion 'U' must contain at least one variadic generic parameter}}
}

func call() {
  func multipleParameters<each T>(xs: repeat each T, ys: repeat each T) -> (repeat each T) {
    return (repeat each xs)
  }
  multipleParameters()

  let x: String = multipleParameters(xs: "", ys: "")
  let (one, two) = multipleParameters(xs: "", 5.0, ys: "", 5.0)
  multipleParameters(xs: "", 5.0, ys: 5.0, "") // expected-error {{type of expression is ambiguous without more context}}

  func multipleSequences<each T, each U>(xs: repeat each T, ys: repeat each U) -> (repeat each T) {
    return (repeat each ys)
    // expected-error@-1 {{cannot convert return expression of type '(repeat each U)' to return type '(repeat each T)'}}
  }

  multipleSequences()
  _ = multipleSequences(xs: "", ys: "")
  _ = multipleSequences(xs: "", 5.0, ys: 5.0, "")
}

func contextualTyping() {
  func firsts<each T>(_ seqs: repeat [each T]) -> (repeat (each T)?) {
    fatalError()
  }

  let (_, _): (Int?, String?) = firsts([42], [""]) // OK
  let (_, _): (String?, String?) = firsts([42], [""]) // expected-error {{cannot convert value of type '((Int)?, (String)?)' to specified type '(String?, String?)'}}
  let (_, _): ([Int], String?) = firsts([42], [""]) // expected-error {{cannot convert value of type '((Int)?, (String)?)' to specified type '([Int], String?)'}}
  let (_, _, _): (String?, String?, Int) = firsts([42], [""]) // expected-error {{'((Int)?, (String)?)' is not convertible to '(String?, String?, Int)', tuples have a different number of elements}}

  func dependent<each T>(_ seqs: repeat Array<each T>) -> (repeat Array<each T>.Element?) {
    fatalError()
  }

  let (_, _): (Int?, String?) = dependent([42], [""]) // OK
  let (_, _): (String?, String?) = dependent([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '(String?, String?)'}}
  let (_, _): ([Int], String?) = dependent([42], [""]) // expected-error {{cannot convert value of type '(Int?, String?)' to specified type '([Int], String?)'}}
  let (_, _, _): (String?, String?, Int) = dependent([42], [""]) // expected-error {{'(Int?, String?)' is not convertible to '(String?, String?, Int)', tuples have a different number of elements}}
}
