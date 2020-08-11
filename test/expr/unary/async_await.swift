// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency

func test1(asyncfp : () async -> Int, fp : () -> Int) async {
  _ = __await asyncfp()
  _ = __await asyncfp() + asyncfp()
  _ = __await asyncfp() + fp()
  _ = __await fp() + 42  // expected-warning {{no calls to 'async' functions occur within 'await' expression}}
  _ = asyncfp() // expected-error {{call is 'async' but is not marked with 'await'}}
}

func getInt() async -> Int { return 5 }

// Locations where "await" is prohibited.
func test2(
  defaulted: Int = __await getInt() // expected-error{{'async' call cannot occur in a default argument}}
) async {
  defer {
    _ = __await getInt() // expected-error{{'async' call cannot occur in a defer body}}
  }
  print("foo")
}

func test3() { // expected-note{{add 'async' to function 'test3()' to make it asynchronous}}
  _ = __await getInt() // expected-error{{'async' in a function that does not support concurrency}}
}

enum SomeEnum: Int {
case foo = __await 5 // expected-error{{raw value for enum case must be a literal}}
}

struct SomeStruct {
  var x = __await getInt() // expected-error{{'async' call cannot occur in a property initializer}}
  static var y = __await getInt() // expected-error{{'async' call cannot occur in a global variable initializer}}
}

func acceptAutoclosureNonAsync(_: @autoclosure () -> Int) { }
func acceptAutoclosureAsync(_: @autoclosure () async -> Int) { }

func testAutoclosure() async {
  acceptAutoclosureAsync(getInt()) // expected-error{{call is 'async' in an autoclosure argument is not marked with 'await'}}
  acceptAutoclosureNonAsync(getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}

  acceptAutoclosureAsync(__await getInt())
  acceptAutoclosureNonAsync(__await getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}

  __await acceptAutoclosureAsync(getInt()) // expected-error{{call is 'async' in an autoclosure argument is not marked with 'await'}}
  // expected-warning@-1{{no calls to 'async' functions occur within 'await' expression}}
  __await acceptAutoclosureNonAsync(getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}
  // expected-warning@-1{{no calls to 'async' functions occur within 'await' expression}}
}
