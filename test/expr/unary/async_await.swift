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

func acceptAutoclosureNonAsync(_: @autoclosure () -> Int) async { }
func acceptAutoclosureAsync(_: @autoclosure () async -> Int) async { }

func acceptAutoclosureNonAsyncBad(_: @autoclosure () async -> Int) { }
// expected-error@-1{{'async' autoclosure parameter in a non-'async' function}}
// expected-note@-2{{add 'async' to function 'acceptAutoclosureNonAsyncBad' to make it asynchronous}}

struct HasAsyncBad {
  init(_: @autoclosure () async -> Int) { }
  // expected-error@-1{{'async' autoclosure parameter in a non-'async' function}}
}

func testAutoclosure() async {
  __await acceptAutoclosureAsync(getInt()) // expected-error{{call is 'async' in an autoclosure argument is not marked with 'await'}}
  __await acceptAutoclosureNonAsync(getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}

  __await acceptAutoclosureAsync(__await getInt())
  __await acceptAutoclosureNonAsync(__await getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}

  __await acceptAutoclosureAsync(getInt()) // expected-error{{call is 'async' in an autoclosure argument is not marked with 'await'}}
  __await acceptAutoclosureNonAsync(getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}
}

// Test inference of 'async' from the body of a closure.
func testClosure() {
  let closure = {
     __await getInt()
  }

  let _: () -> Int = closure // expected-error{{cannot convert value of type '() async -> Int' to specified type '() -> Int'}}

  let closure2 = { () async -> Int in
    print("here")
    return __await getInt()
  }

  let _: () -> Int = closure2 // expected-error{{cannot convert value of type '() async -> Int' to specified type '() -> Int'}}
}
