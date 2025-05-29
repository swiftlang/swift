// RUN: %target-typecheck-verify-swift

enum MSV : Error {
  case Foo, Bar, Baz
  case CarriesInt(Int)

  var _domain: String { return "" }
  var _code: Int { return 0 }
}

func opaque_error() -> Error { return MSV.Foo }

func one() {
  do {
    true ? () : throw opaque_error() // expected-error {{expected expression after '? ... :' in ternary expression}}
  } catch _ {
  }

  do {
    
  } catch { // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
    let error2 = error
  }

  do {
  } catch where true { // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
    let error2 = error
  } catch {
  }
  
  // <rdar://problem/20985280> QoI: improve diagnostic on improper pattern match on type
  do {
    throw opaque_error()
  } catch MSV { // expected-error {{'is' keyword required to pattern match against type name}} {{11-11=is }}
  } catch {
  }

  do {
    throw opaque_error()
  } catch is Error {  // expected-warning {{'is' test is always true}}
  }
  
  func foo() throws {}
  
  do {
#if false
    try foo()
#endif
  } catch {    // don't warn, #if code should be scanned.
  }

  do {
#if compiler(>=10)
    throw opaque_error()
#endif
  } catch {    // don't warn, #if code should be scanned.
  }

  do {
#if false
    throw opaque_error()
#endif
  } catch {    // don't warn, #if code should be scanned.
  }
  
  do {
    throw opaque_error()
  } catch MSV.Foo, MSV.CarriesInt(let num) { // expected-error {{'num' must be bound in every pattern}}
  } catch {
  }
}

func takesAutoclosure(_ fn : @autoclosure () -> Int) {}
func takesThrowingAutoclosure(_ fn : @autoclosure () throws -> Int) {}

func genError() throws -> Int { throw MSV.Foo }
func genNoError() -> Int { return 0 }

func testAutoclosures() throws {
  takesAutoclosure(genError()) // expected-error {{call can throw, but it is not marked with 'try' and it is executed in a non-throwing autoclosure}}
  takesAutoclosure(genNoError())

  try takesAutoclosure(genError()) // expected-error {{call can throw, but it is executed in a non-throwing autoclosure}}
  try takesAutoclosure(genNoError()) // expected-warning {{no calls to throwing functions occur within 'try' expression}}

  takesAutoclosure(try genError()) // expected-error {{call can throw, but it is executed in a non-throwing autoclosure}}
  takesAutoclosure(try genNoError()) // expected-warning {{no calls to throwing functions occur within 'try' expression}}

  takesThrowingAutoclosure(try genError())
  takesThrowingAutoclosure(try genNoError()) // expected-warning {{no calls to throwing functions occur within 'try' expression}}

  try takesThrowingAutoclosure(genError())
  try takesThrowingAutoclosure(genNoError()) // expected-warning {{no calls to throwing functions occur within 'try' expression}}

  takesThrowingAutoclosure(genError()) // expected-error {{call can throw but is not marked with 'try'}}
                                       // expected-note@-1 {{did you mean to use 'try'?}} {{28-28=try }}
                                       // expected-note@-2 {{did you mean to handle error as optional value?}} {{28-28=try? }}
                                       // expected-note@-3 {{did you mean to disable error propagation?}} {{28-28=try! }}
  takesThrowingAutoclosure(genNoError())
}

func illformed() throws {
    do {
      _ = try genError()

    } catch MSV.CarriesInt(let i) where i == genError()) { // expected-error {{call can throw, but errors cannot be thrown out of a catch guard expression}} expected-error {{expected '{'}}
    }
}

func postThrows() -> Int throws { // expected-error{{'throws' may only occur before '->'}}{{19-19=throws }}{{26-33=}}
  return 5
}

func postThrows2() -> throws Int { // expected-error{{'throws' may only occur before '->'}}{{20-20=throws }}{{23-30=}}
  return try postThrows()
}

func postRethrows(_ f: () throws -> Int) -> Int rethrows { // expected-error{{'rethrows' may only occur before '->'}}{{42-42=rethrows }}{{49-58=}}
  return try f()
}

func postRethrows2(_ f: () throws -> Int) -> rethrows Int { // expected-error{{'rethrows' may only occur before '->'}}{{43-43=rethrows }}{{46-55=}}
  return try f()
}

func postThrows3() {
  _ = { () -> Int throws in } // expected-error {{'throws' may only occur before '->'}} {{19-26=}} {{12-12=throws }}
}

func dupThrows1() throws rethrows -> throws Int throw {}
// expected-error@-1 {{'rethrows' has already been specified}} {{26-35=}}
// expected-error@-2 {{'throws' has already been specified}} {{38-45=}}
// expected-error@-3 {{'throw' has already been specified}} {{49-55=}}

func dupThrows2(_ f: () throws -> rethrows Int) {}
// expected-error@-1 {{'rethrows' has already been specified}} {{35-44=}}

func dupThrows3() {
  _ = { () try throws in }
// expected-error@-1 {{expected throwing specifier; did you mean 'throws'?}} {{12-15=throws}}
// expected-error@-2 {{'throws' has already been specified}} {{16-23=}}

  _ = { () throws -> Int throws in }
// expected-error@-1 {{'throws' has already been specified}} {{26-33=}}
}

func incompleteThrowType() {
  // FIXME: Bad recovery for incomplete function type.
  let _: () throws
  // expected-error @-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error @-2 {{expected expression}}
}

// rdar://21328447
func fixitThrow0() throw {} // expected-error{{expected throwing specifier; did you mean 'throws'?}} {{20-25=throws}}
func fixitThrow1() throw -> Int {} // expected-error{{expected throwing specifier; did you mean 'throws'?}} {{20-25=throws}}
func fixitThrow2() throws {
  var _: (Int)
  throw MSV.Foo
  var _: (Int) throw -> Int // expected-error{{expected throwing specifier; did you mean 'throws'?}} {{16-21=throws}}
}

let fn: () -> throws Void  // expected-error{{'throws' may only occur before '->'}} {{12-12=throws }} {{15-22=}}

// https://github.com/apple/swift/issues/53979

func fixitTry0<T>(a: T) try where T:ExpressibleByStringLiteral {} // expected-error{{expected throwing specifier; did you mean 'throws'?}} {{25-28=throws}}
func fixitTry1<T>(a: T) try {} // expected-error{{expected throwing specifier; did you mean 'throws'?}} {{25-28=throws}}
func fixitTry2() try {} // expected-error{{expected throwing specifier; did you mean 'throws'?}} {{18-21=throws}}
let fixitTry3 : () try -> Int // expected-error{{expected throwing specifier; did you mean 'throws'?}} {{20-23=throws}}

func fixitAwait0() await { } // expected-error{{expected async specifier; did you mean 'async'?}}{{20-25=async}}
func fixitAwait1() await -> Int { } // expected-error{{expected async specifier; did you mean 'async'?}}{{20-25=async}}
func fixitAwait2() throws await -> Int { } // expected-error{{expected async specifier; did you mean 'async'?}}{{27-32=async}}
