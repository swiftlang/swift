// RUN: %target-typecheck-verify-swift -enable-throw-without-try

// Test the -enable-throw-without-try option. Throwing function calls should
// not require annotation with 'try'.

// rdar://21444103 - only at the top level

func foo() throws -> Int { return 0 }

// That applies to global "script" code.

var global: Int = 0
global = foo() // no error
global = try foo() // still no error

var global2: Int = foo() // no error
var global3: Int = try foo() // no error

// That includes autoclosures.
func doLazy(_ fn: @autoclosure () throws -> Int) {}
doLazy(foo())

// It doesn't include explicit closures.
var closure: () -> () = {
  _ = foo() // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
  doLazy(foo()) // expected-error {{call can throw but is not marked with 'try'}}
}

// Or any other sort of structure.

struct A {
  static var lazyCache: Int = foo() // expected-error {{call can throw, but errors cannot be thrown out of a global variable initializer}}
}

func baz() throws -> Int {
  var x: Int = 0
  x = foo() // expected-error{{call can throw but is not marked with 'try'}}
  x = try foo() // no error
  return x
}

func baz2() -> Int {
  var x: Int = 0
  x = foo() // expected-error{{call can throw, but it is not marked with 'try' and the error is not handled}}
  x = try foo() // expected-error{{errors thrown from here are not handled}}
  return x
}
