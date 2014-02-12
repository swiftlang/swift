// RUN: %swift -parse %s -verify

class A {
  func foo(i: Int) bar(Double) { }
  func foo(i: Int) bar(Double) wibble(String) -> String { return "" }
}

func global(i: Int) printer(s: String) { }

// -----------------------------------------------------------------------------
// Test multi-piece selector calls on a single line
// -----------------------------------------------------------------------------

func testMethodCallSingleLine(a: A, i: Int, d: Double, s: String) {  
  var x = a.foo(i) bar(d) wibble(s)
  x = "hello"

  // Line break ends the current call-suffix
  a.foo(i) bar(d)
  wibble(s) // expected-error{{use of unresolved identifier 'wibble'}}
}

func testGlobalCallSingleLine(i: Int, s: String) {
  global(i) printer(s)
}

extension A {
  func testImplicitSelfCallSingleLine(i: Int, d: Double, s: String) {
    var x = foo(i) bar(d) wibble(s)
    x = "hello"
  }
}

// -----------------------------------------------------------------------------
// Test multi-piece selector calls that chain
// -----------------------------------------------------------------------------
func testMethodCallSingleLineChaining(a: A, i: Int, d: Double, s: String) {
  var x = a.foo(i) bar(d) wibble(s).codeUnits
}

// -----------------------------------------------------------------------------
// Test parser recovery
// -----------------------------------------------------------------------------
func testIllFormed(a: A, i: Int, d: Double, s: String) {
  var x = a.foo(i) bar // expected-error{{expected '(' following selector piece 'bar'}}
}
