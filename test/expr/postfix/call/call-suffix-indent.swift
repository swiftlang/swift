// RUN: %swift -parse %s -verify

class A {
  func foo(i: Int) bar(Double) { }
  func foo(i: Int) bar(Double) wibble(String) -> String { return "" }

  func foo(i: Int) bar(Double) wobble(String) -> Int { return 5 }

  func foo(i: Int) bar(Double) closure(() -> String) -> String { 
    return closure() 
  }

  func foo(i: Int) bar(Double) closure(() -> String) onError(() -> ()) 
         -> String { 
    return closure() 
  }

  func closure(() -> Int) onError(() -> ()) {  }
 
  init withRed(r: Int) green(Int) blue(Int) { }
}

func global(i: Int) printer(s: String) { }

// -----------------------------------------------------------------------------
// Test multi-piece selector calls with continuations
// -----------------------------------------------------------------------------

// Test continuation based on indentation (simple cases).
func testSimpleIndentationPass(a: A, i: Int, d: Double, s: String) -> String {  
  // Indented from the nearest enclosing declaration continues the call.
  var x = a.foo(i) 
    bar(d) 
    wibble(s)

  x = "hello"

  // ... or the nearest statement.
  return a.foo(i) 
    bar(d) 
    wibble(s)

  // ... or expression.
  a.foo(i) 
   bar(d) 
   wibble(s)
}

// Test non-continuations based on indentation (simple cases).
func testSimpleIdentationFail(a: A, i: Int, d: Double, s: String) {
  // Declarations.
  var x = a.foo(i) 
    bar(d) 
  wibble(s)  // expected-error{{use of unresolved identifier 'wibble'}}

  // Expressions.
  a.foo(i) bar(d)
  wibble(s) // expected-error{{use of unresolved identifier 'wibble'}}

  // Statements.
  return a.foo(i)
    bar(d)
  wibble(s)  // expected-error{{use of unresolved identifier 'wibble'}}
}

// Test cases where we always have a continuation, regardless of
// context.
func testAlwaysContinuationContexts(a: A, i: Int, d: Double, s: String) {  
  // Parentheses
  var x = (a.foo(i)
bar(d)
wibble(s))
  
  // Array literals
  var arr = [a.foo(i)
bar(d)
wibble(s)]

  // Array allocations
  var arr2 = new Int[a.foo(i)
bar(d)
wobble(s)]
}

// Testing involving multiple statements on a line.
func testMultipleStatements(a: A, i: Int, d: Double, s: String) {
  // The indentation at the start of the nearest declaration's line
  // matters...
  var x1 = 
      5; var x2 = a.foo(i)
       bar(d)
       wibble(s)

  // ... not the start of the chain of statements.
  var y1 = 
      5; var y2 = a.foo(i)
       bar(d)
    wibble(s) // expected-error{{use of unresolved identifier 'wibble'}}
}

// Tests where comments affect indentation.
func testComments(a: A, i: Int, d: Double, s: String) {
  /* leading comment */ var x = a.foo(i)
    /* right aligned */         bar(d) // still part of continuation
  /*  longer aligned */       wibble(d) // expected-error{{use of unresolved identifier 'wibble'}}
}

// Tests were spaces vs. tabs affect indentation.
func testTabs(a: A, i: Int, d: Double, s: String) {
	var x1 = a.foo(i)
	  bar(d)
          wibble(s) // expected-error{{use of unresolved identifier 'wibble'}}
}

// Tests for adjacent statements that get parsed as a single "selector
// call".
func testAdjacentStatements() {
// FIXME: We'll need to do some heroic recovery here.
  println(5) // expected-error{{expression does not type-check}}
    println("hello")
}

