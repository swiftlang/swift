// RUN: %swift %s -verify

/* block comments */
/* /* nested too */ */

def f1(a: Int, y: Int) {}
def f2() {}
def f3() -> Int {}

def invalid_semi() {
  ; // expected-error {{';' statements are not allowed}} {{3-4=}}
}

def nested1(x: Int) {
  var y : Int
  
  def nested2(z: Int) -> Int {
    return x+y+z
  }
  
  nested2(1)
}

def funcdecl5(a: Int, y: Int) {
  var x : Int

  // a few statements
  if (x != 0) {
    if (x != 0 || f3() != 0) {
      // while with and without a space after it.
      while(true) { 4; 2; 1 }
      while (true) { 4; 2; 1 }
    }
  }

  // Assignment statement.
  x = x
  (x) = x

  // FIXME: Can we provide nicer diagnostics for this case?
  1 = x        // expected-error {{expression does not type-check}}
  (1) = x      // expected-error {{expression does not type-check}}
  (x:1).x = 1 // expected-error {{expression does not type-check}}
  var tup : (x:Int, y:Int)
  tup.x = 1

  var B : Bool

  // if/then/else.
  if (B) {
  } else if (y == 2) {
  }

  // FIXME: This diagnostic is terrible - rdar://12939553
  if x {}   // expected-error {{type 'Int' does not conform to protocol 'LogicValue'}}

  if true {
    if (B) {
    } else {
    }
  }

  if (B) {
    f1(1,2)
  } else {
    f2()
  }

  if (B) {
    if (B) {
      f1(1,2)
    } else {
      f2()
    }
  } else {
    f2()
  }
  
  // while statement.
  while (B) {
  }

  // It's okay to leave out the spaces in these.
  while(B) {}
  if(B) {}
}

struct infloopbool {
  def getLogicValue() -> infloopbool {
    return self
  }
}

def infloopbooltest() {
  if (infloopbool()) {} // expected-error {{type 'infloopbool' does not conform to protocol 'LogicValue'}}
}

// test "builder" API style
Int
  .min()

def for_loop() {
  var x = 0
  for ;; { }
  for x = 1; x != 42; ++x { }
  for infloopbooltest(); x != 12; infloopbooltest() {}
  
  for ; { } // expected-error {{expected ';' in 'for' statement}}
  
  for var y = 1; y != 42; ++y {}
  for (var y = 1; y != 42; ++y) {}
  var z = 10
  for (; z != 0; --z) {}
  for (z = 10; z != 0; --z) {}
  for var (a,b) = (0,12); a != b; --b {}
  for (var (a,b) = (0,12); a != b; --b) {}
  var j, k : Int
  for ((j,k) = (0,10); j != k; --k) {}
}

break // expected-error {{'break' is only allowed inside a loop}}
continue // expected-error {{'continue' is only allowed inside a loop}}
while true {
  def f() {
    break // expected-error {{'break' is only allowed inside a loop}}
    continue // expected-error {{'continue' is only allowed inside a loop}}
  }
}

def tuple_assign() {
  var a,b,c,d : Int
  (a,b) = (1,2)
  def f() -> (Int,Int) { return (1,2) }
  ((a,b), (c,d)) = (f(), f())
}

def missing_semicolons() {
  var a = new Int[4]a[0]  // expected-error{{expression resolves to an unused l-value}} expected-error{{consecutive statements}} {{21-21=;}}
  var w = 321
  def g() {}
  g() ++w             // expected-error{{consecutive statements}} {{6-6=;}}
  var y = w'g'        // expected-error{{consecutive statements}} {{12-12=;}}
  var z = w"hello"    // expected-error{{consecutive statements}} {{12-12=;}}
  class  C {}class  C2 {} // expected-error{{consecutive statements}} {{14-14=;}}
  struct S {}struct S2 {} // expected-error{{consecutive statements}} {{14-14=;}}
  def j() {}def k() {}  // expected-error{{consecutive statements}} {{13-13=;}}
}

//===--- Return statement.

return 42 // expected-error {{return invalid outside of a func}}

return // expected-error {{return invalid outside of a func}}

def NonVoidReturn1() -> Int {
  return // expected-error {{non-void function should return a value}}
}

def NonVoidReturn2() -> Int {
  return + // expected-error {{unary operator cannot be separated from its operand}} expected-error {{expected expression in 'return' statement}}
}

def VoidReturn1() {
  if true { return }
  // Semicolon should be accepted -- rdar://11344875
  return; // no-error
}

def VoidReturn2() {
  return () // no-error
}

def VoidReturn3() {
  return VoidReturn2() // no-error
}

//===--- If statement.

def IfStmt1() {
  if 1 > 0 // expected-error {{expected '{' after 'if' condition}}
  var x = 42
}

def IfStmt2() {
  if 1 > 0 {
  } else // expected-error {{expected '{' after 'else'}}
  var x = 42
}

//===--- While statement.

def WhileStmt1() {
  while 1 > 0 // expected-error {{expected '{' after 'while' condition}}
  var x = 42
}

//===--- Do-while statement.

def DoWhileStmt1() {
  do {} while true

  do {} while false

  do { break } while true
  do { continue } while true
}

def DoWhileStmt2() {
  do // expected-error {{expected '{' after 'do'}} expected-error {{expected 'while' in 'do-while' loop}}
}

def DoWhileStmt3() {
  do {
  } // expected-error {{expected 'while' in 'do-while' loop}}
}

def DoWhileStmt4() {
  do {
  } while + // expected-error {{unary operator cannot be separated from its operand}} expected-error {{expected expression in 'do-while' condition}}
}

