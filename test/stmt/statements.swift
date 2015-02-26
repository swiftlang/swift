// RUN: %target-parse-verify-swift -enable-character-literals

/* block comments */
/* /* nested too */ */

func f1(a: Int, y: Int) {}
func f2() {}
func f3() -> Int {}

func invalid_semi() {
  ; // expected-error {{';' statements are not allowed}} {{3-4=}}
}

func nested1(x: Int) {
  var y : Int
  
  func nested2(z: Int) -> Int {
    return x+y+z
  }
  
  nested2(1)
}

func funcdecl5(a: Int, y: Int) {
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
  x = y
  (x) = y

  // FIXME: Can we provide nicer diagnostics for this case?
  1 = x        // expected-error {{cannot assign to a literal value}}
  (1) = x      // expected-error {{cannot assign to the result of this expression}}
  (x:1).x = 1 // expected-error {{cannot assign to immutable value of type 'Int'}}
  var tup : (x:Int, y:Int)
  tup.x = 1

  var B : Bool

  // if/then/else.
  if (B) {
  } else if (y == 2) {
  }

  // FIXME: This diagnostic is terrible - rdar://12939553
  if x {}   // expected-error {{type 'Int' does not conform to protocol 'BooleanType'}}

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
  var boolValue: infloopbool {
    return self
  }
}

func infloopbooltest() {
  if (infloopbool()) {} // expected-error {{type 'infloopbool' does not conform to protocol 'BooleanType'}}
}

// test "builder" API style
extension Int {
  static func builder() { }
}
Int
  .builder()

func for_loop() {
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
  for var i = 0, j = 0; i * j < 10; i++, j++ {}
  for j = 0, k = 52; j < k; ++j, --k { }
  // rdar://19540536
  // expected-error@+4{{expected var declaration in a 'for' statement}}
  // expected-error@+3{{type of expression is ambiguous without more context}}
  // expected-error@+2{{expected an attribute name}}
  // expected-error@+1{{braced block of statements is an unused closure}}
  for @ {}
}

break // expected-error {{'break' is only allowed inside a loop}}
continue // expected-error {{'continue' is only allowed inside a loop}}
while true {
  func f() {
    break // expected-error {{'break' is only allowed inside a loop}}
    continue // expected-error {{'continue' is only allowed inside a loop}}
  }

  // Labeled if
  MyIf: if 1 != 2 {
    break MyIf
    continue MyIf  // expected-error {{'continue' cannot be used with if statements}}
    break          // break the while
    continue       // continue the while.
  }
}

// Labeled if
MyOtherIf: if 1 != 2 {
  break MyOtherIf
  continue MyOtherIf  // expected-error {{'continue' cannot be used with if statements}}
  break          // expected-error {{unlabeled 'break' is only allowed inside a loop or switch, a labeled break is required to exit an if}}
  continue       // expected-error {{'continue' is only allowed inside a loop}}
}


func tuple_assign() {
  var a,b,c,d : Int
  (a,b) = (1,2)
  func f() -> (Int,Int) { return (1,2) }
  ((a,b), (c,d)) = (f(), f())
}

func missing_semicolons() {
  var w = 321
  func g() {}
  g() ++w             // expected-error{{consecutive statements}} {{6-6=;}}
  var y = w'g'        // expected-error{{consecutive statements}} {{12-12=;}} // expected-error {{expression does not conform to type 'CharacterLiteralConvertible'}}
  var z = w"hello"    // expected-error{{consecutive statements}} {{12-12=;}}
  class  C {}class  C2 {} // expected-error{{consecutive statements}} {{14-14=;}}
  struct S {}struct S2 {} // expected-error{{consecutive statements}} {{14-14=;}}
  func j() {}func k() {}  // expected-error{{consecutive statements}} {{14-14=;}}
}

//===--- Return statement.

return 42 // expected-error {{return invalid outside of a func}}

return // expected-error {{return invalid outside of a func}}

func NonVoidReturn1() -> Int {
  return // expected-error {{non-void function should return a value}}
}

func NonVoidReturn2() -> Int {
  return + // expected-error {{unary operator cannot be separated from its operand}} expected-error {{expected expression in 'return' statement}}
}

func VoidReturn1() {
  if true { return }
  // Semicolon should be accepted -- rdar://11344875
  return; // no-error
}

func VoidReturn2() {
  return () // no-error
}

func VoidReturn3() {
  return VoidReturn2() // no-error
}

//===--- If statement.

func IfStmt1() {
  if 1 > 0 // expected-error {{expected '{' after 'if' condition}}
  var x = 42
}

func IfStmt2() {
  if 1 > 0 {
  } else // expected-error {{expected '{' after 'else'}}
  var x = 42
}

//===--- While statement.

func WhileStmt1() {
  while 1 > 0 // expected-error {{expected '{' after 'while' condition}}
  var x = 42
}

//===--- Do-while statement.

func DoWhileStmt1() {
  do {} while true

  do {} while false

  do { break } while true
  do { continue } while true
}

func DoWhileStmt2() {
  do // expected-error {{expected '{' after 'do'}} expected-error {{expected 'while' in 'do-while' loop}}
}

func DoWhileStmt3() {
  do {
  } // expected-error {{expected 'while' in 'do-while' loop}}
}

func DoWhileStmt4() {
  do {
  } while + // expected-error {{unary operator cannot be separated from its operand}} expected-error {{expected expression in 'do-while' condition}}
}

func brokenSwitch(x: Int) -> Int {
  switch x {
  case .Blah(var rep): // expected-error{{enum case pattern cannot match values of the non-enum type 'Int'}}
    return rep
  }
}

func breakContinue(x : Int) -> Int {

Outer:
  for a in 0...1000 {

  Switch:
    switch x {
    case 42: break Outer
    case 97: continue Outer
    case 102: break Switch
    case 13: continue
    case 139: break   // <rdar://problem/16563853> 'break' should be able to break out of switch statements
    }
  }
  
  // <rdar://problem/16692437> shadowing loop labels should be an error
Loop:  // expected-note {{previously declared here}}
  for i in 0...2 {
  Loop:  // expected-error {{label 'Loop' cannot be reused on an inner statement}}
    for j in 0...2 {
    }
  }


  // <rdar://problem/16798323> Following a 'break' statment by another statement on a new line result in an error/fit-it
  switch 5 {
  case 5:
    println("before the break")
    break
    println("after the break")    // println is not a label for the break.
  default:
    println("")
  }
  
  var x : Int? = 42
  
  // <rdar://problem/16879701> Should be able to pattern match 'nil' against optionals
  switch x {
  case .Some(42): break
  case nil: break
  
  }
  
}

