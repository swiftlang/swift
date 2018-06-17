// RUN: %target-swift-frontend -emit-sil %s -verify

func overflowTrap() {
  let _ = Int8(123231)
  //#assert(Int8(124) + 8 > 42)
}

func isOne(_ x: Int) -> Bool {
  return x == 1
}

func assertionSuccess() {
  #assert(isOne(1))
  #assert(isOne(1), "1 is not 1")
}

func assertionFailure() {
  #assert(isOne(2)) // expected-error{{assertion failed}}
  #assert(isOne(2), "2 is not 1") // expected-error{{2 is not 1}}
}

func nonConstant() {
  #assert(isOne(Int(readLine()!)!)) // expected-error{{#assert condition not constant}}
  #assert(isOne(Int(readLine()!)!), "input is not 1") // expected-error{{#assert condition not constant}}
}

// @constexpr
func recursive(a: Int) -> Int {
  if a == 0 { return 0 }     // expected-note {{expression is too large to evaluate at compile-time}}
  return recursive(a: a-1)
}

func test_recursive() {
  // expected-error @+1 {{#assert condition not constant}}
  #assert(recursive(a: 20000) > 42)
}

// @constexpr
func loops1(a: Int) -> Int {
  var x = 42
  while x <= 42 {
    x += a
  } // expected-note {{control flow loop found}}
  return x
}

// @constexpr
func loops2(a: Int) -> Int {
  var x = 42
  for i in 0 ... a {
    x += i
  }
  return x
}


func test_loops() {
  // expected-error @+1 {{#assert condition not constant}}
  #assert(loops1(a: 20000) > 42)

  // TODO: xpected-error @+1 {{#assert condition not constant}}
  //#assert(loops2(a: 20000) > 42)
}


//===----------------------------------------------------------------------===//
// Reduced testcase propagating substitutions around.

protocol substitutionsP {
  init<T: substitutionsP>(something: T)

  func get() -> Int
}

struct substitutionsX : substitutionsP {
  var state : Int
  init<T: substitutionsP>(something: T) {
    // BUG: expected-note @+1 {{could not fold operation}}
    state = something.get()
  }

  func get() -> Int {
    return state
  }
}

struct substitutionsY : substitutionsP {
  init() {}
  init<T: substitutionsP>(something: T) {
  }

  func get() -> Int {
    return 123
  }
}
func substitutionsF<T: substitutionsP>(_: T.Type) -> T {
  return T(something: substitutionsY())
}

func testProto() {
  // BUG: expected-error @+1 {{#assert condition not constant}}
  #assert(substitutionsF(substitutionsX.self).get() == 123)
}

//===----------------------------------------------------------------------===//

