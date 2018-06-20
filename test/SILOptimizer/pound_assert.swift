// RUN: %target-swift-frontend -emit-sil %s -verify

func overflowTrap() {
  let _ = Int8(123231)  // TODO: No warning?

  // TODO: Generate a better error message for this, we end up in:
  // sil [noinline] [_semantics "arc.programtermination_point"] [canonical] @$Ss18_fatalErrorMessage__4file4line5flagss5NeverOs12StaticStringV_A2HSus6UInt32VtF

  // expected-error @+2 {{#assert condition not constant}}
  // expected-note @+1 {{could not fold operation}}
  #assert(Int8(123231) > 42)

  // expected-error @+1 {{#assert condition not constant}}
  #assert(Int8(124) + 8 > 42)
  // expected-note @-1 {{could not fold operation}}
  // TODO: We don't yet handle partial_apply, so the case above isn't detected
  // as a problem yet.
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

  // expected-error @+2 {{#assert condition not constant}}
  // expected-note @+1 {{could not fold operation}}
  #assert(loops2(a: 20000) > 42)
}

//===----------------------------------------------------------------------===//
// Reduced testcase propagating substitutions around.
protocol SubstitutionsP {
  init<T: SubstitutionsP>(something: T)

  func get() -> Int
}

struct SubstitutionsX : SubstitutionsP {
  var state : Int
  init<T: SubstitutionsP>(something: T) {
    state = something.get()
  }
  func get() -> Int {
    fatalError()
  }

  func getState() -> Int {
    return state
  }
}

struct SubstitutionsY : SubstitutionsP {
  init() {}
  init<T: SubstitutionsP>(something: T) {
  }

  func get() -> Int {
    return 123
  }
}
func substitutionsF<T: SubstitutionsP>(_: T.Type) -> T {
  return T(something: SubstitutionsY())
}

func testProto() {
  // This is because our memory representation is not correct.
  // BUG: expected-error @+1 {{#assert condition not constant}}
  #assert(substitutionsF(SubstitutionsX.self).getState() == 123)
}

//===----------------------------------------------------------------------===//
// Generic thunk - partial_apply testcase.
//===----------------------------------------------------------------------===//

struct Transform<T> {
  let fn: (T) -> T
}
func double(x: Int) -> Int {
  return x + x
}

func testGenericThunk() {
  let myTransform = Transform(fn: double)

  // This is because we don't support partial application yet.
  // TODO: expected-error @+1 {{#assert condition not constant}}
  #assert(myTransform.fn(42) > 1)
  // expected-note @-1{{could not fold operation}}
}

//===----------------------------------------------------------------------===//
