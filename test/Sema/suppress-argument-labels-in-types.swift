// RUN: %target-swift-frontend -module-name TestModule -typecheck -verify %s

// Test non-overloaded global function references.
func f1(a: Int, b: Int) -> Int { }

func testF1(a: Int, b: Int) {
  _ = f1(a: a, b: a) // okay: direct call requires argument labels
  _ = (f1)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((f1))(a: a, b: a) // okay: direct call requires argument labels

  _ = f1(a:b:)(1, 2) // compound name suppresses argument labels

  let _: Int = f1    // expected-error{{cannot convert value of type '(Int, Int) -> Int' to specified type 'Int'}}
}

// Test multiple levels of currying.
func f2(a: Int, b: Int) -> (Int) -> (Int) -> Int { }

func testF2(a: Int, b: Int) {
  _ = f2(a: a, b: b)(a) // okay: direct call requires argument labels
  _ = f2(a: a, b: b)(a)(b) // okay: direct call requires argument labels
}

// Check throwing functions.
func f3(a: Int, b: Int) throws -> Int { }

func testF3(a: Int, b: Int) {
  do {
  _ = try f3(a: a, b: a) // okay: direct call requires argument labels
  _ = try (f3)(a: a, b: a) // okay: direct call requires argument labels
  _ = try ((f3))(a: a, b: a) // okay: direct call requires argument labels

  _ = try f3(a:b:)(1, 2) // compound name suppresses argument labels

    let i: Int = f3    // expected-error{{cannot convert value of type '(Int, Int) throws -> Int' to specified type 'Int'}}

    _ = i
  } catch {
  } 
}

// Test overloaded global function references.
func f4(a: Int, b: Int) -> Int { }
func f4(c: Double, d: Double) -> Double { }

func testF4(a: Int, b: Int, c: Double, d: Double) {
  _ = f4(a: a, b: a) // okay: direct call requires argument labels
  _ = (f4)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((f4))(a: a, b: a) // okay: direct call requires argument labels
  _ = f4(c: c, d: d) // okay: direct call requires argument labels
  _ = (f4)(c: c, d: d) // okay: direct call requires argument labels
  _ = ((f4))(c: c, d: d) // okay: direct call requires argument labels

  _ = f4(a:b:)(1, 2) // compound name suppresses argument labels
  _ = f4(c:d:)(1.5, 2.5) // compound name suppresses argument labels

  let _: (Int, Int) -> Int = f4
  let _: (Double, Double) -> Double = f4
  
  let _: (_ x: Int, _ y: Int) -> Int = f4
  let _: (_ x: Double, _ y: Double) -> Double = f4
}

// Test module-qualified function references.
func testModuleQualifiedRef(a: Int, b: Int, c: Double, d: Double) {
  _ = TestModule.f1(a: a, b: a) // okay: direct call requires argument labels
  _ = (TestModule.f1)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((TestModule.f1))(a: a, b: a) // okay: direct call requires argument labels

  _ = TestModule.f1(a:b:)(1, 2) // compound name suppresses argument labels

  let _: Int = TestModule.f1    // expected-error{{cannot convert value of type '(Int, Int) -> Int' to specified type 'Int'}}

  _ = TestModule.f4(a: a, b: a) // okay: direct call requires argument labels
  _ = (TestModule.f4)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((TestModule.f4))(a: a, b: a) // okay: direct call requires argument labels
  _ = TestModule.f4(c: c, d: d) // okay: direct call requires argument labels
  _ = (TestModule.f4)(c: c, d: d) // okay: direct call requires argument labels
  _ = ((TestModule.f4))(c: c, d: d) // okay: direct call requires argument labels

  _ = TestModule.f4(a:b:)(1, 2) // compound name suppresses argument labels
  _ = TestModule.f4(c:d:)(1.5, 2.5) // compound name suppresses argument labels

  let _: (Int, Int) -> Int = TestModule.f4
  let _: (Double, Double) -> Double = TestModule.f4

  let _: (_ x: Int, _ y: Int) -> Int = TestModule.f4
  let _: (_ x: Double, _ y: Double) -> Double = TestModule.f4
}

// Test member references.
struct S0 {
  init(a: Int, b: Int) { }

  func f1(a: Int, b: Int) -> Int { }
  func f2(a: Int, b: Int) -> (Int) -> (Int) -> Int { }

  func f4(a: Int, b: Int) -> Int { }
  func f4(c: Double, d: Double) -> Double { }

  subscript (a a: Int, b b: Int) -> Int {
    get { }
    set { }
  }
}

func testS0Methods(s0: S0, a: Int, b: Int, c: Double, d: Double) {
  _ = s0.f1(a: a, b: a) // okay: direct call requires argument labels
  _ = (s0.f1)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((s0.f1))(a: a, b: a) // okay: direct call requires argument labels

  _ = s0.f1(a:b:)(a, b) // compound name suppresses argument labels

  let _: Int = s0.f1    // expected-error{{cannot convert value of type '(Int, Int) -> Int' to specified type 'Int'}}

  _ = s0.f2(a: a, b: b)(a) // okay: direct call requires argument labels
  _ = s0.f2(a: a, b: b)(a)(b) // okay: direct call requires argument labels

  _ = s0.f4(a: a, b: a) // okay: direct call requires argument labels
  _ = (s0.f4)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((s0.f4))(a: a, b: a) // okay: direct call requires argument labels
  _ = s0.f4(c: c, d: d) // okay: direct call requires argument labels
  _ = (s0.f4)(c: c, d: d) // okay: direct call requires argument labels
  _ = ((s0.f4))(c: c, d: d) // okay: direct call requires argument labels

  _ = s0.f4(a:b:)(1, 2) // compound name suppresses argument labels
  _ = s0.f4(c:d:)(1.5, 2.5) // compound name suppresses argument labels

  let _: (Int, Int) -> Int = s0.f4
  let _: (Double, Double) -> Double = s0.f4

  let _: (_ x: Int, _ y: Int) -> Int = s0.f4
  let _: (_ x: Double, _ y: Double) -> Double = s0.f4
}

// Curried instance methods.
func testS0CurriedInstanceMethods(s0: S0, a: Int, b: Int) {
  _ = S0.f1(s0)(a: a, b: a)  // okay: direct call requires argument labels
  _ = (S0.f1)(s0)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((S0.f1))(s0)(a: a, b: a) // okay: direct call requires argument labels

  _ = S0.f1(a:b:)(s0)(a, b) // compound name suppresses argument labels

  let _: Int = S0.f1    // expected-error{{cannot convert value of type '(S0) -> (Int, Int) -> Int' to specified type 'Int'}}
  let f1OneLevel = S0.f1(s0)
  let _: Int = f1OneLevel // expected-error{{cannot convert value of type '(Int, Int) -> Int' to specified type 'Int'}}
}

// Initializers.
func testS0Initializers(s0: S0, a: Int, b: Int) {
  let _ = S0(a: a, b: b)  // okay: direct call requires argument labels
  let _ = S0.init(a: a, b: b)  // okay: direct call requires argument labels

  let _ = S0.init(a:b:)(a, b) // compound name suppresses argument labels

  // Curried references to the initializer drop argument labels.
  let s0c1 = S0.init
  let _: Int = s0c1 // expected-error{{cannot convert value of type '(Int, Int) -> S0' to specified type 'Int'}}

  let s0c2 = S0.init(a:b:)
  let _: Int = s0c2 // expected-error{{cannot convert value of type '(Int, Int) -> S0' to specified type 'Int'}}
}

struct S1 {
  subscript (i: Int) -> Int {
    get { }
    nonmutating set { }
  }
}

func testS0Subscripts(s0: S0, s1: S1, a: Int, b: Int) {
  let _ = s0[a: a, b: b]

  var s0Var = s0
  s0Var[a: a, b: b] = a

  let _ = s1[a]
  s1[a] = b
}

struct S2 {
  let s1: S1
}

func testS1Subscripts(s2: S2, a: Int) {
  let _ = s2.s1[a]
  s2.s1[a] = a
}

// Test delegating initialization.
struct S3 {
  init(a: Int, b: Int) { }

  init(all: Int) {
    self.init(a:b:)(all, all)
  }
}


// Check lazy inference, which broke in amusing ways with SE-0111.
class C0 {
  lazy var a = 32
}

// Check diagnostics changes.
let _ = min(Int(3), Float(2.5)) // expected-error{{cannot convert value of type 'Float' to expected argument type 'Int'}}

// SR-11429
func testIntermediateCoercions() {
  _ = (f1 as (Int, Int) -> Int)(a: 0, b: 1) // expected-error {{extraneous argument labels 'a:b:' in call}}
  _ = (f1 as (Int, Int) -> Int)(0, 1)

  typealias Magic<T> = T
  _ = (f1 as Magic)(a: 0, b: 1) // expected-error {{extraneous argument labels 'a:b:' in call}}
  _ = (f1 as Magic)(0, 1)

  _ = (f4 as (Int, Int) -> Int)(0, 0)
  _ = (f4 as (Double, Double) -> Double)(0, 0)

  func iuoReturning() -> Int! {}
  _ = (iuoReturning as () -> Int?)()
  _ = (iuoReturning as Magic)()
  _ = (iuoReturning as () -> Int)() // expected-error {{cannot convert value of type '() -> Int?' to type '() -> Int' in coercion}}
}
