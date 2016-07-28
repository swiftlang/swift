// RUN: %target-swift-frontend -parse -verify -suppress-argument-labels-in-types %s

// Test non-overloaded global function references.
func f1(a: Int, b: Int) -> Int { }

func testF1(a: Int, b: Int) {
  _ = f1(a: a, b: a) // okay: direct call requires argument labels
  _ = (f1)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((f1))(a: a, b: a) // okay: direct call requires argument labels

  _ = f1(a:b:)(1, 2) // compound name suppresses argument labels

  let i: Int = f1    // expected-error{{cannot convert value of type '(Int, Int) -> Int' to specified type 'Int'}}

  _ = i
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
  
  // Note: these will become ill-formed when the rest of SE-0111 is
  // implemented. For now, they check that the labels were removed by the type
  // system.
  let _: (x: Int, y: Int) -> Int = f4
  let _: (x: Double, y: Double) -> Double = f4
}
