// RUN: %target-typecheck-verify-swift -disable-fuzzy-forward-scan-trailing-closure-matching

func forwardMatch1(
  a: Int = 0, b: Int = 17, closure1: (Int) -> Int = { $0 }, c: Int = 42,
  numbers: Int..., closure2: ((Double) -> Double)? = nil,
  closure3: (String) -> String = { $0 + "!" }
) {
}

func testForwardMatch1(i: Int, d: Double, s: String) {
  forwardMatch1()

  // Matching closure1
  forwardMatch1 { _ in i }
  forwardMatch1 { _ in i } closure2: { d + $0 }
  forwardMatch1 { _ in i } closure2: { d + $0 } closure3: { $0 + ", " + s + "!" }
  forwardMatch1 { _ in i } closure3: { $0 + ", " + s + "!" }

  forwardMatch1(a: 5) { $0 + i }
  forwardMatch1(a: 5) { $0 + i } closure2: { d + $0 }
  forwardMatch1(a: 5) { $0 + i } closure2: { d + $0 } closure3: { $0 + ", " + s + "!" }
  forwardMatch1(a: 5) { $0 + i } closure3: { $0 + ", " + s + "!" }

  forwardMatch1(b: 1) { $0 + i }
  forwardMatch1(b: 1) { $0 + i } closure2: { d + $0 }
  forwardMatch1(b: 1) { $0 + i } closure2: { d + $0 } closure3: { $0 + ", " + s + "!" }
  forwardMatch1(b: 1) { $0 + i } closure3: { $0 + ", " + s + "!" }

  // Matching closure2
  forwardMatch1(closure1: { $0 + i }) { d + $0 }
  forwardMatch1(closure1: { $0 + i }, numbers: 1, 2, 3) { d + $0 }
  forwardMatch1(closure1: { $0 + i }, numbers: 1, 2, 3) { d + $0 } closure3: { $0 + ", " + s + "!" }

  // Matching closure3
  forwardMatch1(closure2: nil) { $0 + ", " + s + "!" }
}

func forwardMatchWithAutoclosure1(
  a: String = "hello",
  b: @autoclosure () -> Int = 17,
  closure1: () -> String
) { }

func testForwardMatchWithAutoclosure1(i: Int, s: String) {
  forwardMatchWithAutoclosure1 {
    print(s)
    return s
  }
  forwardMatchWithAutoclosure1(a: s) {
    print(s)
    return s
  }
  forwardMatchWithAutoclosure1(b: i) {
    print(s)
    return s
  }
  forwardMatchWithAutoclosure1(a: s, b: i) {
    print(s)
    return s
  }
}

func forwardMatchWithAutoclosure2(
  a: String = "hello",
  closure1: @autoclosure () -> (() -> String),
  closure2: () -> Int = { 5 }
) { }

func testForwardMatchWithAutoclosure2(i: Int, s: String) {
  forwardMatchWithAutoclosure2 {
    print(s)
    return s
  }
  forwardMatchWithAutoclosure2(a: s) {
    print(s)
    return s
  }

  forwardMatchWithAutoclosure2(a: s, closure1: { s }) {
    print(i)
    return i
  }
}

// Match a closure parameter to a variadic parameter.
func acceptsVariadicClosureParameter(closures: ((Int, Int) -> Int)...) {
}

func testVariadicClosureParameter() {
  acceptsVariadicClosureParameter { x, y in x % y }
  acceptsVariadicClosureParameter() { x, y in x % y }
  acceptsVariadicClosureParameter(closures: +, -, *, /) { x, y in x % y }
}
