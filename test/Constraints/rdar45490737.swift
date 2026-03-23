// RUN: %target-typecheck-verify-swift

protocol X {}
class B : Equatable {
  static func == (lhs: B, rhs: B) -> Bool { fatalError() }
}
class C : B {}
extension C : X {}

func f<T: Equatable>(_ lhs: T, _ rhs: T) {}

// Ensure that we can call both a function that has generic parameters
// as well as one that has the generic parameters wrapped in
// Optionals.
func test1(x: (any X & B), y: C) {
  f(x, y)
}

func test2(x: (any X & B)?, y: C?) {
  f(x, y)
}

func test3(x: [any X & B], y: [C]) {
  f(x, y)
}
