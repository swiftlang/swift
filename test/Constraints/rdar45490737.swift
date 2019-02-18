// RUN: %target-typecheck-verify-swift

protocol X {}
class B : Equatable {
  static func == (lhs: B, rhs: B) -> Bool { fatalError() }
}
class C : B {}
extension C : X {}

func f<T: Equatable>(_ lhs: T, _ rhs: T) {}

extension Optional where Wrapped : Equatable {
  static func f(_ lhs: Wrapped?, _ rhs: Wrapped?) {}
}

// Ensure that we can call both a function that has generic parameters
// as well as one that has the generic parameters wrapped in
// Optionals.
func test(x: (X & B)?, y: C?) {
  f(x, y)
  Optional.f(x, y)
}
