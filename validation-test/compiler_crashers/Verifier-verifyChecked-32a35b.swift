// {"kind":"typecheck","signature":"(anonymous namespace)::Verifier::verifyChecked(swift::AbstractClosureExpr*)","signatureNext":"Verifier::dispatchVisitPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: StringInterpolationProtocol {
  init(literalCapacity: Int, interpolationCount: Int) {
  }
  func appendLiteral(_: String) {
  }
  func appendInterpolation(_: @autoclosure () -> UInt16) {
  }
}
struct b: ExpressibleByStringInterpolation {
  init(stringInterpolation: a) {
  }
  init(stringLiteral: String) {
  }
}
func c(d: b = "\(3)") {
}
