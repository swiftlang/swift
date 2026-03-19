// {"kind":"typecheck","original":"d1c309e0","signature":"swift::constraints::ConstraintSystem::recordMatchCallArgumentResult(swift::constraints::ConstraintLocator*, swift::constraints::MatchCallArgumentResult)","signatureAssert":"Assertion failed: (inserted), function recordMatchCallArgumentResult","signatureNext":"matchCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: ExpressibleByIntegerLiteral {
  init() {
    init(b      c) {
    }
  }
  func callAsFunction()
}
