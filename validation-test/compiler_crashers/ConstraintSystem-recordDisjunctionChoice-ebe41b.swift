// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordDisjunctionChoice(swift::constraints::ConstraintLocator*, unsigned int)","signatureAssert":"Assertion failed: (inserted), function recordDisjunctionChoice"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  init! ( b: Int, () -> Int = {
    }
() -> Int = a(b : 1) {
