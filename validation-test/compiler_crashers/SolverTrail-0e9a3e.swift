// {"kind":"typecheck","original":"cbf69c52","signature":"swift::constraints::SolverTrail::~SolverTrail()","signatureAssert":"Assertion failed: (Changes.empty() && \"Trail corrupted\"), function ~SolverTrail"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  init(wrappedValue: a)
  var wrappedValue: b
}
struct c {
  @a @a var d: Int = 2
}
