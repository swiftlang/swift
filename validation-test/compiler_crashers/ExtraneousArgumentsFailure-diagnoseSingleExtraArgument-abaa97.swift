// {"kind":"typecheck","original":"51143da0","signature":"swift::constraints::ExtraneousArgumentsFailure::diagnoseSingleExtraArgument() const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"ExtraneousArgumentsFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a<b, c {
  h: [(b, b (c) -> Bool)]
  func d(e: @escaping (c, c) -> Bool) {
    h.append(
      (f
    g
    , e
