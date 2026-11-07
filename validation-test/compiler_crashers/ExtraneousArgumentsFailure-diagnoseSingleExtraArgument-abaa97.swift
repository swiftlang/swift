// {"kind":"typecheck","original":"51143da0","signature":"swift::constraints::ExtraneousArgumentsFailure::diagnoseSingleExtraArgument() const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"ExtraneousArgumentsFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
class a<b, c {
  h: [(b, b (c) -> Bool)]
  func d(e: @escaping (c, c) -> Bool) {
    h.append(
      (f
    g
    , e
