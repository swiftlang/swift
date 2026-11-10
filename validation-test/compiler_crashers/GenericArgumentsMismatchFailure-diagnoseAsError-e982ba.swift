// {"kind":"typecheck","original":"c920d4b8","signature":"swift::constraints::GenericArgumentsMismatchFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"GenericArgumentsMismatch::coalesceAndDiagnose"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b {
  c: (repeat [each b])
  func d(e: repeat each b) {
    func g<f>(_: [f] f )
    repeat g(c.element
    e
