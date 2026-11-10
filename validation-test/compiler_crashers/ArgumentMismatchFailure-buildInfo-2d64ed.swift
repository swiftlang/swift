// {"kind":"typecheck","signature":"swift::constraints::ArgumentMismatchFailure::buildInfo(swift::constraints::Solution const&, swift::constraints::ConstraintLocator*)","signatureNext":"ArgumentMismatchFailure::create"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
    callAsFunction( ())
  {
    a {
