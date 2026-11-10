// {"kind":"typecheck","original":"1806b993","signature":"swift::constraints::ArgumentMismatchFailure::buildInfo(swift::constraints::Solution const&, swift::constraints::ConstraintLocator*)","signatureNext":"ArgumentMismatchFailure::create"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b >(repeat inout each b) -> (repeat each b
( ;
let c 0 = a( 12.3 + 3
