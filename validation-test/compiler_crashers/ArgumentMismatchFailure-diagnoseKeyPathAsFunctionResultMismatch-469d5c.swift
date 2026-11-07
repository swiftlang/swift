// {"kind":"typecheck","original":"4a332ff6","signature":"swift::constraints::ArgumentMismatchFailure::diagnoseKeyPathAsFunctionResultMismatch() const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"ArgumentMismatchFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(repeat each b) where repeat each b: AnyKeyPath
a(\c
d
, .e
