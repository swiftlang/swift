// {"kind":"typecheck","signature":"swift::constraints::FailureDiagnostic::resolveType(swift::Type, bool, bool) const","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"MissingArgumentsFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b } {
let:
  String->a = a.b
