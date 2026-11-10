// {"kind":"typecheck","signature":"swift::constraints::ContextualFailure::getDiagnosticFor(swift::ContextualTypePurpose, swift::Type)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"InOutConversionFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
{ struct a func b< c >->c->() var : (inout a)->() = b(
