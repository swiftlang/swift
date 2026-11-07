// {"kind":"typecheck","signature":"swift::Decl::getLoc(bool) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"AvailabilityContext::forDeclSignature"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@_typeEraser(a) protocol b dynamic func c->some b {
  d
