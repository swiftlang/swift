// {"kind":"typecheck","original":"c2b80c1d","signature":"swift::Decl::getASTContext() const","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function GenericFunctionType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(extension conformances
: a) macro b
@freestanding(declaration arbitrary) macro c<d>(isolated())
#c
