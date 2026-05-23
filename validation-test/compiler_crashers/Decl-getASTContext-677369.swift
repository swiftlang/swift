// {"kind":"typecheck","original":"c2b80c1d","signature":"swift::Decl::getASTContext() const","signatureAssert":"Assertion failed: (isConsistentAboutIsolation(info, params)), function GenericFunctionType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(extension conformances
: a) macro b
@freestanding(declaration arbitrary) macro c<d>(isolated())
#c
