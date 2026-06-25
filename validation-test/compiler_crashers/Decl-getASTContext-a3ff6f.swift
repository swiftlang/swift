// {"kind":"typecheck","original":"6a0f5299","signature":"swift::Decl::getASTContext() const","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null GenericSignature!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(extension conformances : repeat each a) macro b < each a>()
