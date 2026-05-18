// {"kind":"typecheck","original":"23c4bafe","signature":"swift::Decl::getASTContext() const","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@attached(extension conformances : () -> a) macro b < a>()
