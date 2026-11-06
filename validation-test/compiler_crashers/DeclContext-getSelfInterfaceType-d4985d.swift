// {"kind":"typecheck","original":"05f65a05","signature":"swift::DeclContext::getSelfInterfaceType() const","signatureAssert":"Assertion failed: (isTypeContext()), function getSelfInterfaceType","signatureNext":"TypeChecker::checkReferencedGenericParams"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
subscript(some a )  Double
