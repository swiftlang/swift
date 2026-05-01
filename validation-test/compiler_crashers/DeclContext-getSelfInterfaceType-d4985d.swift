// {"kind":"typecheck","original":"05f65a05","signature":"swift::DeclContext::getSelfInterfaceType() const","signatureAssert":"Assertion failed: (isTypeContext()), function getSelfInterfaceType","signatureNext":"TypeChecker::checkReferencedGenericParams"}
// RUN: not --crash %target-swift-frontend -typecheck %s
subscript(some a )  Double
