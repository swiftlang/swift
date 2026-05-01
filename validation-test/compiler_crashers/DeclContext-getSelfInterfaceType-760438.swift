// {"kind":"typecheck","signature":"swift::DeclContext::getSelfInterfaceType() const","signatureAssert":"Assertion failed: (isTypeContext()), function getSelfInterfaceType","signatureNext":"DeclContext::getSelfTypeInContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
distributed func a
