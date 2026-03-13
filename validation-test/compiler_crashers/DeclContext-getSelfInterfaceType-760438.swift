// {"kind":"typecheck","signature":"swift::DeclContext::getSelfInterfaceType() const","signatureAssert":"Assertion failed: (isTypeContext()), function getSelfInterfaceType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
distributed func a
