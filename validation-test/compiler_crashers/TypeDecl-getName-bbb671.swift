// {"kind":"typecheck","original":"ad4e7171","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (isTypeContext()), function getSelfInterfaceType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension {
a {
subscript<b>->b
