// {"kind":"typecheck","original":"d2c2e41b","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit"}
// RUN: not %target-swift-frontend -typecheck %s
extension {
a { let b: UInt8 c(&b
