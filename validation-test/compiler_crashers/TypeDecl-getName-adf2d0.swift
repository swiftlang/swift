// {"kind":"typecheck","original":"178d1b27","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (declCtxt->isTypeContext()), function lookupReplacedDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension {
a { @_dynamicReplacement(for: ) func b
