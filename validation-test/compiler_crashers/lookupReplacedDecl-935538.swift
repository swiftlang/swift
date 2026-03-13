// {"kind":"typecheck","signature":"lookupReplacedDecl(swift::DeclNameRef, swift::DeclAttribute const*, swift::ValueDecl const*, llvm::SmallVectorImpl<swift::ValueDecl*>&)","signatureAssert":"Assertion failed: (declCtxt->isTypeContext()), function lookupReplacedDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{ @_dynamicReplacement(for: ) func a
