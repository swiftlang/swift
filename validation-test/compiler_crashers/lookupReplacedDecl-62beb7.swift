// {"kind":"typecheck","original":"9b1a2c5c","signature":"lookupReplacedDecl(swift::DeclNameRef, swift::DeclAttribute const*, swift::ValueDecl const*, llvm::SmallVectorImpl<swift::ValueDecl*>&)","signatureAssert":"Assertion failed: (declCtxt->isTypeContext()), function lookupReplacedDecl","signatureNext":"findReplacedStorageDecl"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  b { @_dynamicReplacement(for: ) var c
  }
