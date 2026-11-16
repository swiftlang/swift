// {"kind":"complete","original":"32bed0bb","signature":"lookupReplacedDecl(swift::DeclNameRef, swift::DeclAttribute const*, swift::ValueDecl const*, llvm::SmallVectorImpl<swift::ValueDecl*>&)","signatureAssert":"Assertion failed: (declCtxt->isTypeContext()), function lookupReplacedDecl","signatureNext":"findSimilarAccessor"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  @_dynamicReplacement(for:) subscript() -> <#type#> {
    #^^#
  }
}
