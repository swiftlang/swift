// {"kind":"complete","original":"91164bca","signature":"createPropertyStoreOrCallSuperclassSetter(swift::AccessorDecl*, swift::Expr*, swift::AbstractStorageDecl*, (anonymous namespace)::TargetImpl, llvm::SmallVectorImpl<swift::ASTNode>&, swift::ASTContext&)","signatureAssert":"Assertion failed: (destType->getOptionalObjectType()->isEqual(value->getType())), function createPropertyStoreOrCallSuperclassSetter","signatureNext":"synthesizeAccessorBody"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a {
  lazy weak var b: AnyObject? =
    #^^#
}
