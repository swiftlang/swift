// {"kind":"complete","original":"b9f3b481","signature":"swift::NominalType::get(swift::NominalTypeDecl*, swift::Type, llvm::ArrayRef<swift::Type>, swift::ASTContext const&, llvm::ArrayRef<swift::ScopeRef>)","signatureAssert":"Assertion failed: ((!parent || parent->is<NominalType>() || parent->is<UnboundGenericType>()) && \"parent must be a nominal type\"), function get","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension Sendable {
  struct a
    ;
    #^^#    let b = [c a
