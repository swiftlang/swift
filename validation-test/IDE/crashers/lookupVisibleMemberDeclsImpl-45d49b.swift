// {"kind":"complete","original":"5ebad56a","signature":"lookupVisibleMemberDeclsImpl(swift::Type, swift::VisibleDeclConsumer&, swift::DeclContext const*, (anonymous namespace)::LookupState, swift::DeclVisibilityKind, llvm::SmallPtrSet<swift::TypeDecl const*, 8u>&)","signatureAssert":"Assertion failed: (!BaseTy->hasTypeParameter()), function lookupVisibleMemberDeclsImpl","signatureNext":"lookupVisibleMemberAndDynamicMemberDecls"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b> {
  class c {
    func d() {
      @dynamicMemberLookup struct e {
        subscript<f>(dynamicMember g: KeyPath<c, f>) -> c {
          #^^#
        }
      }
    }
  }
}
