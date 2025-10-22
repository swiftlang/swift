// {"kind":"complete","original":"8b824012","signature":"swift::Mangle::ASTMangler::appendBoundGenericArgs(swift::DeclContext*, swift::GenericSignature, swift::SubstitutionMap, bool&, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  struct b {
    var c: (some Any, Int) {
      #^^#
    }
  }
}
