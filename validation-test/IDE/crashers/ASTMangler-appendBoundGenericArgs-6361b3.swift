// {"kind":"complete","original":"48e2c440","signature":"swift::Mangle::ASTMangler::appendBoundGenericArgs(swift::DeclContext*, swift::GenericSignature, swift::SubstitutionMap, bool&, swift::ValueDecl const*)","signatureNext":"Mangle::ASTMangler::appendOpaqueTypeArchetype"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {a ->
  Set<some Hashable> {
    #^^#
