// {"kind":"complete","original":"62efa6bf","signature":"(anonymous namespace)::AssociatedTypeInference::inferTypeWitnessesViaValueWitnesses(llvm::SetVector<swift::AssociatedTypeDecl*, llvm::SmallVector<swift::AssociatedTypeDecl*, 0u>, llvm::DenseSet<swift::AssociatedTypeDecl*, llvm::DenseMapInfo<swift::AssociatedTypeDecl*, void>>, 0u> const&)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a: CaseIterable { @available(*) case allCases
#^^#
