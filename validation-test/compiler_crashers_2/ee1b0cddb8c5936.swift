// {"signature":"(anonymous namespace)::AssociatedTypeInference::inferTypeWitnessesViaValueWitnesses(llvm::SetVector<swift::AssociatedTypeDecl*, llvm::SmallVector<swift::AssociatedTypeDecl*, 0u>, llvm::DenseSet<swift::AssociatedTypeDecl*, llvm::DenseMapInfo<swift::AssociatedTypeDecl*, void>>, 0u> const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{associatedtype b} protocol c : RandomAccessCollection struct d < e
    : a,
    f {
  g {
    struct h : c {
      typealias i = e.b subscript(i) f
