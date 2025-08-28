// {"kind":"typecheck","signature":"(anonymous namespace)::AssociatedTypeInference::inferTypeWitnessesViaValueWitnesses(llvm::SetVector<swift::AssociatedTypeDecl*, llvm::SmallVector<swift::AssociatedTypeDecl*, 0u>, llvm::DenseSet<swift::AssociatedTypeDecl*, llvm::DenseMapInfo<swift::AssociatedTypeDecl*, void>>, 0u> const&)","signatureAssert":"Assertion failed: (!result.second->hasTypeParameter() || selfTy && \"We should only see unresolved type witnesses on the \" \"right-hand side of a binding when the value witness came from a \" \"protocol extension\"), function getPotentialTypeWitnessesFromRequirement"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{associatedtype b} protocol c : RandomAccessCollection struct d < e
    : a,
    f {
  g {
    struct h : c {
      typealias i = e.b subscript(i) f
