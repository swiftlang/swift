// {"kind":"complete","original":"8a404a63","signature":"openTypeParameter(swift::constraints::ConstraintSystem&, swift::Type, swift::GenericEnvironment*, llvm::DenseMap<swift::SubstitutableType*, swift::TypeVariableType*, llvm::DenseMapInfo<swift::SubstitutableType*, void>, llvm::detail::DenseMapPair<swift::SubstitutableType*, swift::TypeVariableType*>>&)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b: Numeric
}
func c<d: a>() -> d.b where d == <#type#> {
  #^^#
}
