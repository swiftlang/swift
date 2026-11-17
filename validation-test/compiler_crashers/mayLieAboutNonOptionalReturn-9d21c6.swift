// {"kind":"emit-silgen","original":"825f5ffc","signature":"mayLieAboutNonOptionalReturn(swift::SILModule&, swift::ValueDecl*)","signatureAssert":"Assertion failed: ((type->hasTypeParameter() || isVerbatimNullableTypeInC(M, type->getReferenceStorageReferent())) && \"property's result type is not nullable?!\"), function mayLieAboutNonOptionalReturn","signatureNext":"RValueEmitter::visitInjectIntoOptionalExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a: AnyObject {
}
extension a {
  func b() -> () -> () -> Self {
  }
}
func c<d>(
  e: a
) -> d {
  var f: a?
  let g = e.b()
  withoutActuallyEscaping(g) { h in
    f = h()()
  }
}
