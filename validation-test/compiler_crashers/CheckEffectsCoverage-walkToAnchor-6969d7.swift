// {"kind":"typecheck","signature":"(anonymous namespace)::CheckEffectsCoverage::walkToAnchor(swift::Expr*, llvm::DenseMap<swift::Expr*, swift::Expr*, llvm::DenseMapInfo<swift::Expr*, void>, llvm::detail::DenseMapPair<swift::Expr*, swift::Expr*>>&, swift::InterpolatedStringLiteralExpr*, bool, swift::EffectKind)","signatureAssert":"Assertion failed: (parent == nullptr && \"Expected to be at top of expression\"), function walkToAnchor","signatureNext":"CheckEffectsCoverage::checkEffectSite"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a async {
  "\(_ = a())"
