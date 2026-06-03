// {"kind":"typecheck","original":"99a66dd3","signature":"(anonymous namespace)::ApplyClassifier::classifyApply(swift::ApplyExpr*, (anonymous namespace)::AbstractFunction const&, swift::Expr*, swift::AnyFunctionType const*, swift::ArgumentList*, bool, bool, llvm::DenseSet<swift::Expr const*, llvm::DenseMapInfo<swift::Expr const*, void>>*)::'lambda'(swift::EffectKind)::operator()(swift::EffectKind) const","signatureAssert":"Assertion failed: (!thrownError->hasError()), function forThrows","signatureNext":"CheckEffectsCoverage::checkApply"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: AsyncSequence {
  b {
  max
  }
