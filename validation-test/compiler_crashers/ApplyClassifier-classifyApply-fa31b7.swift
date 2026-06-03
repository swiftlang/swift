// {"kind":"typecheck","original":"1d6718e8","signature":"(anonymous namespace)::ApplyClassifier::classifyApply(swift::ApplyExpr*, (anonymous namespace)::AbstractFunction const&, swift::Expr*, swift::AnyFunctionType const*, swift::ArgumentList*, bool, bool, llvm::DenseSet<swift::Expr const*, llvm::DenseMapInfo<swift::Expr const*, void>>*)::'lambda'(swift::EffectKind)::operator()(swift::EffectKind) const","signatureAssert":"Assertion failed: (!thrownError->hasError()), function forThrows","signatureNext":"ApplyClassifier::FunctionThrowsClassifier::checkApply"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: AsyncSequence {
  typealias Element = UInt64
  let b: UInt64  async
  let c = a(b: 0).reduce(0 {
    $1
