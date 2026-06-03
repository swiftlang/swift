// {"kind":"typecheck","original":"3f6f4f7f","signature":"(anonymous namespace)::ExprRewriter::coerceCallArguments(swift::ArgumentList*, swift::AnyFunctionType*, swift::ConcreteDeclRef, swift::ApplyExpr*, swift::constraints::ConstraintLocatorBuilder, llvm::ArrayRef<swift::AppliedPropertyWrapper>)","signatureNext":"ExprRewriter::finishApply"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b, each c {
  d: (repeat each c) -> b
  func e {
    d(
