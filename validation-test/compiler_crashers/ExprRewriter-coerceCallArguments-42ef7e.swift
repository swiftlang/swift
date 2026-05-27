// {"kind":"typecheck","original":"ab804f2c","signature":"(anonymous namespace)::ExprRewriter::coerceCallArguments(swift::ArgumentList*, swift::AnyFunctionType*, swift::ConcreteDeclRef, swift::ApplyExpr*, swift::constraints::ConstraintLocatorBuilder, llvm::ArrayRef<swift::AppliedPropertyWrapper>)","signatureAssert":"Assertion failed: (substitutedBindings[paramIdx].size() == 1), function coerceCallArguments","signatureNext":"ExprRewriter::finishApply"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c: repeat each b d: (repeat each b) -> Bool) {
  d(repeat each c
  repeat each c
