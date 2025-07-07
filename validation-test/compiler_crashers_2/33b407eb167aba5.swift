// {"signature":"(anonymous namespace)::ExprRewriter::coerceCallArguments(swift::ArgumentList*, swift::AnyFunctionType*, swift::ConcreteDeclRef, swift::ApplyExpr*, swift::constraints::ConstraintLocatorBuilder, llvm::ArrayRef<swift::AppliedPropertyWrapper>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b, each c>(body : (repeat each c)->b) { body(
