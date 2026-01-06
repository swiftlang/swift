// {"kind":"typecheck","original":"1c912312","signature":"(anonymous namespace)::ExprRewriter::coerceCallArguments(swift::ArgumentList*, swift::AnyFunctionType*, swift::ConcreteDeclRef, swift::ApplyExpr*, swift::constraints::ConstraintLocatorBuilder, llvm::ArrayRef<swift::AppliedPropertyWrapper>)","signatureAssert":"Assertion failed: (!param.isInOut()), function coerceCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(  inout UnsafeMutableRawPointer...)
a(
