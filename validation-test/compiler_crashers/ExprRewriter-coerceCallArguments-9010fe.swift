// {"kind":"typecheck","original":"1c912312","signature":"(anonymous namespace)::ExprRewriter::coerceCallArguments(swift::ArgumentList*, swift::AnyFunctionType*, swift::ConcreteDeclRef, swift::ApplyExpr*, swift::constraints::ConstraintLocatorBuilder, llvm::ArrayRef<swift::AppliedPropertyWrapper>)","signatureAssert":"Assertion failed: (!param.isInOut()), function coerceCallArguments","signatureNext":"ExprRewriter::finishApply"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(  inout UnsafeMutableRawPointer...)
a(
