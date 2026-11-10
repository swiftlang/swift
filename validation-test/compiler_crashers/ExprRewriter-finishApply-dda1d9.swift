// {"kind":"typecheck","original":"0fdef466","signature":"(anonymous namespace)::ExprRewriter::finishApply(swift::ApplyExpr*, swift::Type, swift::constraints::ConstraintLocatorBuilder, swift::constraints::ConstraintLocatorBuilder)::$_1::operator()(swift::ApplyExpr*, swift::ConcreteDeclRef, swift::Type) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ExprRewriter::visitApplyExpr"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a func b(c : a) { _openExistential(c do: b
