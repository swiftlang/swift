// {"kind":"typecheck","signature":"(anonymous namespace)::ExprRewriter::finishApply(swift::ApplyExpr*, swift::Type, swift::constraints::ConstraintLocatorBuilder, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(b : ()->Void c : _->Void) { withoutActuallyEscaping(b do : c
