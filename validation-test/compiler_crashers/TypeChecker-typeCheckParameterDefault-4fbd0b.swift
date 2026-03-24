// {"kind":"typecheck","signature":"swift::TypeChecker::typeCheckParameterDefault(swift::Expr*&, swift::DeclContext*, swift::Type, bool, bool)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"DefaultArgumentExprRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
init<a>(b: a == {
