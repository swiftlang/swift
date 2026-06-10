// {"kind":"typecheck","original":"8d13353b","signature":"swift::TypeChecker::typeCheckParameterDefault(swift::Expr*&, swift::DeclContext*, swift::Type, bool, bool)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"CallerSideDefaultArgExprRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a<b>(_: b
(b) =
  #c
class d<e {  f = #a(d
