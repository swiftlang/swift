// {"kind":"typecheck","original":"9f723d8d","signature":"swift::TypeChecker::typeCheckParameterDefault(swift::Expr*&, swift::DeclContext*, swift::Type, bool, bool)","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"CallerSideDefaultArgExprRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a<b {
  init(c: b a  = nil
}
func d<c {
  a(c: c
