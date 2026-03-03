// {"kind":"typecheck","original":"7f0c85e8","signature":"diagSyntacticUseRestrictions(swift::Expr const*, swift::DeclContext const*, bool)::DiagnoseWalker::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not %target-swift-frontend -typecheck %s
class a<b, c>: ExpressibleByDictionaryLiteral {
  typealias Key = b
  typealias Value = c
  func d<b, c>(a    <b, c>) {
    {
      d(["": 2
