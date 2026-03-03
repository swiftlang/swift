// {"kind":"typecheck","original":"1f8c002c","signature":"diagSyntacticUseRestrictions(swift::Expr const*, swift::DeclContext const*, bool, bool)::DiagnoseWalker::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (noncopyableTy->is<TupleType>() && \"will use poor wording\"), function walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @ #a((
