// {"kind":"typecheck","signature":"diagSyntacticUseRestrictions(swift::Expr const*, swift::DeclContext const*, bool)::DiagnoseWalker::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (noncopyableTy->is<TupleType>() && \"will use poor wording\"), function walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{ @#b([a:
