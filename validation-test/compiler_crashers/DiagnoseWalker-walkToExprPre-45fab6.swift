// {"kind":"typecheck","original":"d550fd71","signature":"diagSyntacticUseRestrictions(swift::Expr const*, swift::DeclContext const*, bool)::DiagnoseWalker::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @<#type#>
  #a([
    &(b = 0)
  ])
}
