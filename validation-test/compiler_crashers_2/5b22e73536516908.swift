// {"kind":"typecheck","original":"6f23f24c","signature":"diagnoseUnintendedOptionalBehavior(swift::Expr const*, swift::DeclContext const*)::UnintendedOptionalBehaviorWalker::walkToExprPre(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @<#type#>
  #a {
    &b
    0 as c
  }
}
