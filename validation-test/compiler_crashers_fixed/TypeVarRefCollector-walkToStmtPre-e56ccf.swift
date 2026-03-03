// {"kind":"typecheck","signature":"swift::constraints::TypeVarRefCollector::walkToStmtPre(swift::Stmt*)","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: not %target-swift-frontend -typecheck %s
{
  switch if <#expression#> {
    return
  }
  {
  case let !a where a:
  }
}
