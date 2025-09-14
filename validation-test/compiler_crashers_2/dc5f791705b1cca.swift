// {"kind":"typecheck","signature":"swift::constraints::TypeVarRefCollector::walkToStmtPre(swift::Stmt*)","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  lazy var a =
    if <#expression#> {
      return
    }
  a
}
