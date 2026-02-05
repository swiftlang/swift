// {"kind":"complete","original":"19e34c4f","signature":"swift::TypeChecker::typeCheckCondition(swift::Expr*&, swift::DeclContext*)","signatureAssert":"Assertion failed: (!expr->getType() || isa<OpaqueValueExpr>(expr) && \"the bool condition is already type checked\"), function typeCheckCondition"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  let a = {
    switch a {
    case let b where #c:
      #^^#
    }
  }
}
