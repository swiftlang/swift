// {"kind":"complete","languageMode":6,"original":"6f1fec73","signature":"diagSyntacticUseRestrictions(swift::Expr const*, swift::DeclContext const*, bool, bool)::DiagnoseWalker::checkUseOfMetaTypeName(swift::Expr*)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"DiagnoseWalker::walkToExprPre"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
class a {
  var b = {
    let c = String
    func d() -> #^^#
  }
}
