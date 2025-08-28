// {"kind":"complete","signature":"swift::TypeChecker::resolveDeclRefExpr(swift::UnresolvedDeclRefExpr*, swift::DeclContext*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension <#type#> {
  var
  a = "title \( init String * 0).General\(String#^COMPLETE^#0))"
}
