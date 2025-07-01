// {"kind":"complete","signature":"swift::TypeChecker::resolveDeclRefExpr(swift::UnresolvedDeclRefExpr*, swift::DeclContext*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
extension <#type#> {
  var
  a = "title \( init String * 0).General\(String#^COMPLETE^#0))"
}
