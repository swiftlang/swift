// {"kind":"complete","original":"102f2832","signature":"(anonymous namespace)::AvailabilityScopeBuilder::refinementSourceRangeForDecl(swift::Decl*)","signatureAssert":"Assertion failed: (decl->getSourceRange().isValid()), function refinementSourceRangeForDecl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
let a {
  @#^^# @available(swift    4.2) <#declaration#>
}
