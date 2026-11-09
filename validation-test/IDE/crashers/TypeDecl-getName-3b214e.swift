// {"kind":"complete","original":"b82dd70a","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (inserted), function recordArgumentList"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {
  a = #externalMacro!(#^^#
