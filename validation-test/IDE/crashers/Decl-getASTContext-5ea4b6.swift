// {"kind":"complete","original":"7f3d7fb6","signature":"swift::Decl::getASTContext() const","signatureAssert":"Assertion failed: (!isSpecial() && \"Cannot retrieve identifier from special names\"), function getIdentifier"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@attached(peer prefixed(a)) macro b()
@b init()
@attached(extension conformances: c) macro d () =
#^^#
