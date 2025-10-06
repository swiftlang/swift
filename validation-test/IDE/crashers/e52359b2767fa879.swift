// {"kind":"complete","original":"108dc1e5","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {
a {
  typealias b = ()?
  let c = b?? ()
  #^^#
