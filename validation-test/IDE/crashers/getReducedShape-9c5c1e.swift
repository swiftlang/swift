// {"kind":"complete","original":"0a5ea3d7","signature":"Assertion failed: (!isTypeVariableOrMember()), function getReducedShape","signatureAssert":"Assertion failed: (!isTypeVariableOrMember()), function getReducedShape"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a<each b>(repeat sending each b) -> () -> Void {
  #^^#
