// {"kind":"complete","original":"79064912","signature":"swift::TypeBase::getReducedShape()","signatureAssert":"Assertion failed: (!isTypeVariableOrMember()), function getReducedShape","signatureNext":"PackExpansionType::getReducedShape"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a<each b>(
c: repeat each b) -> (repeat each b) {
(repeat each c -())#^^#
