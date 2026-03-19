// {"kind":"complete","original":"e0b2d8fc","signature":"swift::ide::getTypeForCompletion(swift::constraints::Solution const&, swift::ASTNode)","signatureAssert":"Assertion failed: (false && \"Expression wasn't type checked?\"), function getTypeForCompletion","signatureNext":"getClosureActorIsolation"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a
  #if {
    @b(a  #^^# = {
