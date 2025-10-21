// {"kind":"complete","signature":"swift::ide::getTypeForCompletion(swift::constraints::Solution const&, swift::ASTNode)","signatureAssert":"Assertion failed: (false && \"Expression wasn't type checked?\"), function getTypeForCompletion"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
#fileLiteral(#^COMPLETE^#
