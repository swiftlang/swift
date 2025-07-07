// {"kind":"complete","signature":"swift::ide::getTypeForCompletion(swift::constraints::Solution const&, swift::ASTNode)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
#fileLiteral(#^COMPLETE^#
