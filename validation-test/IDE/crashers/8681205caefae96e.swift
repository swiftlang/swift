// {"kind":"complete","signature":"swift::ide::CompletionLookup::getPostfixKeywordCompletions(swift::Type, swift::Expr*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
func a(b: (() -> Float)!) {
b()#^COMPLETE^#
