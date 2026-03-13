// {"kind":"complete","signature":"swift::ide::CodeCompletionResultBuilder::takeResult()"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a { extension b { struct c { struct d<e{ f { d#^COMPLETE^#
