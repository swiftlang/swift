// {"kind":"complete","signature":"swift::ide::PostfixCompletionCallback::Result::tryMerge(swift::ide::PostfixCompletionCallback::Result const&, swift::DeclContext*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
Int { switch { case Optional.some()#^COMPLETE^#
