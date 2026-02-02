// {"kind":"complete","original":"3de07705","signature":"swift::ide::PostfixCompletionCallback::Result::tryMerge(swift::ide::PostfixCompletionCallback::Result const&, swift::DeclContext*)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
2 ?? .map {} #^^#?? a
