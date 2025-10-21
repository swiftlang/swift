// {"kind":"complete","original":"f8c1c082","signature":"swift::ide::PostfixCompletionCallback::Result::tryMerge(swift::ide::PostfixCompletionCallback::Result const&, swift::DeclContext*)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
typealias a = () arr = [a].append([
0])#^^#
