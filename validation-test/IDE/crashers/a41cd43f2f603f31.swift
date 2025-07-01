// {"kind":"complete","signature":"(anonymous namespace)::CodeCompletionCallbacksImpl::completeAccessorBeginning(swift::CodeCompletionExpr*)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
case <#expression#>= { protocol a { #^COMPLETE^#
