// {"kind":"complete","signature":"(anonymous namespace)::CodeCompletionCallbacksImpl::completeNominalMemberBeginning(llvm::SmallVectorImpl<llvm::StringRef>&, swift::SourceLoc)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
case = switch { case #^COMPLETE^#<#expression#>
