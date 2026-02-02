// {"kind":"complete","signature":"swift::ide::CompletionLookup::getValueCompletionsInDeclContext(swift::SourceLoc, std::__1::function<bool (swift::ValueDecl*, swift::DeclVisibilityKind, swift::DynamicLookupInfo)>, bool, bool)"}
// RUN: %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
for () [
#^COMPLETE^#, (repeat.a
