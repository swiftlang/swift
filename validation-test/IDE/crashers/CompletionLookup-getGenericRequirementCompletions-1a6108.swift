// {"kind":"complete","original":"f1dae84a","signature":"swift::ide::CompletionLookup::getGenericRequirementCompletions(swift::DeclContext*, swift::SourceLoc)","signatureAssert":"Assertion failed: (!GP->getName().empty()), function addGenericTypeParamRef","signatureNext":"CodeCompletionCallbacksImpl::readyForTypeChecking"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a(some b )
where
  #^^#
