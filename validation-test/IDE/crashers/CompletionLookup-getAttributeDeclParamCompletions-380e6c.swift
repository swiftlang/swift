// {"kind":"complete","original":"292663c4","signature":"swift::ide::CompletionLookup::getAttributeDeclParamCompletions(swift::ParameterizedDeclAttributeKind, int, bool)","signatureAssert":"Assertion failed: (Sig), function lookupVisibleMemberDecls","signatureNext":"CodeCompletionCallbacksImpl::readyForTypeChecking"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b> {
  @storageRestrictions(
    initializes:
      #^^#)<#declaration#>
}
