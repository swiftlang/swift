// {"kind":"complete","original":"f1dae84a","signature":"swift::ide::CompletionLookup::addGenericTypeParamRef(swift::GenericTypeParamDecl const*, swift::DeclVisibilityKind, swift::DynamicLookupInfo)","signatureAssert":"Assertion failed: (!GP->getName().empty()), function addGenericTypeParamRef"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a(some b )
where
  #^^#
