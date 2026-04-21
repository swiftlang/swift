// {"kind":"complete","original":"0435fa64","signature":"swift::ide::CompletionLookup::foundDecl(swift::ValueDecl*, swift::DeclVisibilityKind, swift::DynamicLookupInfo)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"AccessFilteringDeclConsumer::foundDecl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension Sequence where a == <#type#>[ {
  #^^#
}
