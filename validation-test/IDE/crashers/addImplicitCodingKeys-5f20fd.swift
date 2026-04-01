// {"kind":"complete","languageMode":6,"original":"0cebf46e","signature":"addImplicitCodingKeys(swift::NominalTypeDecl*, llvm::SmallVectorImpl<swift::Identifier>&, swift::Identifier)","signatureAssert":"Assertion failed: (target->lookupDirect(DeclName(codingKeysEnumIdentifier)).empty()), function addImplicitCodingKeys","signatureNext":"canSynthesize"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
@propertyWrapper struct a: Codable {
  wrappedValue: b  init
  @a var c
}
{
  #^^#
  @a var d
