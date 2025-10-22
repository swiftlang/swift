// {"kind":"complete","original":"029049d6","signature":"swift::ide::CompletionLookup::foundDecl(swift::ValueDecl*, swift::DeclVisibilityKind, swift::DynamicLookupInfo)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct c<d {
  var : {
    protocol a {
      struct e {
    b {
      #^^#
