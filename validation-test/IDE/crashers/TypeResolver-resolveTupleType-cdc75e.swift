// {"kind":"complete","original":"a4349038","signature":"(anonymous namespace)::TypeResolver::resolveTupleType(swift::TupleTypeRepr*, swift::TypeResolutionOptions)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a<b > -> () [ {
  var : (b) #^^#
