// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"643ca3e9","signature":"swift::checkConformance(swift::Type, swift::ProtocolDecl*, bool)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"must take a contextual type. if you really are ok with an \" \"indefinite answer (and usually YOU ARE NOT), then consider whether \" \"you really, definitely are ok with an indefinite answer, and \" \"use `checkConformanceWithoutContext` instead\"), function checkConformance","signatureNext":"TypeBase::isSendableType"}
// RUN: not %target-swift-frontend -typecheck -language-mode 6 %s
@propertyWrapper struct a<b> {
  var wrappedValue: b
}
@MainActor struct c<b> {
  @a var d: b
}
