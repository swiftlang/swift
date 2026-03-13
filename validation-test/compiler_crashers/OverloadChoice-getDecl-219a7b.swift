// {"kind":"typecheck","original":"58dc5134","signature":"swift::constraints::OverloadChoice::getDecl(swift::Type, swift::ValueDecl*, swift::FunctionRefInfo)","signatureAssert":"Assertion failed: (!base->hasTypeParameter()), function getDecl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b> {
  c {
  struct d {
  typealias e<b> = a @e var f =
  }
