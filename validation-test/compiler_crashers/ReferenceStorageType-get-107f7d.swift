// {"kind":"typecheck","original":"4c121ec9","signature":"swift::ReferenceStorageType::get(swift::Type, swift::ReferenceOwnership, swift::ASTContext const&)","signatureAssert":"Assertion failed: (!T->hasTypeVariableOrHole()), function get","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b: AnyObject> {
  unowned var wrappedValue: b
}
{
  @a var AnyObject
}
