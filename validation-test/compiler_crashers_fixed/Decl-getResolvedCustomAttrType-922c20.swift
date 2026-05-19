// {"kind":"typecheck","original":"65735e17","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (!canType->hasUnboundGenericType()), function computeInvertibleConformances"}
// RUN: not %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b
  struct c {
    @a    ...var d
