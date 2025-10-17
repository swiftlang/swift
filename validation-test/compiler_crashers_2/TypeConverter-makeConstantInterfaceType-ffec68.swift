// {"kind":"emit-silgen","original":"91fe640b","signature":"swift::Lowering::TypeConverter::makeConstantInterfaceType(swift::SILDeclRef)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper struct a<b> {
  init(wrappedValue: b...) {
  }
  var wrappedValue: b
}
class c {
  @a var d = 2
}
