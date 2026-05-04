// {"kind":"emit-silgen","original":"bfb51766","signature":"swift::Lowering::TypeConverter::getLoweredLocalCaptures(swift::SILDeclRef)","signatureAssert":"Assertion failed: (Captures.hasBeenComputed()), function getCaptureInfo","signatureNext":"Lowering::TypeConverter::setCaptureTypeExpansionContext"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper struct a<b> {
  init(wrappedValue: @autoclosure () -> b) {
  }
  var wrappedValue: b
}
func c(@a d: String) {
}
