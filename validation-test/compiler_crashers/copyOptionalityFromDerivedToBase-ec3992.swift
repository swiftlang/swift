// {"kind":"emit-silgen","signature":"copyOptionalityFromDerivedToBase(swift::Lowering::TypeConverter&, swift::CanType, swift::CanType)","signatureAssert":"Assertion failed: (derivedParam.getParameterFlags() == baseParam.getParameterFlags()), function copyOptionalityFromDerivedToBase","signatureNext":"Lowering::TypeConverter::getConstantOverrideInfo"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
class a {
  func b(c: (Int) -> Void) {
  }
}
class d: a {
  override func b(c: (Int...) -> Void) {
  }
}
