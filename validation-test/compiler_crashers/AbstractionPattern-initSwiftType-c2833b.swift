// {"kind":"emit-silgen","original":"f04bb3f5","signature":"swift::Lowering::AbstractionPattern::initSwiftType(swift::SubstitutionMap, swift::CanGenericSignature, swift::CanType, swift::Lowering::AbstractionPattern::Kind)","signatureAssert":"Assertion failed: (signature || !origType->hasTypeParameter()), function initSwiftType","signatureNext":"Lowering::TypeConverter::getTypeLowering"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
class a<b> {
  func c(g: (Self) -> Void) {
    withoutActuallyEscaping(g) {
      d in
      let e = { f in
        {
          d(f)
        }
      }
    }
  }
}
