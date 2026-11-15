// {"kind":"emit-silgen","signature":"swift::Lowering::AbstractionPattern::initSwiftType(swift::SubstitutionMap, swift::CanGenericSignature, swift::CanType, swift::Lowering::AbstractionPattern::Kind)","signatureAssert":"Assertion failed: (signature || !origType->hasTypeParameter()), function initSwiftType","signatureNext":"getSILFunctionType"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
actor a<d> {
  var b: @convention(thin) () -> d {
    _read {
      var c =
        b ?? {
          fatalError()
        }
    }
  }
}
