// {"kind":"emit-silgen","original":"6e2be603","signature":"(anonymous namespace)::Transform::transform(swift::Lowering::ManagedValue, swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","signatureNext":"Lowering::SILGenFunction::emitTransformedValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  static var b: UInt32 {
    get
  }
}
func c(d: [a.Type]) {
  d.map(\.b)
}
