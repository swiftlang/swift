// {"kind":"emit-silgen","original":"6e2be603","signature":"(anonymous namespace)::Transform::transform(swift::Lowering::ManagedValue, swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","signatureNext":"Lowering::SILGenFunction::emitTransformedValue"}
// RUN: %target-swift-frontend -emit-silgen -verify %s
protocol a {
  static var b: UInt32 {
    get
  }
}
func c(d: [a.Type]) {
  d.map(\.b) // expected-error {{key path cannot refer to static member 'b' of protocol type 'a'}}
}
