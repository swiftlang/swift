// {"frontendArgs":["-emit-silgen"],"kind":"custom","signature":"(anonymous namespace)::Transform::transform(swift::Lowering::ManagedValue, swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","signatureNext":"Lowering::SILGenFunction::emitTransformedValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper struct a<b> {
  init(wrappedValue: @autoclosure () -> b) {
  }
  var wrappedValue: b
}
func d() {
  let c = {
    (@a e) in
    e + 0
  }
}
