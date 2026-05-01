// {"kind":"emit-silgen","original":"44c57ff3","signature":"(anonymous namespace)::Transform::transform(swift::Lowering::ManagedValue, swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","signatureAssert":"Assertion failed: (expectedFnType->getExtInfo().hasContext() && \"conversion thunk will not be thin!\"), function transformFunction","signatureNext":"Lowering::SILGenFunction::emitTransformedValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b, c>(d: @convention(thin) (b) -> c) {
}
a { e in
  e * 2
}
