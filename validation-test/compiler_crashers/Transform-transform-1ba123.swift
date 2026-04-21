// {"kind":"emit-silgen","original":"9be99b8e","signature":"(anonymous namespace)::Transform::transform(swift::Lowering::RValue&&, swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"Lowering::SILGenFunction::emitOrigToSubstValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
struct a<each b> {
  let c: (repeat each b)
}
@available(SwiftStdlib 5.9, *)
func d() {
  a(c: (Int.self, Float.self)).c
}
