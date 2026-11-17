// {"kind":"emit-silgen","signature":"(anonymous namespace)::Transform::transform(swift::Lowering::ManagedValue, swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILType, swift::Lowering::SGFContext)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a: AnyObject {
}
class b {
}
func c(d: borrowing a) {
  d as? b
}
