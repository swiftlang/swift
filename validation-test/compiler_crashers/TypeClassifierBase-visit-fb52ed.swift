// {"kind":"emit-silgen","signature":"(anonymous namespace)::TypeClassifierBase<(anonymous namespace)::LowerType, swift::Lowering::TypeLowering*>::visit(swift::CanType, swift::Lowering::AbstractionPattern, swift::IsTypeExpansionSensitive_t)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  associatedtype b
}
struct c<d: a> {
  var e: d.b
}
struct f<g>: a {
  typealias b = c<f<f>>
}
func h() -> c<f<Int>> {
}
