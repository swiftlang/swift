// {"kind":"emit-silgen","signature":"(anonymous namespace)::LogicalKeyPathApplicationComponent::get(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, swift::Lowering::SGFContext) &&","signatureAssert":"Assertion failed: (keyPathTy->getGenericArgs()[0]->getCanonicalType() == BaseFormalType->getCanonicalType()), function get","signatureNext":"Lowering::SILGenFunction::emitLoadOfLValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
}
struct c<b>: a {
  init(d: b) {
  }
}
func e<f>(_: f) -> KeyPath<f, f> {
}
func g<b>(_ h: b) -> some a {
  c(d: h)
}
func i() {
  let j = g("")
  let k = e(j)
  j[keyPath: k]
}
