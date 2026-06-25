// {"kind":"emit-silgen","original":"177eee19","signature":"(anonymous namespace)::LetValueInitialization::bindValue(swift::SILValue, swift::Lowering::SILGenFunction&, bool, swift::SILLocation)","signatureAssert":"Assertion failed: (!SGF.VarLocs.count(vd) && \"Already emitted this vardecl?\"), function bindValue","signatureNext":"LetValueInitialization::copyOrInitValueInto"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a {
  let b: Double
  let c: Double
}
func ~= (d: a, e: Double) -> Bool {
}
protocol f {
}
struct g {
}
enum h: ~Copyable {
  case n(f, (Double, Int))
}
func i(j: consuming h) {
  switch consume j {
  case .n(let k as g, (a(b: 0.0, c: 0.0), let l)) where l > 0:
    k
  case let m:
    m
  }
}
