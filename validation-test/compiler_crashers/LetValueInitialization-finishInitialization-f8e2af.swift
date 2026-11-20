// {"extraArgs":["-enable-sil-opaque-values"],"kind":"emit-silgen","signature":"(anonymous namespace)::LetValueInitialization::finishInitialization(swift::Lowering::SILGenFunction&)","signatureAssert":"Assertion failed: (!SGF.VarLocs.count(vd) && \"Should not have bound the let yet!\"), function finishInitialization","signatureNext":"PatternMatchEmission::bindVariable"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-sil-opaque-values %s
enum a<b>: ~Copyable {
  case c(b)
  case d
  consuming func e(f: b) {
    switch self {
    case .c(let g):
      g
    case .d:
      f
    }
  }
}
