// {"kind":"emit-sil","signature":"(anonymous namespace)::MemoryLifetimeVerifier::require(llvm::SmallBitVector const&, llvm::Twine const&, swift::SILInstruction*, bool)","signatureNext":"MemoryLifetimeVerifier::requireBitsSet"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
protocol c {
  func k() -> Double
}
@_effects(readnone)
func d<e>(_ f: e) -> e {
  f
}
func j<e>(_: e, _ b: e, l: Bool) -> e {
  b
}
func g(h: c, i: [Int]) -> Double {
  let a = d(h)
  let b = j(a, h, l: i.isEmpty)
  return b.k()
}
