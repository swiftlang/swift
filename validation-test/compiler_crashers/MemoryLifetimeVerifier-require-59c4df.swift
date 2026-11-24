// {"kind":"emit-sil","signature":"(anonymous namespace)::MemoryLifetimeVerifier::require(llvm::SmallBitVector const&, llvm::Twine const&, swift::SILInstruction*, bool)","signatureNext":"MemoryLifetimeVerifier::verify"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
@_effects(readnone)
func c<d>(_ e: consuming d) -> d {
  e
}
func f<d>(_ l: inout d, g: d) -> d {
  let h = l
  return h
}
func i<d>(q: d) -> () -> d {
  let p = q
  return {
    p
  }
}
func m<d>(_ a: inout d, _ b: d, o: Int) -> d {
  let j = f(&a, g: b)
  return k(&a, j, o: 1)
}
func k<d>(_ a: inout d, _ b: consuming d, o: Int) -> d {
  if o <= 0 {
    return b
  }
  let n = c(b)
  return m(&a, n, o: 1)
}
