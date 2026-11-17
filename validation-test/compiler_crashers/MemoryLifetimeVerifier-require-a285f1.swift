// {"kind":"emit-sil","signature":"(anonymous namespace)::MemoryLifetimeVerifier::require(llvm::SmallBitVector const&, llvm::Twine const&, swift::SILInstruction*, bool)","signatureNext":"MemoryLifetimeVerifier::requireBitsClear"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
class a {
}
class b: a {
}
@_effects(readnone)
func c<k>(_ d: inout k, _: consuming k) -> k {
  let e = d
  return e
}
func f<k>(_: inout k) -> Int {
  0
}
func g<k>(_ d: inout k, _: a, _: Bool) -> () -> k {
  { [d] in
    d
  }
}
func h(i: b, l: inout Int, m: inout a) -> Int {
  var j = c(&m, i)
  let n = f(&j)
  let o = g(&l, i, n > 0)
  return o()
}
