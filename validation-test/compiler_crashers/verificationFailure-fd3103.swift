// {"extraArgs":["-O"],"kind":"emit-sil","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"BridgedVerifier::verifierError"}
// RUN: not --crash %target-swift-frontend -emit-sil -O %s
class a {
  var b: Int
  init(b: Int) {
    self.b = b
  }
}
class c: a {
  var h: Int
  init(b: Int, h: Int) {
    self.h = h
    super.init(b: b)
  }
}
public struct d {
  var e: c
  public mutating func f() -> Bool {
    let g = e
    if isKnownUniquelyReferenced(&e) {
      e = c(b: g.b, h: g.h)
    }
    return true
  }
}
