// {"kind":"emit-sil","original":"8baed720","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier::checkLegalSILType"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
protocol a {
  associatedtype b
  func d() -> b
}
func e<each f: a>(
  g: repeat each f,
  h: (repeat () -> (each f).b) -> Int
) -> Int {
  func l<i: a>(_ j: i) -> () -> i.b {
    {
      j.d()
    }
  }
  h(repeat l(each g))
  func k() -> Int {
    func m() -> Int {
      k()
    }
    return m()
  }
  var c = 0
  return c
}
