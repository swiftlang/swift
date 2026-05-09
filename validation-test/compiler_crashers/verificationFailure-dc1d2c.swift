// {"kind":"emit-sil","original":"07dae9f0","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier::verifyCheckedCast"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
protocol a<b> {
  associatedtype b
}
@available(SwiftStdlib 5.7, *)
func c<d, e>(h: d, i: e) -> Bool {
  let f: Any.Type = a<d>.self
  f is ().Type
  let g = 0
  return g != nil
}
