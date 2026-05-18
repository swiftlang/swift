// {"kind":"emit-sil","original":"1b01e010","signature":"(anonymous namespace)::GlobalLivenessChecker::testInstVectorLiveness(llvm::SmallMapVector<swift::SILInstruction*, llvm::SmallBitVector, 4u>&)","signatureAssert":"Assertion failed: ((isLive == IsLive::LiveOut || foundSingleBlockError || foundInit) && \"Should either have a pure live out, found an init, or we \" \"should have found \" \"an error.\"), function testInstVectorLiveness","signatureNext":"MoveOnlyAddressCheckerPImpl::performSingleCheck"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
enum c: ~Copyable {
  case d(Double, Double)
  case g(Double)
  case e
}
func h(f: consuming c) -> c {
  switch f {
  case .d(let a, let b):
    .g(b)
  case .g:
    f
  case .e:
    .e
  }
}
