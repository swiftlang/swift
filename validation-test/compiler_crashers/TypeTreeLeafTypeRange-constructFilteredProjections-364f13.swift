// {"kind":"emit-sil","original":"fff6b414","signature":"swift::TypeTreeLeafTypeRange::constructFilteredProjections(swift::SILValue, swift::SILInstruction*, llvm::SmallBitVector&, swift::DominanceInfo*, llvm::function_ref<bool (swift::SILValue, swift::TypeTreeLeafTypeRange, swift::NeedsDestroy_t)>)","signatureAssert":"Assertion failed: (foundProjection || llvm::count_if( enumDecl->getAllElements(), [](auto *elt) { return elt->hasAssociatedValues(); }) == 1 || isDominatedByPayloadlessSwitchEnumAddrDests(insertPt, seais, domTree)), function constructFilteredProjections","signatureNext":"TypeTreeLeafTypeRange::constructProjectionsForNeededElements"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
enum a<b, c>: ~Copyable {
  case d(b)
  case e(c)
  func h() {
    if case .d(let f) = consume self {
    }
  }
}
