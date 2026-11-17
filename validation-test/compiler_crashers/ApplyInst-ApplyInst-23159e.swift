// {"extraArgs":["-O"],"kind":"emit-sil","signature":"swift::ApplyInst::ApplyInst(swift::SILDebugLocation, swift::SILValue, swift::SILType, swift::SILType, swift::SubstitutionMap, llvm::ArrayRef<swift::SILValue>, llvm::ArrayRef<swift::SILValue>, std::__1::optional<llvm::ArrayRef<swift::SILLocation>>, swift::optionset::OptionSet<swift::ApplyFlags, unsigned char>, swift::GenericSpecializationInformation const*, std::__1::optional<swift::ApplyIsolationCrossing>)","signatureAssert":"Assertion failed: (!substCalleeTy.castTo<SILFunctionType>()->isCoroutine()), function ApplyInst","signatureNext":"ApplyInst::create"}
// RUN: not --crash %target-swift-frontend -emit-sil -O %s
public struct a<b> {
  var c: [b]
  public subscript(d: Int) -> b {
    @_specialize(where b == Int)
    _read {
      yield c[d]
    }
  }
}
