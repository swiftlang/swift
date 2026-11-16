// {"extraArgs":["-O","-sil-verify-all"],"kind":"emit-ir","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitDebugValueInst"}
// RUN: not --crash %target-swift-frontend -emit-ir -O -sil-verify-all %s
public struct c {
  var d: Double
  var e, f, g, h: Double
}
public struct i<j> {
}
enum k {
  case r(c, c)
}
func l(m: k) -> c {
  switch m {
  case .r(let a, let b):
    var n = a
    return n
  }
}
public func o<j>(
  s: i<j>,
  t: ((), () -> c)
) -> i<j> {
  let p = s
  var q = p
  return q
}
