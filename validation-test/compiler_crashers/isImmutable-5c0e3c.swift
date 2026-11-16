// {"kind":"emit-sil","signature":"isImmutable(swift::SILValue, swift::StoreWeakInst*, llvm::DenseSet<swift::SILFunctionArgument*, llvm::DenseMapInfo<swift::SILFunctionArgument*, void>>&)","signatureAssert":"Assertion failed: (!fArg->isInferredImmutable() && \"Should have been checked earlier\"), function isImmutable","signatureNext":"MarkNeverWrittenMutableClosureBoxesAsImmutable::run"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
class d: Sendable {
}
func f(b: d) {
  Task { [weak b] in
    func e() {
      b
    }
    let a = e
    let c = e
  }
}
