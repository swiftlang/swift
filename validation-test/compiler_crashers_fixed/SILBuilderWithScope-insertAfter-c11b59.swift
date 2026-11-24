// {"frontendArgs":["-emit-sil","-O","-enable-experimental-feature","OnceFunctions"],"kind":"custom","signature":"swift::SILBuilderWithScope::insertAfter(swift::SILInstruction*, llvm::function_ref<void (swift::SILBuilder&)>)","signatureAssert":"Assertion failed: (succBlock->getSinglePredecessorBlock() == inst->getParent() && \"the terminator instruction must not have critical successors\"), function insertAfter","signatureNext":"tryOptimizeApplyOfPartialApply"}
// RUN: %target-swift-frontend -emit-sil -O -enable-experimental-feature OnceFunctions %s
// REQUIRES: swift_feature_OnceFunctions

struct Failure: Error {}
@inline(never)
func callThrowing(_ f: @callable(once) () throws -> Void) throws {
  try f()
}
public func run(_ n: Int) {
  do {
    try callThrowing {
      print(n)
      throw Failure()
    }
  } catch {}
}
