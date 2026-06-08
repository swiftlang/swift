#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumARCInstructionsElided, "Number of retain/release calls removed via topology");

namespace {

class TopologicalARCElision : public SILFunctionTransform {

  /// Evaluates if a given allocation never escapes its localized execution frame.
  bool isTopologicallyBounded(SILValue Value) {
    for (auto *Use : Value->getUses()) {
      auto *Instruction = Use->getUser();
      // If the value escapes to a background task or external pointer, we must count it.
      if (isa<ReturnInst>(Instruction) || isa<StoreInst>(Instruction)) {
        return false;
      }
    }
    return true;
  }

  void run() override {
    SILFunction *Function = getFunction();
    bool Changed = false;

    // Traverse the intermediate execution blocks
    for (auto &Block : *Function) {
      for (auto InstIt = Block.begin(), EndIt = Block.end(); InstIt != EndIt; ) {
        SILInstruction *Inst = &*InstIt++;

        // Intercept Apple's standard ARC insertions
        if (auto *Retain = dyn_cast<StrongRetainInst>(Inst)) {
          if (isTopologicallyBounded(Retain->getOperand())) {
            Retain->eraseFromParent();
            NumARCInstructionsElided++;
            Changed = true;
          }
        } else if (auto *Release = dyn_cast<StrongReleaseInst>(Inst)) {
          if (isTopologicallyBounded(Release->getOperand())) {
            Release->eraseFromParent();
            NumARCInstructionsElided++;
            Changed = true;
          }
        }
      }
    }

    if (Changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

// --- REGISTRATION FOR APPLE'S CI/CD ---
SILTransform *swift::createTopologicalARCElision() {
  return new TopologicalARCElision();
}
