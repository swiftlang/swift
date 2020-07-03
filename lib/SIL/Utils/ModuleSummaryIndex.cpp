#include "swift/Demangling/Demangle.h"
#include "swift/SIL/ModuleSummary.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/MD5.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "function-order-printer"

using namespace swift;

std::unique_ptr<FunctionSummary>
buildFunctionSummaryIndex(SILFunction &F, BasicCalleeAnalysis &BCA) {
  std::vector<FunctionSummary::EdgeTy> CallGraphEdgeList;

  for (auto &BB : F) {
    for (auto &I : BB) {
      auto FAS = FullApplySite::isa(&I);
      if (!FAS)
        continue;
      CalleeList Callees = BCA.getCalleeList(FAS);
      if (Callees.isIncomplete()) {
        auto Callee = FAS.getCalleeOrigin();
        switch (Callee->getKind()) {
        case ValueKind::WitnessMethodInst: {
          auto WMI = cast<WitnessMethodInst>(Callee);
          auto edge = FunctionSummary::EdgeTy::witnessCall(WMI->getMember());
          CallGraphEdgeList.push_back(edge);
          break;
        }
        case ValueKind::ClassMethodInst: {
          auto CMI = cast<ClassMethodInst>(Callee);
          auto edge = FunctionSummary::EdgeTy::vtableCall(CMI->getMember());
          CallGraphEdgeList.push_back(edge);
          break;
        }
        default:
          llvm_unreachable("invalid kind");
        }
        continue;
      }

      // For static calls
      std::vector<GUID> CalleeGUIDs;
      for (auto Callee : Callees) {
        auto edge =
            FunctionSummary::EdgeTy::staticCall(getGUID(Callee->getName()));
        CallGraphEdgeList.push_back(edge);
      }
    }
  }
  return std::make_unique<FunctionSummary>(CallGraphEdgeList);
}

ModuleSummaryIndex buildModuleSummaryIndex(SILModule &M,
                                           BasicCalleeAnalysis &BCA) {
  ModuleSummaryIndex index;
  for (auto &F : M) {
    auto FS = buildFunctionSummaryIndex(F, BCA);
    index.addFunctionSummary(getGUID(F.getName()), std::move(FS));
  }
  return index;
}
