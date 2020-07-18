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

namespace {
class FunctionSummaryIndexer {
  std::vector<FunctionSummary::EdgeTy> CallGraphEdgeList;
public:
  void indexInstruction(SILFunction &F, SILInstruction *I);
  void indexFunction(SILFunction &F);
  
  std::unique_ptr<FunctionSummary> takeSummary() {
    return std::make_unique<FunctionSummary>(std::move(CallGraphEdgeList));
  }
};

void FunctionSummaryIndexer::indexInstruction(SILFunction &F, SILInstruction *I) {
  // TODO: Handle dynamically replacable function ref inst
  if (auto *FRI = dyn_cast<FunctionRefInst>(I)) {
    SILFunction *callee = FRI->getReferencedFunctionOrNull();
    assert(callee);
    auto edge =
        FunctionSummary::EdgeTy::staticCall(getGUID(callee->getName()));
    CallGraphEdgeList.push_back(edge);
    return;
  }

  if (auto *WMI = dyn_cast<WitnessMethodInst>(I)) {
    auto edge = FunctionSummary::EdgeTy::witnessCall(WMI->getMember());
    CallGraphEdgeList.push_back(edge);
    return;
  }

  if (auto *MI = dyn_cast<MethodInst>(I)) {
    auto edge = FunctionSummary::EdgeTy::vtableCall(MI->getMember());
    CallGraphEdgeList.push_back(edge);
    return;
  }
}

void FunctionSummaryIndexer::indexFunction(SILFunction &F) {
  for (auto &BB : F) {
     for (auto &I : BB) {
       indexInstruction(F, &I);
     }
  }
}
};

std::unique_ptr<FunctionSummary>
buildFunctionSummaryIndex(SILFunction &F, BasicCalleeAnalysis &BCA) {
  FunctionSummaryIndexer indexer;
  indexer.indexFunction(F);
  return indexer.takeSummary();
}

void indexWitnessTable(ModuleSummaryIndex &index, SILWitnessTable &WT) {
  auto &Protocol = *WT.getProtocol();
  for (auto entry : WT.getEntries()) {
    if (entry.getKind() != SILWitnessTable::Method) break;

    auto methodWitness = entry.getMethodWitness();
    auto VirtualFunc = methodWitness.Requirement.getFuncDecl();
    auto Witness = methodWitness.Witness;
    llvm::dbgs() << "Emit table entry for " << Witness->getName() << "\n";
    VirtualMethodSlot slot(*VirtualFunc, Protocol);
    index.addImplementation(slot, getGUID(Witness->getName()));
  }
}


void indexVTable(ModuleSummaryIndex &index, SILVTable &VT) {
  auto &Class = *VT.getClass();
  for (auto entry : VT.getEntries()) {
    auto VirtualFunc = entry.getMethod().getFuncDecl();
    auto Impl = entry.getImplementation();
    VirtualMethodSlot slot(*VirtualFunc, Class);
    index.addImplementation(slot, getGUID(Impl->getName()));
  }
}

ModuleSummaryIndex swift::buildModuleSummaryIndex(SILModule &M,
                                           BasicCalleeAnalysis &BCA) {
  ModuleSummaryIndex index;

  index.setModuleName(M.getSwiftModule()->getName().str());

  for (auto &F : M) {
    auto FS = buildFunctionSummaryIndex(F, BCA);
    index.addFunctionSummary(F.getName(), std::move(FS));
  }
  
  for (auto &WT : M.getWitnessTableList()) {
    indexWitnessTable(index, WT);
  }
  
  for (auto &VT : M.getVTables()) {
    indexVTable(index, *VT);
  }
  return index;
}
