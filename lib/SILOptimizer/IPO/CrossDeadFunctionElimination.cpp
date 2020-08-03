#define DEBUG_TYPE "sil-cross-dead-function-elimination"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/Serialization/ModuleSummaryFile.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumDeadFunc, "Number of dead functions eliminated");

//===----------------------------------------------------------------------===//
//                      Pass Definition and Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILCrossDeadFuncElimination : public SILModuleTransform {
private:
  ModuleSummaryIndex TheSummary;

public:
  SILCrossDeadFuncElimination() {}
  
  void eliminateDeadEntriesFromTables(SILModule &M) {
    
    for (auto VT : M.getVTables()) {
      VT->removeEntries_if([&] (SILVTable::Entry &entry) -> bool {
        auto Impl = entry.getImplementation();
        auto &maybePair = TheSummary.getFunctionInfo(getGUID(Impl->getName()));
        if (!maybePair)
          return false;
        auto info = maybePair.getValue().first;
        return !info->isLive();
      });
    }
  
    auto &WitnessTables = M.getWitnessTableList();
    for (auto WI = WitnessTables.begin(), EI = WitnessTables.end(); WI != EI;) {
      SILWitnessTable *WT = &*WI;
      ++WI;
      WT->clearMethods_if([&] (const SILWitnessTable::MethodWitness &MW) -> bool {
        auto Impl = MW.Witness;
        auto &maybePair = TheSummary.getFunctionInfo(getGUID(Impl->getName()));
        if (!maybePair)
          return false;
        auto info = maybePair.getValue().first;
        return !info->isLive();
      });
    }

    auto DefaultWitnessTables = M.getDefaultWitnessTables();
    for (auto WI = DefaultWitnessTables.begin(),
              EI = DefaultWitnessTables.end();
         WI != EI;) {
      SILDefaultWitnessTable *WT = &*WI;
      ++WI;
      WT->clearMethods_if([&](SILFunction *MW) -> bool {
        if (!MW)
          return false;
        auto &maybePair = TheSummary.getFunctionInfo(getGUID(MW->getName()));
        if (!maybePair)
          return false;
        auto info = maybePair.getValue().first;
        return !info->isLive();
      });
    }
  }
  
  void eliminateDeadFunctions(SILModule &M, std::vector<SILFunction *> &DeadFunctions) {
    for (auto &pair : TheSummary) {
      auto &info = pair.second;
      if (info.TheSummary->isLive()) {
        continue;
      }

      auto F = M.lookUpFunction(info.Name);
      if (!F) {
        continue;
      }
      F->dropAllReferences();
      DeadFunctions.push_back(F);
      LLVM_DEBUG(llvm::dbgs() << "Eliminate " << info.Name << "\n");
    }
  }
  
  void ensureLive(SILFunction *F) {
    ensureLive(getGUID(F->getName()));
  }
  
  void ensureLive(GUID guid) {
    auto maybePair = this->TheSummary.getFunctionInfo(guid);
    if (maybePair) {
      auto pair = maybePair.getValue();
      pair.first->setLive(true);
    }
  }
  
  void ensureLive(VirtualMethodSlot slot) {
    auto Impls = this->TheSummary.getImplementations(slot);
    if (!Impls) return;
    for (auto Impl : Impls.getValue()) {
      ensureLive(Impl);
    }
  }

  void
  ensureKeyPathComponentIsAlive(const KeyPathPatternComponent &component) {
    component.visitReferencedFunctionsAndMethods(
      [this](SILFunction *F) {
        this->ensureLive(F);
      },
      [this](SILDeclRef method) {
        auto decl = cast<AbstractFunctionDecl>(method.getDecl());
        if (auto clas = dyn_cast<ClassDecl>(decl->getDeclContext())) {
          VirtualMethodSlot slot(method, VirtualMethodSlot::KindTy::VTable);
          this->ensureLive(slot);
        } else if (isa<ProtocolDecl>(decl->getDeclContext())) {
          VirtualMethodSlot slot(method, VirtualMethodSlot::KindTy::Witness);
          this->ensureLive(slot);
        } else {
          llvm_unreachable("key path keyed by a non-class, non-protocol method");
        }
      }
    );
  }
  
  void ensurePreserved(SILModule &M) {
    // Check property descriptor implementations.
    for (SILProperty &P : M.getPropertyList()) {
      if (auto component = P.getComponent()) {
        ensureKeyPathComponentIsAlive(*component);
      }
    }
  }

  void run() override {
    LLVM_DEBUG(llvm::dbgs() << "Running CrossDeadFuncElimination\n");
    auto &Context = getModule()->getASTContext();
    auto ModuleSummaryPath = getOptions().ModuleSummaryPath;
    auto ErrOrBuf = llvm::MemoryBuffer::getFile(ModuleSummaryPath);
    if (!ErrOrBuf) {
      Context.Diags.diagnose(SourceLoc(), diag::error_no_such_file_or_directory,
                             ModuleSummaryPath);
      return;
    }

    auto HasErr = modulesummary::loadModuleSummaryIndex(
        ErrOrBuf.get()->getMemBufferRef(), TheSummary);
    if (HasErr) {
      llvm::report_fatal_error("Invalid module summary");
    }

    auto &M = *getModule();
    this->ensurePreserved(M);
    this->eliminateDeadEntriesFromTables(M);
    std::vector<SILFunction *> DeadFunctions;
    this->eliminateDeadFunctions(M, DeadFunctions);

    while (!DeadFunctions.empty()) {
      SILFunction *F = DeadFunctions.back();
      DeadFunctions.pop_back();
      notifyWillDeleteFunction(F);
      M.eraseFunction(F);
    }
    this->invalidateFunctionTables();
  }
};

} // end anonymous namespace

SILTransform *swift::createCrossDeadFunctionElimination() {
  return new SILCrossDeadFuncElimination();
}
