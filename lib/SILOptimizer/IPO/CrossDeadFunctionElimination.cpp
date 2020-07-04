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

    auto M = getModule();

    for (auto &pair : TheSummary) {
      auto &info = pair.second;
      if (info.TheSummary->isLive()) {
        continue;
      }

      auto F = M->lookUpFunction(info.Name);
      if (!F) {
        llvm::dbgs() << "Couldn't eliminate " << info.Name
                     << " because it's not found\n";
        continue;
      }
      F->dropAllReferences();
      notifyWillDeleteFunction(F);
      M->eraseFunction(F);

      llvm::dbgs() << "Eliminate " << info.Name << "\n";
    }
    this->invalidateFunctionTables();
  }
};

} // end anonymous namespace

SILTransform *swift::createCrossDeadFunctionElimination() {
  return new SILCrossDeadFuncElimination();
}
