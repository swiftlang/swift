#define DEBUG_TYPE "lto-cross-module-opt"

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Option/Options.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/Serialization/ModuleSummaryFile.h"
#include "swift/Serialization/Validation.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Bitstream/BitstreamReader.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"

using namespace llvm::opt;
using namespace swift;

static llvm::cl::opt<std::string>
    LTOPrintLiveTrace("lto-print-live-trace", llvm::cl::init(""),
                      llvm::cl::desc("Print liveness trace for the symbol"));

static llvm::cl::list<std::string>
    InputFilenames(llvm::cl::Positional, llvm::cl::desc("[input files...]"));
static llvm::cl::opt<std::string>
    OutputFilename("o", llvm::cl::desc("output filename"));

static llvm::DenseSet<GUID> computePreservedGUIDs(ModuleSummaryIndex *summary) {
  llvm::DenseSet<GUID> Set(1);
  Set.insert(getGUID("main"));
  for (auto &pair : *summary) {
    auto &info = pair.second;
    if (info.TheSummary->isPreserved()) {
      Set.insert(pair.first);
    }
  }
  return Set;
}

class LivenessTrace {
public:
  enum ReasonTy { Preserved, StaticReferenced, IndirectReferenced };
  std::shared_ptr<LivenessTrace> markedBy;
  std::string symbol;
  GUID guid;
  ReasonTy reason;

  LivenessTrace(std::shared_ptr<LivenessTrace> markedBy, GUID guid,
                ReasonTy reason)
      : markedBy(markedBy), guid(guid), reason(reason) {}

  void setName(std::string name) { this->symbol = name; }

  void dump() { dump(llvm::errs()); }
  void dump(llvm::raw_ostream &os) {
    if (!symbol.empty()) {
      os << symbol;
    } else {
      os << "**missing name**"
         << " (" << guid << ")";
    }
    os << "is referenced by:\n";

    auto target = markedBy;
    while (target) {
      os << " - ";
      if (!target->symbol.empty()) {
        os << target->symbol;
      } else {
        os << "**missing name**";
      }
      os << " (" << target->guid << ")";
      os << "\n";
      target = target->markedBy;
    }
  }
};

void markDeadSymbols(ModuleSummaryIndex &summary, llvm::DenseSet<GUID> &PreservedGUIDs) {

  SmallVector<std::shared_ptr<LivenessTrace>, 8> Worklist;
  unsigned LiveSymbols = 0;

  for (auto GUID : PreservedGUIDs) {
    Worklist.push_back(std::make_shared<LivenessTrace>(
        nullptr, GUID, LivenessTrace::Preserved));
  }
  std::shared_ptr<LivenessTrace> dumpTarget;
  while (!Worklist.empty()) {
    auto trace = Worklist.pop_back_val();

    auto maybePair = summary.getFunctionInfo(trace->guid);
    if (!maybePair) {
      llvm_unreachable("Bad GUID");
    }
    auto pair = maybePair.getValue();
    auto FS = pair.first;
    trace->setName(pair.second);
    if (LTOPrintLiveTrace == pair.second) {
      dumpTarget = trace;
    }
    if (FS->isLive()) continue;

    LLVM_DEBUG(llvm::dbgs() << "Mark " << pair.second << " as live\n");
    FS->setLive(true);
    LiveSymbols++;
    
    for (auto Call : FS->calls()) {
      switch (Call.getKind()) {
      case FunctionSummary::EdgeTy::Kind::Static: {
        Worklist.push_back(std::make_shared<LivenessTrace>(
            trace, Call.getCallee(), LivenessTrace::StaticReferenced));
        continue;
      }
      case FunctionSummary::EdgeTy::Kind::Witness:
      case FunctionSummary::EdgeTy::Kind::VTable: {
        auto Impls = summary.getImplementations(Call.slot());
        if (!Impls) {
          continue;
        }
        for (auto Impl : Impls.getValue()) {
          Worklist.push_back(std::make_shared<LivenessTrace>(
              trace, Impl, LivenessTrace::IndirectReferenced));
        }
        break;
      }
      case FunctionSummary::EdgeTy::Kind::kindCount:
        llvm_unreachable("impossible");
      }
    }
  }
  if (dumpTarget) {
    dumpTarget->dump();
  }
}

int cross_module_opt_main(ArrayRef<const char *> Args, const char *Argv0,
                          void *MainAddr) {
  INITIALIZE_LLVM();

  llvm::cl::ParseCommandLineOptions(Args.size(), Args.data(), "Swift LTO\n");

  CompilerInstance Instance;
  PrintingDiagnosticConsumer PDC;
  Instance.addDiagnosticConsumer(&PDC);

  if (InputFilenames.empty()) {
    Instance.getDiags().diagnose(SourceLoc(),
                                 diag::error_mode_requires_an_input_file);
    return 1;
  }

  auto TheSummary = std::make_unique<ModuleSummaryIndex>();

  for (auto Filename : InputFilenames) {
    LLVM_DEBUG(llvm::dbgs() << "Loading module summary " << Filename << "\n");
    auto ErrOrBuf = llvm::MemoryBuffer::getFile(Filename);
    if (!ErrOrBuf) {
      Instance.getDiags().diagnose(
          SourceLoc(), diag::error_no_such_file_or_directory, Filename);
      return 1;
    }

    auto HasErr = swift::modulesummary::loadModuleSummaryIndex(
        ErrOrBuf.get()->getMemBufferRef(), *TheSummary.get());

    if (HasErr)
      llvm::report_fatal_error("Invalid module summary");
  }

  TheSummary->setModuleName("combined");
  
  auto PreservedGUIDs = computePreservedGUIDs(TheSummary.get());
  markDeadSymbols(*TheSummary.get(), PreservedGUIDs);

  modulesummary::emitModuleSummaryIndex(*TheSummary, Instance.getDiags(),
                                        OutputFilename);
  return 0;
}
