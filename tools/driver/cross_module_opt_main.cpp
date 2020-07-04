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
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"

using namespace llvm::opt;
using namespace swift;

class MergeModuleSummaryInvocation {
private:
  std::string MainExecutablePath;
  std::string OutputFilename = "-";
  std::vector<std::string> InputFilenames;

public:
  void setMainExecutablePath(const std::string &Path) {
    MainExecutablePath = Path;
  }

  const std::string &getOutputFilename() { return OutputFilename; }

  const std::vector<std::string> &getInputFilenames() { return InputFilenames; }

  int parseArgs(llvm::ArrayRef<const char *> Args, DiagnosticEngine &Diags) {
    using namespace options;

    // Parse frontend command line options using Swift's option table.
    std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
    unsigned MissingIndex;
    unsigned MissingCount;
    llvm::opt::InputArgList ParsedArgs =
        Table->ParseArgs(Args, MissingIndex, MissingCount, ModuleWrapOption);
    if (MissingCount) {
      Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                     ParsedArgs.getArgString(MissingIndex), MissingCount);
      return 1;
    }

    if (ParsedArgs.hasArg(OPT_UNKNOWN)) {
      for (const Arg *A : ParsedArgs.filtered(OPT_UNKNOWN)) {
        Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                       A->getAsString(ParsedArgs));
      }
      return true;
    }

    for (const Arg *A : ParsedArgs.filtered(OPT_INPUT)) {
      InputFilenames.push_back(A->getValue());
    }

    if (InputFilenames.empty()) {
      Diags.diagnose(SourceLoc(), diag::error_mode_requires_an_input_file);
      return 1;
    }

    if (const Arg *A = ParsedArgs.getLastArg(OPT_o)) {
      OutputFilename = A->getValue();
    }

    return 0;
  }
};

static llvm::DenseSet<GUID> computePreservedGUIDs() {
  llvm::DenseSet<GUID> Set(1);
  Set.insert(getGUID("main"));
  return Set;
}

void markDeadSymbols(ModuleSummaryIndex &summary, llvm::DenseSet<GUID> &PreservedGUIDs) {
  
  SmallVector<GUID, 8> Worklist;
  unsigned LiveSymbols = 0;

  for (auto GUID : PreservedGUIDs) {
    Worklist.push_back(GUID);
  }
  
  while (!Worklist.empty()) {
    auto GUID = Worklist.pop_back_val();
    
    auto maybePair = summary.getFunctionInfo(GUID);
    if (!maybePair) {
      llvm_unreachable("Bad GUID");
    }
    auto pair = maybePair.getValue();
    auto FS = pair.first;
    if (FS->isLive()) continue;

    llvm::dbgs() << "Mark " << pair.second << " as live\n";
    FS->setLive(true);
    LiveSymbols++;
    
    for (auto Call : FS->calls()) {
      switch (Call.getKind()) {
      case FunctionSummary::EdgeTy::Kind::Static: {
        Worklist.push_back(Call.getCallee());
        continue;
      }
      case FunctionSummary::EdgeTy::Kind::Witness:
      case FunctionSummary::EdgeTy::Kind::VTable: {
        llvm_unreachable("Witness and VTable calls are not supported yet.");
      }
      case FunctionSummary::EdgeTy::Kind::kindCount:
        llvm_unreachable("impossible");
      }
    }
  }
}

int cross_module_opt_main(ArrayRef<const char *> Args, const char *Argv0,
                          void *MainAddr) {
  INITIALIZE_LLVM();

  CompilerInstance Instance;
  PrintingDiagnosticConsumer PDC;
  Instance.addDiagnosticConsumer(&PDC);

  MergeModuleSummaryInvocation Invocation;
  std::string MainExecutablePath =
      llvm::sys::fs::getMainExecutable(Argv0, MainAddr);
  Invocation.setMainExecutablePath(MainExecutablePath);

  // Parse arguments.
  if (Invocation.parseArgs(Args, Instance.getDiags()) != 0) {
    return 1;
  }

  auto TheSummary = std::make_unique<ModuleSummaryIndex>();

  for (auto Filename : Invocation.getInputFilenames()) {
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
  
  auto PreservedGUIDs = computePreservedGUIDs();
  markDeadSymbols(*TheSummary.get(), PreservedGUIDs);

  modulesummary::emitModuleSummaryIndex(*TheSummary, Instance.getDiags(),
                                        Invocation.getOutputFilename());
  return 0;
}
