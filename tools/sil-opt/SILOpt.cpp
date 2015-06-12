//===-- SILOpt.cpp - SIL Optimization Driver ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This is a tool for reading sil files and running sil passes on them. The
// targeted usecase is debugging and testing SIL passes.
//
//===----------------------------------------------------------------------===//

#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/SILOptions.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/Serialization/SerializationOptions.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
using namespace swift;

namespace {

enum class OptGroup {
  Unknown, Diagnostics, Performance
};

} // end anonymous namespace

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::desc("input file"), llvm::cl::init("-"),
              llvm::cl::Positional);

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("output filename"));

static llvm::cl::list<std::string>
ImportPaths("I", llvm::cl::desc("add a directory to the import search path"));

static llvm::cl::list<std::string>
FrameworkPaths("F", llvm::cl::desc("add a directory to the framework search path"));

static llvm::cl::opt<std::string>
ModuleName("module-name", llvm::cl::desc("The name of the module if processing"
                                         " a module. Necessary for processing "
                                         "stdin."));

static llvm::cl::opt<std::string>
ResourceDir("resource-dir",
    llvm::cl::desc("The directory that holds the compiler resource files"));

static llvm::cl::opt<std::string>
SDKPath("sdk", llvm::cl::desc("The path to the SDK for use with the clang "
                              "importer."),
        llvm::cl::init(""));

static llvm::cl::opt<std::string>
Target("target", llvm::cl::desc("target triple"));

static llvm::cl::opt<OptGroup> OptimizationGroup(
    llvm::cl::desc("Predefined optimization groups:"),
    llvm::cl::values(clEnumValN(OptGroup::Diagnostics, "diagnostics",
                                "Run diagnostic passes"),
                     clEnumValN(OptGroup::Performance, "O",
                                "Run performance passes"),
                     clEnumValEnd),
    llvm::cl::init(OptGroup::Unknown));

static llvm::cl::list<PassKind>
Passes(llvm::cl::desc("Passes:"),
       llvm::cl::values(clEnumValN(PassKind::AllocBoxToStack,
                                   "allocbox-to-stack", "Promote memory"),
                        clEnumValN(PassKind::CapturePromotion,
                                   "capture-promotion",
                                   "Promote closure capture variables"),
                        clEnumValN(PassKind::Mem2Reg,
                                   "mem2reg",
                                   "Promote stack allocations to registers"),
                        clEnumValN(PassKind::SILCleanup,
                                   "cleanup",
                                   "Cleanup SIL in preparation for IRGen"),
                        clEnumValN(PassKind::DiagnosticConstantPropagation,
                                   "diagnostic-constant-propagation",
                                   "Propagate constants and emit diagnostics"),
                        clEnumValN(PassKind::PerformanceConstantPropagation,
                                   "performance-constant-propagation",
                                   "Propagate constants and do not emit"
                                   " diagnostics"),
                        clEnumValN(PassKind::CSE,
                                   "cse",
                                   "Perform constant subexpression "
                                   "elimination."),
                        clEnumValN(PassKind::EmitDFDiagnostics,
                                   "dataflow-diagnostics",
                                   "Emit SIL diagnostics"),
                        clEnumValN(PassKind::NoReturnFolding,
                                   "noreturn-folding",
                                   "Add 'unreachable' after noreturn calls"),
                        clEnumValN(PassKind::DiagnoseUnreachable,
                                   "diagnose-unreachable",
                                   "Diagnose unreachable code"),
                        clEnumValN(PassKind::DefiniteInitialization,
                                   "definite-init","definitive initialization"),
                        clEnumValN(PassKind::InOutDeshadowing,
                                   "inout-deshadow",
                                   "Remove inout argument shadow variables"),
                        clEnumValN(PassKind::GlobalOpt,
                                   "global-opt",
                                   "Global variable optimizations"),
                        clEnumValN(PassKind::MandatoryInlining,
                                   "mandatory-inlining",
                                   "Inline transparent functions"),
                        clEnumValN(PassKind::GenericSpecializer,
                                   "specialize",
                                   "Specialize generic functions"),
                        clEnumValN(PassKind::Devirtualizer,
                                   "devirtualize",
                                   "Devirtualize virtual calls"),
                        clEnumValN(PassKind::InlineCaches,
                                   "inlinecaches",
                                   "Create inline caches for virtual calls"),
                        clEnumValN(PassKind::PredictableMemoryOptimizations,
                                   "predictable-memopt",
                                   "Predictable early memory optimization"),
                        clEnumValN(PassKind::SILCombine,
                                   "sil-combine",
                                   "Perform small peepholes and combine"
                                   " operations."),
                        clEnumValN(PassKind::DeadFunctionElimination,
                                   "sil-deadfuncelim",
                                   "Remove private unused functions"),
                        clEnumValN(PassKind::JumpThreadSimplifyCFG,
                                   "simplify-cfg",
                                   "Clean up the CFG of SIL functions"),
                        clEnumValN(PassKind::PerfInliner,
                                   "inline",
                                   "Inline functions which are determined to be"
                                   " less than a pre-set cost."),
                        clEnumValN(PassKind::EarlyInliner,
                                   "early-inline",
                                   "Inline functions that are not marked as "
                                   "having special semantics."),
                        clEnumValN(PassKind::EarlyCodeMotion,
                                   "early-codemotion",
                                   "Perform early code motion optimizations"),
                        clEnumValN(PassKind::LateCodeMotion,
                                   "late-codemotion",
                                   "Perform late code motion optimizations"),
                        clEnumValN(PassKind::LowerAggregateInstrs,
                                   "lower-aggregate-instrs",
                                   "Perform lower aggregate instrs to scalar "
                                   "instrs."),
                        clEnumValN(PassKind::SROA,
                                   "sroa",
                                   "Perform SIL scalar replacement of "
                                   "aggregates."),
                        clEnumValN(PassKind::StripDebugInfo,
                                   "strip-debug-info",
                                   "Strip debug info."),
                        clEnumValN(PassKind::DeadObjectElimination,
                                   "deadobject-elim",
                                   "Eliminate unused object allocation with no "
                                   "side effect destructors."),
                        clEnumValN(PassKind::InstCount,
                                   "inst-count",
                                   "Count all instructions in the given "
                                   "module."),
                        clEnumValN(PassKind::AADumper,
                                   "aa-dump",
                                   "Dump AA result for all pairs of ValueKinds"
                                   " in all functions."),
                        clEnumValN(PassKind::SILLinker,
                                   "linker",
                                   "Link in all serialized SIL referenced by "
                                   "the given SIL file."),
                        clEnumValN(PassKind::GlobalARCOpts,
                                   "global-arc-opts",
                                   "Perform multiple basic block arc optzns."),
                        clEnumValN(PassKind::DCE,
                                   "dce",
                                   "Eliminate dead code"),
                        clEnumValN(PassKind::LoopInfoPrinter,
                                   "loop-info-printer",
                                   "Display loop information."),
                        clEnumValN(PassKind::FunctionSignatureOpts,
                                   "function-signature-opts",
                                   "Optimize function signatures."),
                        clEnumValN(PassKind::CFGPrinter,
                                   "view-cfg",
                                   "View the CFG of all passed in functions."),
                        clEnumValN(PassKind::LoopRotate,
                                   "loop-rotate",
                                   "Rotate loops."),
                        clEnumValN(PassKind::LICM,
                                   "licm",
                                   "Loop invariant code motion."),
                        clEnumValN(PassKind::IVInfoPrinter,
                                   "iv-info-printer",
                                   "Display induction variable information."),
                        clEnumValN(PassKind::GlobalLoadStoreOpts,
                                   "global-load-store-opts",
                                   "Multiple basic block load store opts."),
                        clEnumValN(PassKind::COWArrayOpts,
                                   "cowarray-opt",
                                   "COW Array optimizations"),
                        clEnumValN(PassKind::ABCOpt,
                                   "abcopts",
                                   "Array bounds check opts."),
                        clEnumValN(PassKind::ClosureSpecializer,
                                   "closure-specialize",
                                   "Closure specialization."),
                        clEnumValN(PassKind::CapturePropagation,
                                   "capture-prop",
                                   "Captured Constant Propagation."),
                        clEnumValN(PassKind::CopyForwarding,
                                   "copy-forwarding",
                                   "Copy Forwarding."),
                        clEnumValN(PassKind::SplitAllCriticalEdges,
                                   "split-critical-edges",
                                   "Split all critical edges"),
                        clEnumValN(PassKind::SimplifyBBArgs,
                                   "simplify-bb-args",
                                   "Simplify basic block arguments"),
                        clEnumValN(PassKind::ExternalDefsToDecls,
                                   "external-defs-to-decls",
                                   "Convert external definitions to decls"),
                        clEnumValN(PassKind::MergeCondFails,
                                   "merge-cond_fails",
                                   "Merge cond_fail instructions"),
                        clEnumValN(PassKind::CropOverflowChecks,
                                   "crop-overflow-checks",
                                   "Removes redundant overflow checks"),
                        clEnumValN(PassKind::RemovePins,
                                   "remove-pins",
                                   "Remove pin/unpin pairs"),
                        clEnumValN(PassKind::SwiftArrayOpts,
                                   "array-specialize",
                                   "Specialize arrays"),
                        clEnumValN(PassKind::GlobalPropertyOpt,
                                   "global-property-opt",
                                   "Optimize properties"),
                        clEnumValEnd));

static llvm::cl::opt<bool>
PrintStats("print-stats", llvm::cl::desc("Print various statistics"));

static llvm::cl::opt<bool>
VerifyMode("verify",
           llvm::cl::desc("verify diagnostics against expected-"
                          "{error|warning|note} annotations"));

static llvm::cl::opt<unsigned>
AssertConfId("assert-conf-id", llvm::cl::Hidden,
             llvm::cl::init(0));

static llvm::cl::opt<int>
SILInlineThreshold("sil-inline-threshold", llvm::cl::Hidden,
                   llvm::cl::init(-1));

static llvm::cl::opt<bool>
EnableSILVerifyAll("enable-sil-verify-all",
                   llvm::cl::Hidden,
                   llvm::cl::init(true),
                   llvm::cl::desc("Run sil verifications after every pass."));

static llvm::cl::opt<bool>
RemoveRuntimeAsserts("remove-runtime-asserts",
                     llvm::cl::Hidden,
                     llvm::cl::init(false),
                     llvm::cl::desc("Remove runtime assertions (cond_fail)."));

static llvm::cl::opt<bool>
EmitVerboseSIL("emit-verbose-sil",
               llvm::cl::desc("Emit locations during sil emission."));

static llvm::cl::opt<bool>
EmitSIB("emit-sib", llvm::cl::desc("Emit serialized AST + SIL file(s)"));

static llvm::cl::opt<std::string>
ModuleCachePath("module-cache-path", llvm::cl::desc("Clang module cache path"));

static llvm::cl::opt<bool>
EnableSILSortOutput("sil-sort-output", llvm::cl::Hidden,
                    llvm::cl::init(false),
                    llvm::cl::desc("Sort Functions, VTables, Globals, "
                                   "WitnessTables by name to ease diffing."));

static llvm::cl::opt<bool>
DisableASTDump("sil-disable-ast-dump", llvm::cl::Hidden,
               llvm::cl::init(false),
               llvm::cl::desc("Do not dump AST."));

static llvm::cl::opt<unsigned>
ASTVerifierProcessCount("ast-verifier-process-count", llvm::cl::Hidden,
                        llvm::cl::init(1));

static llvm::cl::opt<unsigned>
ASTVerifierProcessId("ast-verifier-process-id", llvm::cl::Hidden,
                     llvm::cl::init(1));

static void runCommandLineSelectedPasses(SILModule *Module) {
  SILPassManager PM(Module);

  PM.registerAnalysis(createCallGraphAnalysis(Module));
  PM.registerAnalysis(createAliasAnalysis(Module));
  PM.registerAnalysis(createDominanceAnalysis(Module));
  PM.registerAnalysis(createPostDominanceAnalysis(Module));
  PM.registerAnalysis(createLoopInfoAnalysis(Module, &PM));
  PM.registerAnalysis(createInductionVariableAnalysis(Module));
  PM.registerAnalysis(createPostOrderAnalysis(Module));
  PM.registerAnalysis(createClassHierarchyAnalysis(Module));
  PM.registerAnalysis(createRCIdentityAnalysis(Module, &PM));
  PM.registerAnalysis(createDestructorAnalysis(Module));

  for (auto Pass : Passes) {
    PM.addPass(Pass);
  }
  PM.run();
}

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// getMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement getMainExecutable
// without being given the address of a function in the main executable).
void anchorForGetMainExecutable() {}

int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram X(argc, argv);

  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift SIL optimizer\n");

  // Call llvm_shutdown() on exit to print stats and free memory.
  llvm::llvm_shutdown_obj Y;

  if (PrintStats)
    llvm::EnableStatistics();

  CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(argv[0],
          reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  // Give the context the list of search paths to use for modules.
  Invocation.setImportSearchPaths(ImportPaths);
  Invocation.setFrameworkSearchPaths(FrameworkPaths);
  // Set the SDK path and target if given.
  if (SDKPath.getNumOccurrences() == 0) {
    const char *SDKROOT = getenv("SDKROOT");
    if (SDKROOT)
      SDKPath = SDKROOT;
  }
  if (!SDKPath.empty())
    Invocation.setSDKPath(SDKPath);
  if (!Target.empty())
    Invocation.setTargetTriple(Target);
  if (!ResourceDir.empty())
    Invocation.setRuntimeResourcePath(ResourceDir);
  // Set the module cache path. If not passed in we use the default swift module
  // cache.
  Invocation.getClangImporterOptions().ModuleCachePath = ModuleCachePath;
  Invocation.setParseStdlib();
  Invocation.getLangOptions().EnableAccessControl = false;

  Invocation.getLangOptions().ASTVerifierProcessCount =
      ASTVerifierProcessCount;
  Invocation.getLangOptions().ASTVerifierProcessId =
      ASTVerifierProcessId;

  // Setup the SIL Options.
  SILOptions &SILOpts = Invocation.getSILOptions();
  SILOpts.InlineThreshold = SILInlineThreshold;
  SILOpts.VerifyAll = EnableSILVerifyAll;
  SILOpts.RemoveRuntimeAsserts = RemoveRuntimeAsserts;
  SILOpts.AssertConfig = AssertConfId;

  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
    llvm::MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (!FileBufOrErr) {
    fprintf(stderr, "Error! Failed to open file: %s\n", InputFilename.c_str());
    exit(-1);
  }

  // If it looks like we have an AST, set the source file kind to SIL and the
  // name of the module to the file's name.
  Invocation.addInputBuffer(FileBufOrErr.get().get());

  serialization::ExtendedValidationInfo extendedInfo;
  auto result = serialization::validateSerializedAST(
      FileBufOrErr.get()->getBuffer(), &extendedInfo);
  bool HasSerializedAST = result.status == serialization::Status::Valid;

  if (HasSerializedAST) {
    const StringRef Stem = ModuleName.size() ?
                             StringRef(ModuleName) :
                             llvm::sys::path::stem(InputFilename);
    Invocation.setModuleName(Stem);
    Invocation.setInputKind(InputFileKind::IFK_Swift_Library);
  } else {
    const StringRef Name = ModuleName.size() ? StringRef(ModuleName) : "main";
    Invocation.setModuleName(Name);
    Invocation.setInputKind(InputFileKind::IFK_SIL);
  }

  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  if (CI.setup(Invocation))
    return 1;
  CI.performSema();

  // If parsing produced an error, don't run any passes.
  if (CI.getASTContext().hadError())
    return 1;

  // Load the SIL if we have a module. We have to do this after SILParse
  // creating the unfortunate double if statement.
  if (HasSerializedAST) {
    assert(!CI.hasSILModule() &&
           "performSema() should not create a SILModule.");
    CI.setSILModule(SILModule::createEmptyModule(CI.getMainModule(),
                                                 CI.getSILOptions()));
    std::unique_ptr<SerializedSILLoader> SL = SerializedSILLoader::create(
        CI.getASTContext(), CI.getSILModule(), nullptr);

    if (extendedInfo.isSIB())
      SL->getAllForModule(CI.getMainModule()->getName(), nullptr);
    else
      SL->getAll();
  }

  // If we're in verify mode, install a custom diagnostic handling for
  // SourceMgr.
  if (VerifyMode)
    enableDiagnosticVerifier(CI.getSourceMgr());

  if (OptimizationGroup == OptGroup::Diagnostics) {
    runSILDiagnosticPasses(*CI.getSILModule());
  } else if (OptimizationGroup == OptGroup::Performance) {
    runSILOptimizationPasses(*CI.getSILModule());
  } else {
    runCommandLineSelectedPasses(CI.getSILModule());
  }

  if (EmitSIB) {
    llvm::SmallString<128> OutputFile;
    if (OutputFilename.size()) {
      OutputFile = OutputFilename;
    } else if (ModuleName.size()) {
      OutputFile = ModuleName;
      llvm::sys::path::replace_extension(OutputFile, SIB_EXTENSION);
    } else {
      OutputFile = CI.getMainModule()->getName().str();
      llvm::sys::path::replace_extension(OutputFile, SIB_EXTENSION);
    }

    SerializationOptions serializationOpts;
    serializationOpts.OutputPath = OutputFile.c_str();
    serializationOpts.SerializeAllSIL = true;
    serializationOpts.IsSIB = true;

    serialize(CI.getMainModule(), serializationOpts, CI.getSILModule());
  } else {
    const StringRef OutputFile = OutputFilename.size() ?
                                   StringRef(OutputFilename) : "-";
    std::error_code EC;
    llvm::raw_fd_ostream OS(OutputFile, EC, llvm::sys::fs::F_None);
    if (EC) {
      llvm::errs() << "while opening '" << OutputFile << "': "
                   << EC.message() << '\n';
      return 1;
    }
    CI.getSILModule()->print(OS, EmitVerboseSIL, CI.getMainModule(),
                             EnableSILSortOutput, !DisableASTDump);
  }

  bool HadError = CI.getASTContext().hadError();

  // If we're in -verify mode, we've buffered up all of the generated
  // diagnostics.  Check now to ensure that they meet our expectations.
  if (VerifyMode) {
    HadError = verifyDiagnostics(CI.getSourceMgr(), CI.getInputBufferIDs());
    DiagnosticEngine &diags = CI.getDiags();
    if (diags.hasFatalErrorOccurred() &&
        !Invocation.getDiagnosticOptions().ShowDiagnosticsAfterFatalError) {
      diags.resetHadAnyError();
      diags.diagnose(SourceLoc(), diag::verify_encountered_fatal);
      HadError = true;
    }
  }

  return HadError;
}
