//===--- LLVMOpt.cpp ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This is a simple reimplementation of opt that includes support for Swift-
/// specific LLVM passes. It is meant to make it easier to handle issues related
/// to transitioning to the new LLVM pass manager (which lacks the dynamism of
/// the old pass manager) and also problems during the code base transition to
/// that pass manager. Additionally it will enable a user to exactly simulate
/// Swift's LLVM pass pipeline by using the same pass pipeline building
/// machinery in IRGen, something not possible with opt.
///
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/LLVMContext.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/LLVMPasses/PassesFwd.h"
#include "swift/LLVMPasses/Passes.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/CallGraphSCCPass.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/RegionPass.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/CodeGen/CommandFlags.def"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LegacyPassNameParser.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/LinkAllIR.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PluginLoader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                            Option Declarations
//===----------------------------------------------------------------------===//

// The OptimizationList is automatically populated with registered passes by the
// PassNameParser.
//
static llvm::cl::list<const llvm::PassInfo *, bool, llvm::PassNameParser>
    PassList(llvm::cl::desc("Optimizations available:"));

static llvm::cl::opt<bool>
    Optimized("O", llvm::cl::desc("Optimization level O. Similar to swift -O"));

// TODO: I wanted to call this 'verify', but some other pass is using this
// option.
static llvm::cl::opt<bool> VerifyEach(
    "verify-each",
    llvm::cl::desc("Should we spend time verifying that the IR is well "
                   "formed"));

static llvm::cl::opt<std::string>
    TargetTriple("mtriple",
                 llvm::cl::desc("Override target triple for module"));

static llvm::cl::opt<bool>
    PrintStats("print-stats",
               llvm::cl::desc("Should LLVM Statistics be printed"));

static cl::opt<std::string> InputFilename(cl::Positional,
                                          cl::desc("<input file>"),
                                          cl::init("-"),
                                          cl::value_desc("filename"));

static cl::opt<std::string> OutputFilename("o",
                                           cl::desc("Override output filename"),
                                           cl::value_desc("filename"));

static cl::opt<std::string> DefaultDataLayout(
    "default-data-layout",
    cl::desc("data layout string to use if not specified by module"),
    cl::value_desc("layout-string"), cl::init(""));

//===----------------------------------------------------------------------===//
//                               Helper Methods
//===----------------------------------------------------------------------===//

static llvm::CodeGenOpt::Level GetCodeGenOptLevel() {
  // TODO: Is this the right thing to do here?
  if (Optimized)
    return CodeGenOpt::Default;
  return CodeGenOpt::None;
}

// Returns the TargetMachine instance or zero if no triple is provided.
static llvm::TargetMachine *
getTargetMachine(llvm::Triple TheTriple, StringRef CPUStr,
                 StringRef FeaturesStr, const llvm::TargetOptions &Options) {
  std::string Error;
  const auto *TheTarget =
      llvm::TargetRegistry::lookupTarget(MArch, TheTriple, Error);
  // Some modules don't specify a triple, and this is okay.
  if (!TheTarget) {
    return nullptr;
  }

  return TheTarget->createTargetMachine(
      TheTriple.getTriple(), CPUStr, FeaturesStr, Options,
      Optional<Reloc::Model>(RelocModel), getCodeModel(), GetCodeGenOptLevel());
}

static void dumpOutput(llvm::Module &M, llvm::raw_ostream &os) {
  // For now just always dump assembly.
  legacy::PassManager EmitPasses;
  EmitPasses.add(createPrintModulePass(os));
  EmitPasses.run(M);
}

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// getMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement getMainExecutable
// without being given the address of a function in the main executable).
void anchorForGetMainExecutable() {}

static inline void addPass(legacy::PassManagerBase &PM, Pass *P) {
  // Add the pass to the pass manager...
  PM.add(P);
  if (P->getPassID() == &SwiftAAWrapperPass::ID) {
    PM.add(createExternalAAWrapperPass([](Pass &P, Function &, AAResults &AAR) {
      if (auto *WrapperPass = P.getAnalysisIfAvailable<SwiftAAWrapperPass>())
        AAR.addAAResult(WrapperPass->getResult());
    }));
  }

  // If we are verifying all of the intermediate steps, add the verifier...
  if (VerifyEach)
    PM.add(createVerifierPass());
}

static void runSpecificPasses(StringRef Binary, llvm::Module *M,
                              llvm::TargetMachine *TM,
                              llvm::Triple &ModuleTriple) {
  llvm::legacy::PassManager Passes;
  llvm::TargetLibraryInfoImpl TLII(ModuleTriple);
  Passes.add(new TargetLibraryInfoWrapperPass(TLII));

  const llvm::DataLayout &DL = M->getDataLayout();
  if (DL.isDefault() && !DefaultDataLayout.empty()) {
    M->setDataLayout(DefaultDataLayout);
  }

  // Add internal analysis passes from the target machine.
  Passes.add(createTargetTransformInfoWrapperPass(TM ? TM->getTargetIRAnalysis()
                                                     : TargetIRAnalysis()));

  if (TM) {
    // FIXME: We should dyn_cast this when supported.
    auto &LTM = static_cast<LLVMTargetMachine &>(*TM);
    Pass *TPC = LTM.createPassConfig(Passes);
    Passes.add(TPC);
  }

  for (const llvm::PassInfo *PassInfo : PassList) {
    llvm::Pass *P = nullptr;
    if (PassInfo->getNormalCtor())
      P = PassInfo->getNormalCtor()();
    else
      errs() << Binary << ": cannot create pass: " << PassInfo->getPassName()
             << "\n";
    if (P) {
      addPass(Passes, P);
    }
  }

  // Do it.
  Passes.run(*M);
}

//===----------------------------------------------------------------------===//
//                            Main Implementation
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  PROGRAM_START(argc, argv);
  INITIALIZE_LLVM();

  // Initialize passes
  PassRegistry &Registry = *PassRegistry::getPassRegistry();
  initializeCore(Registry);
  initializeScalarOpts(Registry);
  initializeObjCARCOpts(Registry);
  initializeVectorization(Registry);
  initializeIPO(Registry);
  initializeAnalysis(Registry);
  initializeTransformUtils(Registry);
  initializeInstCombine(Registry);
  initializeInstrumentation(Registry);
  initializeTarget(Registry);
  // For codegen passes, only passes that do IR to IR transformation are
  // supported.
  initializeCodeGenPreparePass(Registry);
  initializeAtomicExpandPass(Registry);
  initializeRewriteSymbolsLegacyPassPass(Registry);
  initializeWinEHPreparePass(Registry);
  initializeDwarfEHPreparePass(Registry);
  initializeSjLjEHPreparePass(Registry);

  // Register Swift Only Passes.
  initializeSwiftAAWrapperPassPass(Registry);
  initializeSwiftRCIdentityPass(Registry);
  initializeSwiftARCOptPass(Registry);
  initializeSwiftARCContractPass(Registry);
  initializeInlineTreePrinterPass(Registry);
  initializeSwiftMergeFunctionsPass(Registry);

  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift LLVM optimizer\n");

  if (PrintStats)
    llvm::EnableStatistics();

  llvm::SMDiagnostic Err;

  // Load the input module...
  std::unique_ptr<Module> M =
      parseIRFile(InputFilename, Err, getGlobalLLVMContext());

  if (!M) {
    Err.print(argv[0], errs());
    return 1;
  }

  if (verifyModule(*M, &errs())) {
    errs() << argv[0] << ": " << InputFilename
           << ": error: input module is broken!\n";
    return 1;
  }

  // If we are supposed to override the target triple, do so now.
  if (!TargetTriple.empty())
    M->setTargetTriple(llvm::Triple::normalize(TargetTriple));

  // Figure out what stream we are supposed to write to...
  std::unique_ptr<llvm::ToolOutputFile> Out;
  // Default to standard output.
  if (OutputFilename.empty())
    OutputFilename = "-";

  std::error_code EC;
  Out.reset(
      new llvm::ToolOutputFile(OutputFilename, EC, llvm::sys::fs::F_None));
  if (EC) {
    errs() << EC.message() << '\n';
    return 1;
  }

  llvm::Triple ModuleTriple(M->getTargetTriple());
  std::string CPUStr, FeaturesStr;
  llvm::TargetMachine *Machine = nullptr;
  const llvm::TargetOptions Options = InitTargetOptionsFromCodeGenFlags();

  if (ModuleTriple.getArch()) {
    CPUStr = getCPUStr();
    FeaturesStr = getFeaturesStr();
    Machine = getTargetMachine(ModuleTriple, CPUStr, FeaturesStr, Options);
  }

  std::unique_ptr<llvm::TargetMachine> TM(Machine);

  // Override function attributes based on CPUStr, FeaturesStr, and command line
  // flags.
  setFunctionAttributes(CPUStr, FeaturesStr, *M);

  if (Optimized) {
    IRGenOptions Opts;
    Opts.OptMode = OptimizationMode::ForSpeed;

    // Then perform the optimizations.
    performLLVMOptimizations(Opts, M.get(), TM.get());
  } else {
    runSpecificPasses(argv[0], M.get(), TM.get(), ModuleTriple);
  }

  // Finally dump the output.
  dumpOutput(*M, Out->os());

  return 0;
}
