//===--- SILOpt.cpp - SIL Optimization Driver -----------------------------===//
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
//
// This is a tool for reading sil files and running sil passes on them. The
// targeted usecase is debugging and testing SIL passes.
//
//===----------------------------------------------------------------------===//

#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/SILOptions.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/LLVMContext.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/IRGen/IRGenPublic.h"
#include "swift/IRGen/IRGenSILPasses.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include <cstdio>
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

static llvm::cl::opt<bool>
EnableResilience("enable-resilience",
                 llvm::cl::desc("Compile the module to export resilient "
                                "interfaces for all public declarations by "
                                "default"));

static llvm::cl::opt<bool>
EnableSILOwnershipOpt("enable-sil-ownership",
                 llvm::cl::desc("Compile the module with sil-ownership initially enabled for all functions"));

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
                                "Run performance passes")),
    llvm::cl::init(OptGroup::Unknown));

static llvm::cl::list<PassKind>
Passes(llvm::cl::desc("Passes:"),
       llvm::cl::values(
#define PASS(ID, NAME, DESCRIPTION) clEnumValN(PassKind::ID, NAME, DESCRIPTION),
#include "swift/SILOptimizer/PassManager/Passes.def"
       clEnumValN(0, "", "")));

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
EnableSILSortOutput("emit-sorted-sil", llvm::cl::Hidden,
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

static llvm::cl::opt<bool>
PerformWMO("wmo", llvm::cl::desc("Enable whole-module optimizations"));

static llvm::cl::opt<bool>
AssumeUnqualifiedOwnershipWhenParsing(
    "assume-parsing-unqualified-ownership-sil", llvm::cl::Hidden, llvm::cl::init(false),
    llvm::cl::desc("Assume all parsed functions have unqualified ownership"));

static void runCommandLineSelectedPasses(SILModule *Module,
                                         irgen::IRGenModule *IRGenMod) {
  SILPassManager PM(Module, IRGenMod);
  for (auto P : Passes) {
#define PASS(ID, Name, Description)
#define IRGEN_PASS(ID, Name, Description)                                      \
  if (P == PassKind::ID)                                                       \
    PM.registerIRGenPass(swift::PassKind::ID, irgen::create##ID());
#include "swift/SILOptimizer/PassManager/Passes.def"
  }

  PM.executePassPipelinePlan(
      SILPassPipelinePlan::getPassPipelineForKinds(Passes));

  if (Module->getOptions().VerifyAll)
    Module->verify();
}

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// getMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement getMainExecutable
// without being given the address of a function in the main executable).
void anchorForGetMainExecutable() {}

int main(int argc, char **argv) {
  INITIALIZE_LLVM(argc, argv);

  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift SIL optimizer\n");

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
  Invocation.getFrontendOptions().EnableResilience = EnableResilience;
  // Set the module cache path. If not passed in we use the default swift module
  // cache.
  Invocation.getClangImporterOptions().ModuleCachePath = ModuleCachePath;
  Invocation.setParseStdlib();
  Invocation.getLangOptions().DisableAvailabilityChecking = true;
  Invocation.getLangOptions().EnableAccessControl = false;
  Invocation.getLangOptions().EnableObjCAttrRequiresFoundation = false;
  Invocation.getLangOptions().EnableObjCInterop =
    llvm::Triple(Target).isOSDarwin();

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
  if (OptimizationGroup != OptGroup::Diagnostics)
    SILOpts.Optimization = SILOptions::SILOptMode::Optimize;
  SILOpts.EnableSILOwnership = EnableSILOwnershipOpt;
  SILOpts.AssumeUnqualifiedOwnershipWhenParsing =
    AssumeUnqualifiedOwnershipWhenParsing;

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

  if (!PerformWMO) {
    auto &FrontendOpts = Invocation.getFrontendOptions();
    if (!InputFilename.empty() && InputFilename != "-") {
      FrontendOpts.PrimaryInput = SelectedInput(
          FrontendOpts.InputFilenames.size());
    } else {
      FrontendOpts.PrimaryInput = SelectedInput(
          FrontendOpts.InputBuffers.size(), SelectedInput::InputKind::Buffer);
    }
  }

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
    CI.setSILModule(SILModule::createEmptyModule(
        CI.getMainModule(), CI.getSILOptions(), PerformWMO));
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
    auto *SILMod = CI.getSILModule();
    {
      auto T = irgen::createIRGenModule(SILMod, getGlobalLLVMContext());
      runCommandLineSelectedPasses(SILMod, T.second);
      irgen::deleteIRGenModule(T);
    }
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

    if (OutputFile == "-") {
      CI.getSILModule()->print(llvm::outs(), EmitVerboseSIL, CI.getMainModule(),
                               EnableSILSortOutput, !DisableASTDump);
    } else {
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
  }

  bool HadError = CI.getASTContext().hadError();

  // If we're in -verify mode, we've buffered up all of the generated
  // diagnostics.  Check now to ensure that they meet our expectations.
  if (VerifyMode) {
    HadError = verifyDiagnostics(CI.getSourceMgr(), CI.getInputBufferIDs(),
                                 /*autoApplyFixes*/false);
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
