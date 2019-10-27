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

#include "swift/Subsystems.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/SILOptions.h"
#include "swift/Basic/FileTypes.h"
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
#include "llvm/Support/YAMLTraits.h"
#include <cstdio>
using namespace swift;

namespace cl = llvm::cl;

namespace {

enum class OptGroup { Unknown, Diagnostics, Performance, Lowering };

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
EnableLibraryEvolution("enable-library-evolution",
                       llvm::cl::desc("Compile the module to export resilient "
                                      "interfaces for all public declarations by "
                                      "default"));

static llvm::cl::opt<bool> DisableSILOwnershipVerifier(
    "disable-sil-ownership-verifier",
    llvm::cl::desc(
        "Do not verify SIL ownership invariants during SIL verification"));

static llvm::cl::opt<bool> EnableOwnershipLoweringAfterDiagnostics(
    "enable-ownership-lowering-after-diagnostics",
    llvm::cl::desc("Enable ownership lowering after diagnostics"),
    llvm::cl::init(false));

static llvm::cl::opt<bool>
EnableSILOpaqueValues("enable-sil-opaque-values",
                      llvm::cl::desc("Compile the module with sil-opaque-values enabled."));

static llvm::cl::opt<bool>
EnableObjCInterop("enable-objc-interop",
                  llvm::cl::desc("Enable Objective-C interoperability."));

static llvm::cl::opt<bool>
DisableObjCInterop("disable-objc-interop",
                   llvm::cl::desc("Disable Objective-C interoperability."));

static llvm::cl::opt<bool>
VerifyExclusivity("enable-verify-exclusivity",
                  llvm::cl::desc("Verify the access markers used to enforce exclusivity."));

namespace {
enum EnforceExclusivityMode {
  Unchecked, // static only
  Checked,   // static and dynamic
  DynamicOnly,
  None
};
} // end anonymous namespace

static cl::opt<EnforceExclusivityMode> EnforceExclusivity(
  "enforce-exclusivity", cl::desc("Enforce law of exclusivity "
                                  "(and support memory access markers)."),
    cl::init(EnforceExclusivityMode::Checked),
    cl::values(clEnumValN(EnforceExclusivityMode::Unchecked, "unchecked",
                          "Static checking only."),
               clEnumValN(EnforceExclusivityMode::Checked, "checked",
                          "Static and dynamic checking."),
               clEnumValN(EnforceExclusivityMode::DynamicOnly, "dynamic-only",
                          "Dynamic checking only."),
               clEnumValN(EnforceExclusivityMode::None, "none",
                          "No exclusivity checking.")));

static llvm::cl::opt<std::string>
ResourceDir("resource-dir",
    llvm::cl::desc("The directory that holds the compiler resource files"));

static llvm::cl::opt<std::string>
SDKPath("sdk", llvm::cl::desc("The path to the SDK for use with the clang "
                              "importer."),
        llvm::cl::init(""));

static llvm::cl::opt<std::string>
Target("target", llvm::cl::desc("target triple"),
       llvm::cl::init(llvm::sys::getDefaultTargetTriple()));

static llvm::cl::opt<OptGroup> OptimizationGroup(
    llvm::cl::desc("Predefined optimization groups:"),
    llvm::cl::values(
        clEnumValN(OptGroup::Diagnostics, "diagnostics",
                   "Run diagnostic passes"),
        clEnumValN(OptGroup::Performance, "O", "Run performance passes"),
        clEnumValN(OptGroup::Lowering, "lowering", "Run lowering passes")),
    llvm::cl::init(OptGroup::Unknown));

static llvm::cl::list<PassKind>
Passes(llvm::cl::desc("Passes:"),
       llvm::cl::values(
#define PASS(ID, TAG, NAME) clEnumValN(PassKind::ID, TAG, NAME),
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

static llvm::cl::opt<bool>
DisableSILLinking("disable-sil-linking",
                  llvm::cl::desc("Disable SIL linking"));

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

static llvm::cl::opt<bool>
PerformWMO("wmo", llvm::cl::desc("Enable whole-module optimizations"));

static llvm::cl::opt<bool>
EnableExperimentalStaticAssert(
    "enable-experimental-static-assert", llvm::cl::Hidden,
    llvm::cl::init(false), llvm::cl::desc("Enable experimental #assert"));

/// Regular expression corresponding to the value given in one of the
/// -pass-remarks* command line flags. Passes whose name matches this regexp
/// will emit a diagnostic.
static std::shared_ptr<llvm::Regex> createOptRemarkRegex(StringRef Val) {
  std::shared_ptr<llvm::Regex> Pattern = std::make_shared<llvm::Regex>(Val);
  if (!Val.empty()) {
    std::string RegexError;
    if (!Pattern->isValid(RegexError))
      llvm::report_fatal_error("Invalid regular expression '" + Val +
                                   "' in -sil-remarks: " + RegexError,
                               false);
  }
  return Pattern;
}

static cl::opt<std::string> PassRemarksPassed(
    "sil-remarks", cl::value_desc("pattern"),
    cl::desc(
        "Enable performed optimization remarks from passes whose name match "
        "the given regular expression"),
    cl::Hidden);

static cl::opt<std::string> PassRemarksMissed(
    "sil-remarks-missed", cl::value_desc("pattern"),
    cl::desc("Enable missed optimization remarks from passes whose name match "
             "the given regular expression"),
    cl::Hidden);

static cl::opt<std::string>
    RemarksFilename("save-optimization-record-path",
                    cl::desc("YAML output filename for pass remarks"),
                    cl::value_desc("filename"));

static void runCommandLineSelectedPasses(SILModule *Module,
                                         irgen::IRGenModule *IRGenMod) {
  SILPassManager PM(Module, IRGenMod);
  for (auto P : Passes) {
#define PASS(ID, Tag, Name)
#define IRGEN_PASS(ID, Tag, Name)                                              \
  if (P == PassKind::ID)                                                       \
    PM.registerIRGenPass(swift::PassKind::ID, irgen::create##ID());
#include "swift/SILOptimizer/PassManager/Passes.def"
  }

  PM.executePassPipelinePlan(SILPassPipelinePlan::getPassPipelineForKinds(
      Module->getOptions(), Passes));

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
  PROGRAM_START(argc, argv);
  INITIALIZE_LLVM();

  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift SIL optimizer\n");

  if (PrintStats)
    llvm::EnableStatistics();

  CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(argv[0],
          reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  // Give the context the list of search paths to use for modules.
  Invocation.setImportSearchPaths(ImportPaths);
  std::vector<SearchPathOptions::FrameworkSearchPath> FramePaths;
  for (const auto &path : FrameworkPaths) {
    FramePaths.push_back({path, /*isSystem=*/false});
  }
  Invocation.setFrameworkSearchPaths(FramePaths);
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
  Invocation.getFrontendOptions().EnableLibraryEvolution
    = EnableLibraryEvolution;
  // Set the module cache path. If not passed in we use the default swift module
  // cache.
  Invocation.getClangImporterOptions().ModuleCachePath = ModuleCachePath;
  Invocation.setParseStdlib();
  Invocation.getLangOptions().DisableAvailabilityChecking = true;
  Invocation.getLangOptions().EnableAccessControl = false;
  Invocation.getLangOptions().EnableObjCAttrRequiresFoundation = false;
  Invocation.getLangOptions().EnableObjCInterop =
    EnableObjCInterop ? true :
    DisableObjCInterop ? false : llvm::Triple(Target).isOSDarwin();

  Invocation.getLangOptions().EnableSILOpaqueValues = EnableSILOpaqueValues;

  Invocation.getLangOptions().OptimizationRemarkPassedPattern =
      createOptRemarkRegex(PassRemarksPassed);
  Invocation.getLangOptions().OptimizationRemarkMissedPattern =
      createOptRemarkRegex(PassRemarksMissed);

  Invocation.getLangOptions().EnableExperimentalStaticAssert =
      EnableExperimentalStaticAssert;

  // Setup the SIL Options.
  SILOptions &SILOpts = Invocation.getSILOptions();
  SILOpts.InlineThreshold = SILInlineThreshold;
  SILOpts.VerifyAll = EnableSILVerifyAll;
  SILOpts.RemoveRuntimeAsserts = RemoveRuntimeAsserts;
  SILOpts.AssertConfig = AssertConfId;
  if (OptimizationGroup != OptGroup::Diagnostics)
    SILOpts.OptMode = OptimizationMode::ForSpeed;
  SILOpts.VerifySILOwnership = !DisableSILOwnershipVerifier;
  SILOpts.StripOwnershipAfterSerialization =
      EnableOwnershipLoweringAfterDiagnostics;

  SILOpts.VerifyExclusivity = VerifyExclusivity;
  if (EnforceExclusivity.getNumOccurrences() != 0) {
    switch (EnforceExclusivity) {
    case EnforceExclusivityMode::Unchecked:
      // This option is analogous to the -Ounchecked optimization setting.
      // It will disable dynamic checking but still diagnose statically.
      SILOpts.EnforceExclusivityStatic = true;
      SILOpts.EnforceExclusivityDynamic = false;
      break;
    case EnforceExclusivityMode::Checked:
      SILOpts.EnforceExclusivityStatic = true;
      SILOpts.EnforceExclusivityDynamic = true;
      break;
    case EnforceExclusivityMode::DynamicOnly:
      // This option is intended for staging purposes. The intent is that
      // it will eventually be removed.
      SILOpts.EnforceExclusivityStatic = false;
      SILOpts.EnforceExclusivityDynamic = true;
      break;
    case EnforceExclusivityMode::None:
      // This option is for staging purposes.
      SILOpts.EnforceExclusivityStatic = false;
      SILOpts.EnforceExclusivityDynamic = false;
      break;
    }
  }

  serialization::ExtendedValidationInfo extendedInfo;
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      Invocation.setUpInputForSILTool(InputFilename, ModuleName,
                                      /*alwaysSetModuleToMain*/ false,
                                      /*bePrimary*/ !PerformWMO, extendedInfo);
  if (!FileBufOrErr) {
    fprintf(stderr, "Error! Failed to open file: %s\n", InputFilename.c_str());
    exit(-1);
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
  if (Invocation.hasSerializedAST()) {
    assert(!CI.hasSILModule() &&
           "performSema() should not create a SILModule.");
    CI.createSILModule();
    std::unique_ptr<SerializedSILLoader> SL = SerializedSILLoader::create(
        CI.getASTContext(), CI.getSILModule(), nullptr);

    if (extendedInfo.isSIB() || DisableSILLinking)
      SL->getAllForModule(CI.getMainModule()->getName(), nullptr);
    else
      SL->getAll();
  }

  // If we're in verify mode, install a custom diagnostic handling for
  // SourceMgr.
  if (VerifyMode)
    enableDiagnosticVerifier(CI.getSourceMgr());

  if (CI.getSILModule())
    CI.getSILModule()->setSerializeSILAction([]{});

  std::unique_ptr<llvm::raw_fd_ostream> OptRecordFile;
  if (RemarksFilename != "") {
    std::error_code EC;
    OptRecordFile = llvm::make_unique<llvm::raw_fd_ostream>(
        RemarksFilename, EC, llvm::sys::fs::F_None);
    if (EC) {
      llvm::errs() << EC.message() << '\n';
      return 1;
    }
    auto Stream = llvm::make_unique<llvm::yaml::Output>(*OptRecordFile,
                                                        &CI.getSourceMgr());
    CI.getSILModule()->setOptRecordStream(std::move(Stream),
                                          std::move(OptRecordFile));
  }

  if (OptimizationGroup == OptGroup::Diagnostics) {
    runSILDiagnosticPasses(*CI.getSILModule());
  } else if (OptimizationGroup == OptGroup::Performance) {
    runSILOptPreparePasses(*CI.getSILModule());
    runSILOptimizationPasses(*CI.getSILModule());
  } else if (OptimizationGroup == OptGroup::Lowering) {
    runSILLoweringPasses(*CI.getSILModule());
  } else {
    auto *SILMod = CI.getSILModule();
    {
      auto T = irgen::createIRGenModule(
          SILMod, Invocation.getOutputFilenameForAtMostOnePrimary(),
          Invocation.getMainInputFilenameForDebugInfoForAtMostOnePrimary(),
          getGlobalLLVMContext());
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
      llvm::sys::path::replace_extension(
          OutputFile, file_types::getExtension(file_types::TY_SIB));
    } else {
      OutputFile = CI.getMainModule()->getName().str();
      llvm::sys::path::replace_extension(
          OutputFile, file_types::getExtension(file_types::TY_SIB));
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
                                 /*autoApplyFixes*/false,
                                 /*ignoreUnknown*/false);
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
