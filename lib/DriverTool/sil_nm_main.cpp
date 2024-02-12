//===--- sil_nm_main.cpp --------------------------------------------------===//
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
/// This utility is a command line tool that given a sil or sib file dumps out
/// the names of the functions, globals, vtables, and witness tables in a
/// machine readable form. The intention is that it can be used with things like
/// sil-func-extractor to manipulate sil from the commandline.
///
//===----------------------------------------------------------------------===//

#include "swift/Demangling/Demangle.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/Range.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include <cstdio>
#include <functional>

using namespace swift;

struct SILNMOptions {
  llvm::cl::opt<std::string>
    InputFilename = llvm::cl::opt<std::string>(llvm::cl::desc("input file"),
                                               llvm::cl::init("-"),
                                               llvm::cl::Positional);

  llvm::cl::list<std::string>
    ImportPaths = llvm::cl::list<std::string>("I",
                  llvm::cl::desc("add a directory to the import search path"));

  llvm::cl::opt<std::string>
    ModuleName = llvm::cl::opt<std::string>("module-name",
                 llvm::cl::desc("The name of the module if processing"
                                " a module. Necessary for processing "
                                "stdin."));

  llvm::cl::opt<bool>
    DemangleNames = llvm::cl::opt<bool>("demangle",
                    llvm::cl::desc("Demangle names of entities outputted"));

  llvm::cl::opt<std::string>
    ModuleCachePath = llvm::cl::opt<std::string>("module-cache-path",
                      llvm::cl::desc("Clang module cache path"));

  llvm::cl::opt<std::string>
    ResourceDir = llvm::cl::opt<std::string>(
      "resource-dir",
      llvm::cl::desc("The directory that holds the compiler resource files"));

  llvm::cl::opt<std::string>
    SDKPath = llvm::cl::opt<std::string>("sdk", llvm::cl::desc("The path to the SDK for use with the clang "
                                    "importer."),
              llvm::cl::init(""));

  llvm::cl::opt<std::string>
    Triple = llvm::cl::opt<std::string>("target", llvm::cl::desc("target triple"));
};

static void printAndSortNames(std::vector<StringRef> &Names, char Code,
                              const SILNMOptions &options) {
  std::sort(Names.begin(), Names.end());
  for (StringRef N : Names) {
    llvm::outs() << Code << " ";
    if (options.DemangleNames) {
      llvm::outs() << swift::Demangle::demangleSymbolAsString(N);
    } else {
      llvm::outs() << N;
    }
    llvm::outs() << '\n';
  }
}

static void nmModule(SILModule *M, const SILNMOptions &options) {
  {
    std::vector<StringRef> FuncNames;
    for (SILFunction &f : *M) {
      FuncNames.push_back(f.getName());
    }
    printAndSortNames(FuncNames, 'F', options);
  }

  {
    std::vector<StringRef> GlobalNames;
    for (SILGlobalVariable &g : M->getSILGlobals()) {
      GlobalNames.push_back(g.getName());
    }
    printAndSortNames(GlobalNames, 'G', options);
  }

  {
    std::vector<StringRef> WitnessTableNames;
    for (SILWitnessTable &wt : M->getWitnessTables()) {
      WitnessTableNames.push_back(wt.getName());
    }
    printAndSortNames(WitnessTableNames, 'W', options);
  }

  {
    std::vector<StringRef> VTableNames;
    for (SILVTable *vt : M->getVTables()) {
      VTableNames.push_back(vt->getClass()->getName().str());
    }
    printAndSortNames(VTableNames, 'V', options);
  }
}

int sil_nm_main(ArrayRef<const char *> argv, void *MainAddr) {
  INITIALIZE_LLVM();

  SILNMOptions options;

  llvm::cl::ParseCommandLineOptions(argv.size(), argv.data(), "SIL NM\n");

  CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(llvm::sys::fs::getMainExecutable(argv[0], MainAddr));

  // Give the context the list of search paths to use for modules.
  Invocation.setImportSearchPaths(options.ImportPaths);
  // Set the SDK path and target if given.
  if (options.SDKPath.getNumOccurrences() == 0) {
    const char *SDKROOT = getenv("SDKROOT");
    if (SDKROOT)
      options.SDKPath = SDKROOT;
  }
  if (!options.SDKPath.empty())
    Invocation.setSDKPath(options.SDKPath);
  if (!options.Triple.empty())
    Invocation.setTargetTriple(options.Triple);
  if (!options.ResourceDir.empty())
    Invocation.setRuntimeResourcePath(options.ResourceDir);
  Invocation.getClangImporterOptions().ModuleCachePath = options.ModuleCachePath;
  Invocation.setParseStdlib();
  Invocation.getLangOptions().DisableAvailabilityChecking = true;
  Invocation.getLangOptions().EnableAccessControl = false;
  Invocation.getLangOptions().EnableObjCAttrRequiresFoundation = false;

  serialization::ExtendedValidationInfo extendedInfo;
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      Invocation.setUpInputForSILTool(options.InputFilename, options.ModuleName,
                                      /*alwaysSetModuleToMain*/ true,
                                      /*bePrimary*/ false, extendedInfo);
  if (!FileBufOrErr) {
    fprintf(stderr, "Error! Failed to open file: %s\n", options.InputFilename.c_str());
    exit(-1);
  }

  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  std::string InstanceSetupError;
  if (CI.setup(Invocation, InstanceSetupError)) {
    llvm::errs() << InstanceSetupError << '\n';
    return 1;
  }
  CI.performSema();

  // If parsing produced an error, don't run any passes.
  if (CI.getASTContext().hadError())
    return 1;

  auto SILMod = performASTLowering(CI.getMainModule(), CI.getSILTypes(),
                                   CI.getSILOptions());
  nmModule(SILMod.get(), options);

  return CI.getASTContext().hadError();
}
