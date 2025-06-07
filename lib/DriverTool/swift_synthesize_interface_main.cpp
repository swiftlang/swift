//===--- swift_synthesize_interface_main.cpp - Swift interface synthesis --===//
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
//  Prints the synthesized Swift interface for a Clang module.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/Version.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/Option/Options.h"
#include "swift/Parse/ParseVersion.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace options;

int swift_synthesize_interface_main(ArrayRef<const char *> Args,
                                    const char *Argv0, void *MainAddr) {
  INITIALIZE_LLVM();

  CompilerInvocation Invocation;
  CompilerInstance CI;
  PrintingDiagnosticConsumer DiagPrinter;
  auto &Diags = CI.getDiags();
  Diags.addConsumer(DiagPrinter);

  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  unsigned MissingIndex;
  unsigned MissingCount;
  llvm::opt::InputArgList ParsedArgs = Table->ParseArgs(
      Args, MissingIndex, MissingCount, SwiftSynthesizeInterfaceOption);
  if (MissingCount) {
    Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                   ParsedArgs.getArgString(MissingIndex), MissingCount);
    return EXIT_FAILURE;
  }

  if (ParsedArgs.hasArg(OPT_UNKNOWN)) {
    for (const auto *A : ParsedArgs.filtered(OPT_UNKNOWN)) {
      Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                     A->getAsString(ParsedArgs));
    }
    return EXIT_FAILURE;
  }

  auto MainExecutablePath = llvm::sys::fs::getMainExecutable(Argv0, MainAddr);

  if (ParsedArgs.getLastArg(OPT_help) || Args.empty()) {
    std::string ExecutableName =
        llvm::sys::path::stem(MainExecutablePath).str();
    Table->printHelp(llvm::outs(), ExecutableName.c_str(),
                     "Swift Interface Synthesizer",
                     SwiftSynthesizeInterfaceOption, 0,
                     /*ShowAllAliases*/ false);
    return EXIT_FAILURE;
  }

  std::string ModuleName;
  if (auto *A = ParsedArgs.getLastArg(OPT_module_name)) {
    ModuleName = A->getValue();
  } else {
    Diags.diagnose(SourceLoc(), diag::error_option_required, "-module-name");
    return EXIT_FAILURE;
  }

  llvm::Triple Target;
  if (auto *A = ParsedArgs.getLastArg(OPT_target)) {
    Target = llvm::Triple(A->getValue());
  } else {
    Diags.diagnose(SourceLoc(), diag::error_option_required, "-target");
    return EXIT_FAILURE;
  }

  std::string OutputFile;
  if (auto *A = ParsedArgs.getLastArg(OPT_o)) {
    OutputFile = A->getValue();
  } else {
    OutputFile = "-";
  }

  Invocation.setMainExecutablePath(MainExecutablePath);
  Invocation.setModuleName("swift_synthesize_interface");

  if (auto *A = ParsedArgs.getLastArg(OPT_resource_dir)) {
    Invocation.setRuntimeResourcePath(A->getValue());
  }

  std::string SDK = "";
  if (auto *A = ParsedArgs.getLastArg(OPT_sdk)) {
    SDK = A->getValue();
  }
  Invocation.setSDKPath(SDK);
  Invocation.setTargetTriple(Target);

  for (const auto *A : ParsedArgs.filtered(OPT_Xcc)) {
    Invocation.getClangImporterOptions().ExtraArgs.push_back(A->getValue());
  }

  std::vector<SearchPathOptions::SearchPath> FrameworkSearchPaths;
  for (const auto *A : ParsedArgs.filtered(OPT_F)) {
    FrameworkSearchPaths.push_back({A->getValue(), /*isSystem*/ false});
  }
  for (const auto *A : ParsedArgs.filtered(OPT_Fsystem)) {
    FrameworkSearchPaths.push_back({A->getValue(), /*isSystem*/ true});
  }
  Invocation.setFrameworkSearchPaths(FrameworkSearchPaths);
  Invocation.getSearchPathOptions().LibrarySearchPaths =
      ParsedArgs.getAllArgValues(OPT_L);
  std::vector<SearchPathOptions::SearchPath> ImportSearchPaths;
  for (const auto *A : ParsedArgs.filtered(OPT_I)) {
    ImportSearchPaths.push_back({A->getValue(), /*isSystem*/ false});
  }
  for (const auto *A : ParsedArgs.filtered(OPT_Isystem)) {
    ImportSearchPaths.push_back({A->getValue(), /*isSystem*/ true});
  }
  Invocation.setImportSearchPaths(ImportSearchPaths);

  Invocation.getLangOptions().EnableObjCInterop = Target.isOSDarwin();
  Invocation.getLangOptions().setCxxInteropFromArgs(ParsedArgs, Diags,
                                                    Invocation.getFrontendOptions());

  std::string ModuleCachePath = "";
  if (auto *A = ParsedArgs.getLastArg(OPT_module_cache_path)) {
    ModuleCachePath = A->getValue();
  }
  Invocation.setClangModuleCachePath(ModuleCachePath);
  Invocation.getClangImporterOptions().ModuleCachePath = ModuleCachePath;
  Invocation.getClangImporterOptions().ImportForwardDeclarations = true;
  Invocation.setDefaultPrebuiltCacheIfNecessary();

  if (auto *A = ParsedArgs.getLastArg(OPT_swift_version)) {
    using version::Version;
    auto SwiftVersion = A->getValue();
    bool isValid = false;
    if (auto Version = VersionParser::parseVersionString(
            SwiftVersion, SourceLoc(), nullptr)) {
      if (auto Effective = Version.value().getEffectiveLanguageVersion()) {
        Invocation.getLangOptions().EffectiveLanguageVersion = *Effective;
        isValid = true;
      }
    }
    if (!isValid) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     "-swift-version", SwiftVersion);
      return EXIT_FAILURE;
    }
  }

  std::string InstanceSetupError;
  if (CI.setup(Invocation, InstanceSetupError)) {
    llvm::outs() << InstanceSetupError << '\n';
    return EXIT_FAILURE;
  }

  auto M = CI.getASTContext().getModuleByName(ModuleName);
  if (!M) {
    llvm::errs() << "Couldn't load module '" << ModuleName << '\''
                 << " in the current SDK and search paths.\n";

    SmallVector<Identifier, 32> VisibleModuleNames;
    CI.getASTContext().getVisibleTopLevelModuleNames(VisibleModuleNames);

    if (VisibleModuleNames.empty()) {
      llvm::errs() << "Could not find any modules.\n";
    } else {
      std::sort(VisibleModuleNames.begin(), VisibleModuleNames.end(),
                [](const Identifier &A, const Identifier &B) -> bool {
                  return A.str() < B.str();
                });
      llvm::errs() << "Current visible modules:\n";
      for (const auto &ModuleName : VisibleModuleNames) {
        llvm::errs() << ModuleName.str() << "\n";
      }
    }
    return EXIT_FAILURE;
  }

  if (M->failedToLoad()) {
    llvm::errs() << "Error: Failed to load the module '" << ModuleName
                 << "'. Are you missing build dependencies or "
                    "include/framework directories?\n"
                 << "See the previous error messages for details. Aborting.\n";

    return EXIT_FAILURE;
  }

  std::error_code EC;
  llvm::raw_fd_ostream fs(OutputFile, EC);
  if (EC) {
    llvm::errs() << "Cannot open output file: " << OutputFile << "\n";
    return EXIT_FAILURE;
  }

  PrintOptions printOpts =
      PrintOptions::printModuleInterface(/*printFullConvention=*/true);
  if (ParsedArgs.hasArg(OPT_print_fully_qualified_types)) {
    printOpts.FullyQualifiedTypes = true;
  }

  swift::OptionSet<swift::ide::ModuleTraversal> traversalOpts = std::nullopt;
  if (ParsedArgs.hasArg(OPT_include_submodules)) {
    traversalOpts = swift::ide::ModuleTraversal::VisitSubmodules;
  }

  StreamPrinter printer(fs);
  ide::printModuleInterface(M, /*GroupNames=*/{}, traversalOpts, printer,
                            printOpts, /*PrintSynthesizedExtensions=*/false);

  return EXIT_SUCCESS;
}
