//===--- swift_indent_main.cpp - Swift code formatting tool ---------------===//
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
//  Extracts a Symbol Graph from a .swiftmodule file.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/Version.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Option/Options.h"
#include "swift/Parse/ParseVersion.h"
#include "swift/SymbolGraphGen/SymbolGraphGen.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace options;

int swift_symbolgraph_extract_main(ArrayRef<const char *> Args,
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
      Args, MissingIndex, MissingCount, SwiftSymbolGraphExtractOption);
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
                     "Swift Symbol Graph Extractor",
                     SwiftSymbolGraphExtractOption, 0,
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
    Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
  }

  std::string OutputDir;
  if (auto *A = ParsedArgs.getLastArg(OPT_output_dir)) {
    OutputDir = A->getValue();
  } else {
    Diags.diagnose(SourceLoc(), diag::error_option_required, "-output-dir");
    return EXIT_FAILURE;
  }

  if (!llvm::sys::fs::is_directory(OutputDir)) {
    Diags.diagnose(SourceLoc(), diag::error_nonexistent_output_dir, OutputDir);
    return EXIT_FAILURE;
  }

  Invocation.setMainExecutablePath(MainExecutablePath);
  Invocation.setModuleName("swift_symbolgraph_extract");

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
  Invocation.getLangOptions().DebuggerSupport = true;

  Invocation.getFrontendOptions().EnableLibraryEvolution = true;

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

  SmallVector<StringRef, 4> AllowedRexports;
  if (auto *A =
          ParsedArgs.getLastArg(OPT_experimental_allowed_reexported_modules)) {
    for (const auto *val : A->getValues())
      AllowedRexports.emplace_back(val);
  }

  symbolgraphgen::SymbolGraphOptions Options;
  Options.OutputDir = OutputDir;
  Options.Target = Target;
  Options.PrettyPrint = ParsedArgs.hasArg(OPT_pretty_print);
  Options.EmitSynthesizedMembers = !ParsedArgs.hasArg(OPT_skip_synthesized_members);
  Options.PrintMessages = ParsedArgs.hasArg(OPT_v);
  Options.SkipInheritedDocs = ParsedArgs.hasArg(OPT_skip_inherited_docs);
  Options.SkipProtocolImplementations = ParsedArgs.hasArg(OPT_skip_protocol_implementations);
  Options.IncludeSPISymbols = ParsedArgs.hasArg(OPT_include_spi_symbols);
  Options.EmitExtensionBlockSymbols =
      ParsedArgs.hasFlag(OPT_emit_extension_block_symbols,
                         OPT_omit_extension_block_symbols, /*default=*/false);
  Options.AllowedReexportedModules = AllowedRexports;

  if (auto *A = ParsedArgs.getLastArg(OPT_minimum_access_level)) {
    Options.MinimumAccessLevel =
        llvm::StringSwitch<AccessLevel>(A->getValue())
            .Case("open", AccessLevel::Open)
            .Case("public", AccessLevel::Public)
            .Case("package", AccessLevel::Package)
            .Case("internal", AccessLevel::Internal)
            .Case("fileprivate", AccessLevel::FilePrivate)
            .Case("private", AccessLevel::Private)
            .Default(AccessLevel::Public);
  }

  if (auto *A = ParsedArgs.getLastArg(OPT_allow_availability_platforms,
        OPT_block_availability_platforms)) {
    llvm::SmallVector<StringRef> AvailabilityPlatforms;
    StringRef(A->getValue())
        .split(AvailabilityPlatforms, ',', /*MaxSplits*/ -1,
               /*KeepEmpty*/ false);
    Options.AvailabilityPlatforms = llvm::DenseSet<StringRef>(
        AvailabilityPlatforms.begin(), AvailabilityPlatforms.end());
    Options.AvailabilityIsBlockList = A->getOption().matches(OPT_block_availability_platforms);
  }

  Invocation.getLangOptions().setCxxInteropFromArgs(ParsedArgs, Diags,
                                                    Invocation.getFrontendOptions());

  std::string InstanceSetupError;
  if (CI.setup(Invocation, InstanceSetupError)) {
    llvm::outs() << InstanceSetupError << '\n';
    return EXIT_FAILURE;
  }

  auto M = CI.getASTContext().getModuleByName(ModuleName);
  if (!M) {
    llvm::errs()
      << "Couldn't load module '" << ModuleName << '\''
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

  FileUnitKind expectedKind = FileUnitKind::SerializedAST;
  if (M->isNonSwiftModule())
    expectedKind = FileUnitKind::ClangModule;
  const auto &MainFile = M->getMainFile(expectedKind);
  
  if (Options.PrintMessages)
    llvm::errs() << "Emitting symbol graph for module file: " << MainFile.getModuleDefiningPath() << '\n';
  
  int Success = symbolgraphgen::emitSymbolGraphForModule(M, Options);
  
  // Look for cross-import overlays that the given module imports.
  
  // Clear out the diagnostic printer before looking for cross-import overlay modules,
  // since some SDK modules can cause errors in the getModuleByName() call. The call
  // itself will properly return nullptr after this failure, so for our purposes we
  // don't need to print these errors.
  CI.removeDiagnosticConsumer(&DiagPrinter);
  
  SmallVector<ModuleDecl *> Overlays;
  M->findDeclaredCrossImportOverlaysTransitive(Overlays);
  for (const auto *OM : Overlays) {
    auto CIM = CI.getASTContext().getModuleByName(OM->getNameStr());
    if (CIM) {
      const auto &CIMainFile = CIM->getMainFile(FileUnitKind::SerializedAST);
      
      if (Options.PrintMessages)
        llvm::errs() << "Emitting symbol graph for cross-import overlay module file: "
          << CIMainFile.getModuleDefiningPath() << '\n';
      
      Success |= symbolgraphgen::emitSymbolGraphForModule(CIM, Options);
    }
  }

  return Success;
}
