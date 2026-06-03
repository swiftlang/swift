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
#include "swift/AST/AttrKind.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Version.h"
#include "swift/Driver/PluginPaths.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/Option/Options.h"
#include "swift/Parse/ParseVersion.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Path.h"
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
    std::string ExecutableName = llvm::sys::path::stem(Argv0).str();
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
    Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
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

  // Add default toolchain-relative plugin paths.
  SmallString<261> toolchainRoot{MainExecutablePath};
  llvm::sys::path::remove_filename(toolchainRoot); // remove executable name
  llvm::sys::path::remove_filename(toolchainRoot); // remove 'bin'

  SearchPathOptions &SearchPathOpts = Invocation.getSearchPathOptions();

  SmallString<261> inProcPluginServerPath;
  driver::appendInProcPluginServerPath(toolchainRoot, inProcPluginServerPath);
  SearchPathOpts.InProcessPluginServerPath =
      std::string(inProcPluginServerPath);

  SmallString<261> defaultPluginPath;
  driver::appendPluginsPath(toolchainRoot, defaultPluginPath);
  SearchPathOpts.PluginSearchOpts.emplace_back(
      PluginSearchOption::PluginPath{std::string(defaultPluginPath)});

#if defined(__APPLE__) || defined(__unix__)
  SmallString<261> localPluginPath;
  driver::appendLocalPluginsPath(toolchainRoot, localPluginPath);
  SearchPathOpts.PluginSearchOpts.emplace_back(
      PluginSearchOption::PluginPath{std::string(localPluginPath)});
#endif

  Invocation.getLangOptions().EnableObjCInterop = Target.isOSDarwin();
  Invocation.getLangOptions().AttachCommentsToDecls = true;
  Invocation.getLangOptions().setCxxInteropFromArgs(
      ParsedArgs, Diags, Invocation.getFrontendOptions());
  Invocation.computeCXXStdlibOptions();

  if (ParsedArgs.hasArg(OPT_disable_safe_interop_wrappers))
    Invocation.getLangOptions().DisableSafeInteropWrappers = true;

  if (parseFeatureArgs(Invocation.getLangOptions(), ParsedArgs, Diags))
    return EXIT_FAILURE;

  std::string ModuleCachePath = "";
  if (auto *A = ParsedArgs.getLastArg(OPT_module_cache_path)) {
    ModuleCachePath = A->getValue();
  }
  Invocation.setClangModuleCachePath(ModuleCachePath);
  Invocation.getClangImporterOptions().ModuleCachePath = ModuleCachePath;
  Invocation.getClangImporterOptions().ImportForwardDeclarations = true;
  Invocation.setDefaultPrebuiltCacheIfNecessary();

  if (auto *A = ParsedArgs.getLastArg(OPT_language_mode)) {
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
    llvm::errs() << InstanceSetupError << '\n';
    return EXIT_FAILURE;
  }

  (void)CI.getMainModule(); // clang modules inherit default imports from main
                            // module
  auto M = CI.getASTContext().getModuleByName(ModuleName);
  if (!M) {
    llvm::errs() << "Couldn't load module '" << ModuleName << '\''
                 << " in the current SDK and search paths.\n";

    bool Verbose = ParsedArgs.hasArg(OPT_v);

    SmallVector<Identifier, 32> VisibleModuleNames;
    CI.getASTContext().getVisibleTopLevelModuleNames(VisibleModuleNames);

    if (VisibleModuleNames.empty()) {
      if (Verbose)
        llvm::errs() << "Could not find any modules.\n";
    } else {
      const unsigned Threshold = 3;
      SmallVector<std::pair<unsigned, StringRef>, 4> Suggestions;
      for (const auto &Name : VisibleModuleNames) {
        unsigned D = StringRef(ModuleName)
                         .edit_distance_insensitive(
                             Name.str(), /*AllowReplacements=*/true,
                             /*MaxEditDistance=*/Threshold);
        if (D <= Threshold)
          Suggestions.push_back({D, Name.str()});
      }
      std::sort(Suggestions.begin(), Suggestions.end(),
                [](const auto &A, const auto &B) {
                  if (A.first != B.first)
                    return A.first < B.first;
                  return A.second < B.second;
                });
      const unsigned MaxSuggestions = 5;
      if (Suggestions.size() > MaxSuggestions)
        Suggestions.resize(MaxSuggestions);

      if (!Suggestions.empty()) {
        llvm::errs() << "Did you mean:\n";
        for (const auto &S : Suggestions)
          llvm::errs() << "  " << S.second << "\n";
      }

      if (Verbose) {
        std::sort(VisibleModuleNames.begin(), VisibleModuleNames.end(),
                  [](const Identifier &A, const Identifier &B) -> bool {
                    return A.str() < B.str();
                  });
        llvm::errs() << "Current visible modules:\n";
        for (const auto &Name : VisibleModuleNames) {
          llvm::errs() << "  " << Name.str() << "\n";
        }
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

  // Show user-facing synthesized members but do not show other implicit decls
  printOpts.SkipImplicit = true;
  printOpts.AlwaysPrintSynthesized = true;

  // -synthesized-interface-show=<list> and -minimum-access-level
  //
  // Items within a single comma-separated list are additive. Repeated
  // -synthesized-interface-show= invocations follow last-wins semantics:
  // each invocation resets the show state before applying its items.
  std::optional<AccessLevel> MinAccessLevel;
  struct Opts {
    bool ShowUnavailable = false;
    bool ShowImplicitAttrs = false;
    bool ShowQualifiedTypes = false;
    bool ShowCompilerInternals = false;

    bool apply(StringRef name) {
      if (name == "unavailable") {
        ShowUnavailable = true;
        return true;
      }
      if (name == "implicit-attrs") {
        ShowImplicitAttrs = true;
        return true;
      }
      if (name == "qualified-types") {
        ShowQualifiedTypes = true;
        return true;
      }
      if (name == "minimal") {
        return true;
      }
      if (name == "all") {
        ShowUnavailable = true;
        ShowImplicitAttrs = true;
        ShowQualifiedTypes = true;
        return true;
      }
      if (name == "compiler-internal-details") {
        ShowUnavailable = true;
        ShowImplicitAttrs = true;
        ShowQualifiedTypes = true;
        ShowCompilerInternals = true;
        return true;
      }
      return false;
    }

    static bool isShorthand(StringRef item) {
      return item == "all" || item == "minimal" ||
             item == "compiler-internal-details";
    };
  };

  Opts opts{};

  for (const auto *A : ParsedArgs.filtered(OPT_synthesize_interface_show,
                                           OPT_minimum_access_level)) {
    auto &Opt = A->getOption();
    StringRef Value = A->getValue();
    if (Opt.matches(OPT_minimum_access_level)) {
      auto Parsed = llvm::StringSwitch<std::optional<AccessLevel>>(Value)
                        .Case("private", AccessLevel::Private)
                        .Case("fileprivate", AccessLevel::FilePrivate)
                        .Case("internal", AccessLevel::Internal)
                        .Case("package", AccessLevel::Package)
                        .Case("public", AccessLevel::Public)
                        .Case("open", AccessLevel::Open)
                        .Default(std::nullopt);
      if (!Parsed) {
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                       "-minimum-access-level", Value);
        return EXIT_FAILURE;
      }
      MinAccessLevel = *Parsed;
      continue;
    }

    if (Opt.matches(OPT_synthesize_interface_show)) {
      opts = {}; // Reset state for last-win semantics across multiple args

      if (Opts::isShorthand(Value)) {
        // Shorthands cannot be combined with other item in the same list
        opts.apply(Value);
      } else {
        SmallVector<StringRef, 2> Items;
        Value.split(Items, ',', /*MaxSplit=*/-1, /*KeepEmpty=*/false);
        for (auto Item : Items) {
          if (Opts::isShorthand(Item) || !opts.apply(Item)) {
            Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                           "-synthesized-interface-show", Item);
            return EXIT_FAILURE;
          }
        }
      }
      continue;
    }
    llvm_unreachable("unhandled arg");
  }

  printOpts.SkipUnavailable = !opts.ShowUnavailable;
  printOpts.PrintImplicitAttrs = opts.ShowImplicitAttrs;
  printOpts.QualifyImportedTypes = opts.ShowQualifiedTypes;
  if (opts.ShowCompilerInternals) {
    printOpts.SkipUnsafeCXXMethods = false;
    printOpts.SkipImplicit = false;
  }

  if (MinAccessLevel) {
    printOpts.AccessFilter = *MinAccessLevel;
    if (printOpts.AccessFilter < AccessLevel::Public)
      printOpts.PrintAccess = true;
  }

  swift::OptionSet<swift::ide::ModuleTraversal> traversalOpts = std::nullopt;
  if (ParsedArgs.hasArg(OPT_include_submodules)) {
    traversalOpts = swift::ide::ModuleTraversal::VisitSubmodules;
  }

  StreamPrinter printer(fs);
  ide::printModuleInterface(M, /*GroupNames=*/{}, traversalOpts, printer,
                            printOpts, /*PrintSynthesizedExtensions=*/false);

  if (CI.getASTContext().hadError())
    return EXIT_FAILURE;

  return EXIT_SUCCESS;
}
