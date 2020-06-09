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
#include "swift/SymbolGraphGen/SymbolGraphGen.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace llvm::opt;

namespace options {
static llvm::cl::OptionCategory Category("swift-symbolgraph-extract Options");

static llvm::cl::opt<std::string>
ModuleName("module-name", llvm::cl::desc("Name of the module to extract (Required)"), llvm::cl::cat(Category));

static llvm::cl::list<std::string>
FrameworkSearchPaths("F", llvm::cl::desc("Add a directory to the framework search paths"), llvm::cl::ZeroOrMore,
                     llvm::cl::cat(Category));

static llvm::cl::list<std::string>
SystemFrameworkSearchPaths("Fsystem", llvm::cl::desc("Add directory to system framework search path"), llvm::cl::ZeroOrMore,
                     llvm::cl::cat(Category));

static llvm::cl::list<std::string>
LibrarySearchPaths("L", llvm::cl::desc("Add a directory to the library search paths"), llvm::cl::ZeroOrMore,
                   llvm::cl::cat(Category));

static llvm::cl::list<std::string>
ImportSearchPaths("I", llvm::cl::desc("Add directory to the import search paths"), llvm::cl::ZeroOrMore,
                  llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
ModuleCachePath("module-cache-path", llvm::cl::desc("Specifies a path to cache modules"), llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
SDK("sdk", llvm::cl::desc("Path to the SDK"),
    llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
Target("target", llvm::cl::desc("Target triple (Required)"),
       llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
SwiftVersion("swift-version", llvm::cl::desc("Interpret input according to a specific Swift language version number"), llvm::cl::cat(Category));

static llvm::cl::opt<bool>
PrettyPrint("pretty-print", llvm::cl::desc("Pretty-print the resulting Symbol Graph JSON"), llvm::cl::cat(Category));

static llvm::cl::opt<bool>
SkipSynthesizedMembers("skip-synthesized-members",
                       llvm::cl::desc("Skip members inherited through classes or default implementations"),
                       llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
MinimumAccessLevel("minimum-access-level", llvm::cl::desc("Include symbols with this access level or more"), llvm::cl::cat(Category));

static llvm::cl::list<std::string>
Xcc("Xcc", llvm::cl::desc("Pass the following command-line flag to Clang"),
         llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
ResourceDir("resource-dir",
            llvm::cl::desc("Override the directory that holds the compiler resource files"),
            llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
OutputDir("output-dir", llvm::cl::desc("Symbol Graph JSON Output Directory (Required)"), llvm::cl::cat(Category));
} // end namespace options

static bool argumentsAreValid() {
  bool Valid = true;
  if (options::Target.empty()) {
    llvm::errs() << "Required -target option is missing\n";
    Valid = false;
  }

  if (options::ModuleName.empty()) {
    llvm::errs() << "Required -module-name argument is missing\n";
    Valid = false;
  }

  if (options::OutputDir.empty()) {
    llvm::errs() << "Required -output-dir argument is missing\n";
    Valid = false;
  }

  return Valid;
}

int swift_symbolgraph_extract_main(ArrayRef<const char *> Args, const char *Argv0, void *MainAddr) {
  INITIALIZE_LLVM();

  llvm::cl::HideUnrelatedOptions(options::Category);

  // LLVM Command Line expects to trim off argv[0].
  SmallVector<const char *, 8> ArgsWithArgv0 { Argv0 };
  ArgsWithArgv0.append(Args.begin(), Args.end());

  if (Args.empty()) {
    ArgsWithArgv0.push_back("-help");
  }

  llvm::cl::ParseCommandLineOptions(ArgsWithArgv0.size(),
      llvm::makeArrayRef(ArgsWithArgv0).data(),
      "Swift Symbol Graph Extractor\n");

  if (!argumentsAreValid()) {
    llvm::cl::PrintHelpMessage();
    return EXIT_FAILURE;
  }

  if (!llvm::sys::fs::is_directory(options::OutputDir)) {
    llvm::errs() << "-output-dir argument '" << options::OutputDir
      << " does not exist or is not a directory\n";
    return EXIT_FAILURE;
  }

  CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(Argv0, MainAddr));
  Invocation.setModuleName("swift_symbolgraph_extract");
  if (!options::ResourceDir.empty()) {
    Invocation.setRuntimeResourcePath(options::ResourceDir);
  }
  Invocation.setSDKPath(options::SDK);
  Invocation.setTargetTriple(options::Target);

  for (auto &Arg : options::Xcc) {
    Invocation.getClangImporterOptions().ExtraArgs.push_back(Arg);
  }

  std::vector<SearchPathOptions::FrameworkSearchPath> FrameworkSearchPaths;
  for (const auto &Path : options::FrameworkSearchPaths) {
    FrameworkSearchPaths.push_back({ Path, /*isSystem*/ false});
  }
  for (const auto &Path : options::SystemFrameworkSearchPaths) {
    FrameworkSearchPaths.push_back({ Path, /*isSystem*/ true });
  }
  Invocation.setFrameworkSearchPaths(FrameworkSearchPaths);
  Invocation.getSearchPathOptions().LibrarySearchPaths = options::LibrarySearchPaths;
  Invocation.setImportSearchPaths(options::ImportSearchPaths);

  Invocation.getLangOptions().EnableObjCInterop = llvm::Triple(options::Target).isOSDarwin();
  Invocation.getLangOptions().DebuggerSupport = true;

  Invocation.getFrontendOptions().EnableLibraryEvolution = true;

  Invocation.setClangModuleCachePath(options::ModuleCachePath);
  Invocation.getClangImporterOptions().ModuleCachePath = options::ModuleCachePath;
  Invocation.setDefaultPrebuiltCacheIfNecessary();

  if (!options::SwiftVersion.empty()) {
    using version::Version;
    bool isValid = false;
    if (auto Version = Version::parseVersionString(options::SwiftVersion,
                                                   SourceLoc(), nullptr)) {
      if (auto Effective = Version.getValue().getEffectiveLanguageVersion()) {
        Invocation.getLangOptions().EffectiveLanguageVersion = *Effective;
        isValid = true;
      }
    }
    if (!isValid) {
      llvm::errs() << "Unsupported Swift Version.\n";
      return EXIT_FAILURE;
    }
  }

  symbolgraphgen::SymbolGraphOptions Options {
    options::OutputDir,
    llvm::Triple(options::Target),
    options::PrettyPrint,
    AccessLevel::Public,
    !options::SkipSynthesizedMembers,
  };

  if (!options::MinimumAccessLevel.empty()) {
    Options.MinimumAccessLevel =
        llvm::StringSwitch<AccessLevel>(options::MinimumAccessLevel)
      .Case("open", AccessLevel::Open)     
      .Case("public", AccessLevel::Public)     
      .Case("internal", AccessLevel::Internal)     
      .Case("fileprivate", AccessLevel::FilePrivate)     
      .Case("private", AccessLevel::Private)     
      .Default(AccessLevel::Public);
  }

  PrintingDiagnosticConsumer DiagPrinter;

  CompilerInstance CI;
  CI.getDiags().addConsumer(DiagPrinter);

  if (CI.setup(Invocation)) {
    llvm::outs() << "Failed to setup compiler instance\n";
    return EXIT_FAILURE;
  }

  auto M = CI.getASTContext().getModuleByName(options::ModuleName);
  if (!M) {
    llvm::errs()
      << "Couldn't load module '" << options::ModuleName << '\''
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

  const auto &MainFile = M->getMainFile(FileUnitKind::SerializedAST);
  llvm::errs() << "Emitting symbol graph for module file: " << MainFile.getModuleDefiningPath() << '\n';

  return symbolgraphgen::emitSymbolGraphForModule(M,
    Options);
}
