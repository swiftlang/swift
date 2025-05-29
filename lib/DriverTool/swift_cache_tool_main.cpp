//===--- swift_cache_tool_main.cpp - Swift caching tool for inspection ----===//
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
// Utility tool for inspecting and accessing swift cache.
//
//===----------------------------------------------------------------------===//
//
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/Version.h"
#include "swift/Frontend/CachingUtils.h"
#include "swift/Frontend/CompileJobCacheKey.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Parse/ParseVersion.h"
#include "clang/CAS/CASOptions.h"
#include "clang/CAS/IncludeTree.h"
#include "llvm/CAS/ActionCache.h"
#include "llvm/CAS/BuiltinUnifiedCASDatabases.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/MemoryBuffer.h"
#include <memory>

using namespace swift;
using namespace llvm::opt;
using namespace llvm::cas;

namespace {

enum class SwiftCacheToolAction {
  Invalid,
  PrintBaseKey,
  PrintOutputKeys,
  ValidateOutputs,
  RenderDiags,
  PrintIncludeTreeList,
  PrintCompileCacheKey,
};

struct OutputEntry {
  std::string InputPath;
  std::string CacheKey;
  std::vector<std::pair<std::string, std::string>> Outputs;
};

enum ID {
  OPT_INVALID = 0, // This is not an option ID.
#define OPTION(...) LLVM_MAKE_OPT_ID(__VA_ARGS__),
#include "SwiftCacheToolOptions.inc"
  LastOption
#undef OPTION
};

#define PREFIX(NAME, VALUE)                                                    \
  constexpr llvm::StringLiteral NAME##_init[] = VALUE;                         \
  constexpr llvm::ArrayRef<llvm::StringLiteral> NAME(                          \
      NAME##_init, std::size(NAME##_init) - 1);
#include "SwiftCacheToolOptions.inc"
#undef PREFIX

static const OptTable::Info InfoTable[] = {
#define OPTION(...) LLVM_CONSTRUCT_OPT_INFO(__VA_ARGS__),
#include "SwiftCacheToolOptions.inc"
#undef OPTION
};

class CacheToolOptTable : public llvm::opt::GenericOptTable {
public:
  CacheToolOptTable() : GenericOptTable(InfoTable) {}
};

class SwiftCacheToolInvocation {
private:
  CompilerInstance Instance;
  CompilerInvocation Invocation;
  PrintingDiagnosticConsumer PDC;
  std::string MainExecutablePath;
  clang::CASOptions CASOpts;
  std::vector<std::string> Inputs;
  std::vector<std::string> FrontendArgs;
  SwiftCacheToolAction ActionKind = SwiftCacheToolAction::Invalid;

public:
  SwiftCacheToolInvocation(const std::string &ExecPath)
      : MainExecutablePath(ExecPath) {
    Instance.addDiagnosticConsumer(&PDC);
  }

  int parseArgs(ArrayRef<const char *> Args) {
    auto &Diags = Instance.getDiags();

    CacheToolOptTable Table;
    unsigned MissingIndex;
    unsigned MissingCount;
    llvm::opt::InputArgList ParsedArgs =
        Table.ParseArgs(Args, MissingIndex, MissingCount);
    if (MissingCount) {
      Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                     ParsedArgs.getArgString(MissingIndex), MissingCount);
      return 1;
    }

    if (ParsedArgs.getLastArg(OPT_help)) {
      std::string ExecutableName =
          llvm::sys::path::stem(MainExecutablePath).str();
      Table.printHelp(llvm::outs(), ExecutableName.c_str(), "Swift Cache Tool",
                      0, 0, /*ShowAllAliases*/ false);
      return 0;
    }

    if (const Arg* PluginPath = ParsedArgs.getLastArg(OPT_cas_plugin_path))
      CASOpts.PluginPath = PluginPath->getValue();
    if (const Arg* OnDiskPath = ParsedArgs.getLastArg(OPT_cas_path))
      CASOpts.CASPath = OnDiskPath->getValue();
    for (StringRef Opt : ParsedArgs.getAllArgValues(OPT_cas_plugin_option)) {
      StringRef Name, Value;
      std::tie(Name, Value) = Opt.split('=');
      CASOpts.PluginOptions.emplace_back(std::string(Name), std::string(Value));
    }

    // Fallback to default path if not set.
    if (CASOpts.CASPath.empty() && CASOpts.PluginPath.empty())
      CASOpts.CASPath = getDefaultOnDiskCASPath();

    Inputs = ParsedArgs.getAllArgValues(OPT_INPUT);
    FrontendArgs = ParsedArgs.getAllArgValues(OPT__DASH_DASH);
    if (auto *A = ParsedArgs.getLastArg(OPT_cache_tool_action))
      ActionKind =
          llvm::StringSwitch<SwiftCacheToolAction>(A->getValue())
              .Case("print-base-key", SwiftCacheToolAction::PrintBaseKey)
              .Case("print-output-keys", SwiftCacheToolAction::PrintOutputKeys)
              .Case("validate-outputs", SwiftCacheToolAction::ValidateOutputs)
              .Case("render-diags", SwiftCacheToolAction::RenderDiags)
              .Case("print-include-tree-list",
                    SwiftCacheToolAction::PrintIncludeTreeList)
              .Case("print-compile-cache-key",
                    SwiftCacheToolAction::PrintCompileCacheKey)
              .Default(SwiftCacheToolAction::Invalid);

    if (ActionKind == SwiftCacheToolAction::Invalid) {
      llvm::errs()
          << "Invalid option specified for -cache-tool-action: "
          << "print-base-key|print-output-keys|validate-outputs|render-diags|"
          << "print-include-tree-list|print-compile-cache-key\n";
      return 1;
    }

    return 0;
  }

  int run() {
    switch (ActionKind) {
    case SwiftCacheToolAction::PrintBaseKey:
      return printBaseKey();
    case SwiftCacheToolAction::PrintOutputKeys:
      return printOutputKeys();
    case SwiftCacheToolAction::ValidateOutputs:
      return validateOutputs();
    case SwiftCacheToolAction::RenderDiags:
      return renderDiags();
    case SwiftCacheToolAction::PrintIncludeTreeList:
      return printIncludeTreeList();
    case SwiftCacheToolAction::PrintCompileCacheKey:
      return printCompileCacheKey();
    case SwiftCacheToolAction::Invalid:
      return 0; // No action. Probably just print help. Return.
    }
  }

private:
  bool setupCompiler() {
    // Setup invocation.
    SmallString<128> workingDirectory;
    llvm::sys::fs::current_path(workingDirectory);

    // Parse arguments.
    if (FrontendArgs.empty()) {
      llvm::errs() << "missing swift-frontend command-line after --\n";
      return true;
    }
    // drop swift-frontend executable path and leading `-frontend` from
    // command-line.
    if (StringRef(FrontendArgs[0]).ends_with("swift-frontend"))
      FrontendArgs.erase(FrontendArgs.begin());
    if (StringRef(FrontendArgs[0]) == "-frontend")
      FrontendArgs.erase(FrontendArgs.begin());

    SmallVector<std::unique_ptr<llvm::MemoryBuffer>, 4>
        configurationFileBuffers;
    std::vector<const char*> Args;
    for (auto &A: FrontendArgs)
      Args.emplace_back(A.c_str());

    // Make sure CASPath is the same between invocation and cache-tool by
    // appending the cas-path since the option doesn't affect cache key.
    if (!CASOpts.CASPath.empty()) {
      Args.emplace_back("-cas-path");
      Args.emplace_back(CASOpts.CASPath.c_str());
    }
    if (!CASOpts.PluginPath.empty()) {
      Args.emplace_back("-cas-plugin-path");
      Args.emplace_back(CASOpts.PluginPath.c_str());
    }
    std::vector<std::string> PluginJoinedOpts;
    for (const auto& Opt: CASOpts.PluginOptions) {
      PluginJoinedOpts.emplace_back(Opt.first + "=" + Opt.second);
      Args.emplace_back("-cas-plugin-option");
      Args.emplace_back(PluginJoinedOpts.back().c_str());
    }

    if (Invocation.parseArgs(Args, Instance.getDiags(),
                             &configurationFileBuffers, workingDirectory,
                             MainExecutablePath))
      return true;

    if (!Invocation.getCASOptions().EnableCaching) {
      llvm::errs() << "Requested command-line arguments do not enable CAS\n";
      return true;
    }

    // Setup instance.
    std::string InstanceSetupError;
    if (Instance.setup(Invocation, InstanceSetupError, Args)) {
      llvm::errs() << "swift-frontend invocation setup error: "
                   << InstanceSetupError << "\n";
      return true;
    }

    // Disable diagnostic caching from this fake instance.
    if (auto *CDP = Instance.getCachingDiagnosticsProcessor())
      CDP->endDiagnosticCapture();

    return false;
  }

  std::optional<ObjectRef> getBaseKey() {
    auto BaseKey = Instance.getCompilerBaseKey();
    if (!BaseKey) {
      Instance.getDiags().diagnose(SourceLoc(), diag::error_cas,
                                   "query base cache key",
                                   "base cache key doesn't exist");
      return std::nullopt;
    }

    return *BaseKey;
  }

  int printBaseKey() {
    if (setupCompiler())
      return 1;

    auto &CAS = Instance.getObjectStore();
    auto BaseKey = getBaseKey();
    if (!BaseKey)
      return 1;

    if (ActionKind == SwiftCacheToolAction::PrintBaseKey)
      llvm::outs() << CAS.getID(*BaseKey).toString() << "\n";

    return 0;
  }

  int printOutputKeys();
  int validateOutputs();
  int renderDiags();
  int printIncludeTreeList();
  int printCompileCacheKey();
};

} // end anonymous namespace

int SwiftCacheToolInvocation::printOutputKeys() {
  if (setupCompiler())
    return 1;

  auto &CAS = Instance.getObjectStore();
  auto BaseKey = getBaseKey();
  if (!BaseKey)
    return 1;

  std::vector<OutputEntry> OutputKeys;
  bool hasError = false;
  auto addFromInputFile = [&](const InputFile &Input, unsigned InputIndex) {
    auto InputPath = Input.getFileName();
    auto OutputKey =
        createCompileJobCacheKeyForOutput(CAS, *BaseKey, InputIndex);
    if (!OutputKey) {
      llvm::errs() << "cannot create cache key for " << InputPath << ": "
                   << toString(OutputKey.takeError()) << "\n";
      hasError = true;
    }
    OutputKeys.emplace_back(
        OutputEntry{InputPath, CAS.getID(*OutputKey).toString(), {}});
    auto &Outputs = OutputKeys.back().Outputs;
    if (!Input.outputFilename().empty())
      Outputs.emplace_back(file_types::getTypeName(
                               Invocation.getFrontendOptions()
                                   .InputsAndOutputs.getPrincipalOutputType()),
                           Input.outputFilename());
    Input.getPrimarySpecificPaths()
        .SupplementaryOutputs.forEachSetOutputAndType(
            [&](const std::string &File, file_types::ID ID) {
              // Dont print serialized diagnostics.
              if (file_types::isProducedFromDiagnostics(ID))
                return;
              Outputs.emplace_back(file_types::getTypeName(ID), File);
            });
  };
  auto AllInputs =
      Invocation.getFrontendOptions().InputsAndOutputs.getAllInputs();
  for (unsigned Index = 0; Index < AllInputs.size(); ++Index)
    addFromInputFile(AllInputs[Index], Index);

  // Add diagnostics file.
  if (!OutputKeys.empty())
    OutputKeys.front().Outputs.emplace_back(
        file_types::getTypeName(file_types::ID::TY_CachedDiagnostics),
        "<cached-diagnostics>");

  if (hasError)
    return 1;

  llvm::json::OStream Out(llvm::outs(), /*IndentSize=*/4);
  Out.array([&] {
    for (const auto &E : OutputKeys) {
      Out.object([&] {
        Out.attribute("Input", E.InputPath);
        Out.attribute("CacheKey", E.CacheKey);
        Out.attributeArray("Outputs", [&] {
          for (const auto &OutEntry : E.Outputs) {
            Out.object([&] {
              Out.attribute("Kind", OutEntry.first);
              Out.attribute("Path", OutEntry.second);
            });
          }
        });
      });
    }
  });

  return 0;
}

static llvm::Expected<llvm::json::Array>
readOutputEntriesFromFile(StringRef Path) {
  auto JSONContent = llvm::MemoryBuffer::getFile(Path);
  if (!JSONContent)
    return llvm::createStringError(JSONContent.getError(),
                                   "failed to read input file");

  auto JSONValue = llvm::json::parse((*JSONContent)->getBuffer());
  if (!JSONValue)
    return JSONValue.takeError();

  auto Keys = JSONValue->getAsArray();
  if (!Keys)
    return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                   "invalid JSON format for input file");

  return *Keys;
}

int SwiftCacheToolInvocation::validateOutputs() {
  auto DB = CASOpts.getOrCreateDatabases();
  if (!DB)
    report_fatal_error(DB.takeError());

  auto &CAS = *DB->first;
  auto &Cache = *DB->second;

  PrintingDiagnosticConsumer PDC;
  Instance.getDiags().addConsumer(PDC);

  auto lookupFailed = [&](StringRef Key) {
    llvm::errs() << "failed to find output for cache key " << Key << "\n";
    return true;
  };
  auto lookupError = [&](llvm::Error Err, StringRef Key) {
    llvm::errs() << "failed to find output for cache key " << Key << ": "
                 << toString(std::move(Err)) << "\n";
    return true;
  };

  auto validateCacheKeysFromFile = [&](const std::string &Path) {
    auto Keys = readOutputEntriesFromFile(Path);
    if (!Keys) {
      llvm::errs() << "cannot read file " << Path << ": "
                   << toString(Keys.takeError()) << "\n";
      return true;
    }

    for (const auto& Entry : *Keys) {
      if (auto *Obj = Entry.getAsObject()) {
        if (auto Key = Obj->getString("CacheKey")) {
          auto ID = CAS.parseID(*Key);
          if (!ID) {
            llvm::errs() << "failed to parse ID " << Key << ": "
                         << toString(ID.takeError()) << "\n";
            return true;
          }
          auto Ref = CAS.getReference(*ID);
          if (!Ref)
            return lookupFailed(*Key);
          auto KeyID = CAS.getID(*Ref);
          auto Lookup = Cache.get(KeyID);
          if (!Lookup)
            return lookupError(Lookup.takeError(), *Key);

          if (!*Lookup)
            return lookupFailed(*Key);

          auto OutputRef = CAS.getReference(**Lookup);
          if (!OutputRef)
            return lookupFailed(*Key);

          cas::CachedResultLoader Loader(CAS, *OutputRef);
          if (auto Err = Loader.replay(
                  [&](file_types::ID Kind, ObjectRef Ref) -> llvm::Error {
                    auto Proxy = CAS.getProxy(Ref);
                    if (!Proxy)
                      return Proxy.takeError();
                    return llvm::Error::success();
                  })) {
            llvm::errs() << "failed to find output for cache key " << *Key
                         << ": " << toString(std::move(Err)) << "\n";
            return true;
          }
          continue;
        }
      }
      llvm::errs() << "can't read cache key from " << Path << "\n";
      return true;
    }

    return false;
  };

  return llvm::any_of(Inputs, validateCacheKeysFromFile);
}

int SwiftCacheToolInvocation::renderDiags() {
  if (setupCompiler())
    return 1;

  auto *CDP = Instance.getCachingDiagnosticsProcessor();
  if (!CDP) {
    llvm::errs() << "provided commandline doesn't support cached diagnostics\n";
    return 1;
  }

  auto renderDiagsFromFile = [&](const std::string &Path) {
    auto Keys = readOutputEntriesFromFile(Path);
    if (!Keys) {
      llvm::errs() << "cannot read file " << Path << ": "
                   << toString(Keys.takeError()) << "\n";
      return true;
    }

    for (const auto& Entry : *Keys) {
      if (auto *Obj = Entry.getAsObject()) {
        if (auto Kind = Obj->getString("OutputKind")) {
          if (*Kind != "cached-diagnostics")
            continue;
        }
        if (auto Key = Obj->getString("CacheKey")) {
          if (auto Buffer = loadCachedCompileResultFromCacheKey(
                  Instance.getObjectStore(), Instance.getActionCache(),
                  Instance.getDiags(), *Key,
                  file_types::ID::TY_CachedDiagnostics)) {
            if (auto E = CDP->replayCachedDiagnostics(Buffer->getBuffer())) {
              llvm::errs() << "failed to replay cache: "
                           << toString(std::move(E)) << "\n";
              return true;
            }
            return false;
          }
        }
      }
    }
    llvm::errs() << "cannot locate cached diagnostics in file\n";
    return true;
  };

  return llvm::any_of(Inputs, renderDiagsFromFile);
}

int SwiftCacheToolInvocation::printIncludeTreeList() {
  auto error = [](llvm::Error err) {
    llvm::errs() << llvm::toString(std::move(err)) << "\n";
    return 1;
  };
  auto DB = CASOpts.getOrCreateDatabases();
  if (!DB) {
    return error(DB.takeError());
  }
  auto CAS = DB->first;
  for (auto &input: Inputs) {
    auto ID = CAS->parseID(input);
    if (!ID)
      return error(ID.takeError());

    auto Ref = CAS->getReference(*ID);
    if (!Ref) {
      llvm::errs() << "CAS object not found: " << input << "\n";
      return 1;
    }

    auto fileList = clang::cas::IncludeTree::FileList::get(*CAS, *Ref);
    if (!fileList)
      return error(fileList.takeError());

    if (auto err = fileList->print(llvm::outs()))
      return error(std::move(err));
  }

  return 0;
}

int SwiftCacheToolInvocation::printCompileCacheKey() {
  auto error = [](llvm::Error err) {
    llvm::errs() << "cannot print cache key: " << llvm::toString(std::move(err))
                 << "\n";
    return 1;
  };
  if (Inputs.size() != 1) {
    llvm::errs() << "expect 1 CASID as input\n";
    return 1;
  }
  auto DB = CASOpts.getOrCreateDatabases();
  if (!DB) {
    return error(DB.takeError());
  }
  auto CAS = DB->first;
  auto &input = Inputs.front();
  auto ID = CAS->parseID(input);
  if (!ID)
    return error(ID.takeError());

  auto Ref = CAS->getReference(*ID);
  if (!Ref) {
    llvm::errs() << "CAS object not found: " << input << "\n";
    return 1;
  }

  if (auto err = swift::printCompileJobCacheKey(*CAS, *Ref, llvm::outs()))
    return error(std::move(err));

  return 0;
}

int swift_cache_tool_main(ArrayRef<const char *> Args, const char *Argv0,
                          void *MainAddr) {
  INITIALIZE_LLVM();

  SwiftCacheToolInvocation Invocation(
      llvm::sys::fs::getMainExecutable(Argv0, MainAddr));

  if (Invocation.parseArgs(Args) != 0)
    return EXIT_FAILURE;

  if (Invocation.run() != 0)
    return EXIT_FAILURE;

  return EXIT_SUCCESS;
}
