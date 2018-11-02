//===--- Immediate.cpp - the swift immediate mode -------------------------===//
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
// This is the implementation of the swift interpreter, which takes a
// source file and JITs it.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-immediate"
#include "swift/Immediate/Immediate.h"
#include "ImmediateImpl.h"

#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Module.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMContext.h"
#include "swift/Frontend/Frontend.h"
#include "swift/IRGen/IRGenPublic.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Config/config.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Support/Path.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dlfcn.h>
#endif

using namespace swift;
using namespace swift::immediate;

static void *loadRuntimeLib(StringRef runtimeLibPathWithName) {
#if defined(_WIN32)
  return LoadLibraryA(runtimeLibPathWithName.str().c_str());
#else
  return dlopen(runtimeLibPathWithName.str().c_str(), RTLD_LAZY | RTLD_GLOBAL);
#endif
}

static void *loadRuntimeLib(StringRef sharedLibName, StringRef runtimeLibPath) {
  // FIXME: Need error-checking.
  llvm::SmallString<128> Path = runtimeLibPath;
  llvm::sys::path::append(Path, sharedLibName);
  return loadRuntimeLib(Path);
}

void *swift::immediate::loadSwiftRuntime(StringRef runtimeLibPath) {
  return loadRuntimeLib("libswiftCore" LTDL_SHLIB_EXT, runtimeLibPath);
}

static bool tryLoadLibrary(LinkLibrary linkLib,
                           SearchPathOptions searchPathOpts) {
  llvm::SmallString<128> path = linkLib.getName();

  // If we have an absolute or relative path, just try to load it now.
  if (llvm::sys::path::has_parent_path(path.str())) {
    return loadRuntimeLib(path);
  }

  bool success = false;
  switch (linkLib.getKind()) {
  case LibraryKind::Library: {
    llvm::SmallString<32> stem;
    if (llvm::sys::path::has_extension(path.str())) {
      stem = std::move(path);
    } else {
      // FIXME: Try the appropriate extension for the current platform?
      stem = "lib";
      stem += path;
      stem += LTDL_SHLIB_EXT;
    }

    // Try user-provided library search paths first.
    for (auto &libDir : searchPathOpts.LibrarySearchPaths) {
      path = libDir;
      llvm::sys::path::append(path, stem.str());
      success = loadRuntimeLib(path);
      if (success)
        break;
    }

    // Let loadRuntimeLib determine the best search paths.
    if (!success)
      success = loadRuntimeLib(stem);

    // If that fails, try our runtime library path.
    if (!success)
      success = loadRuntimeLib(stem, searchPathOpts.RuntimeLibraryPath);
    break;
  }
  case LibraryKind::Framework: {
    // If we have a framework, mangle the name to point to the framework
    // binary.
    llvm::SmallString<64> frameworkPart{std::move(path)};
    frameworkPart += ".framework";
    llvm::sys::path::append(frameworkPart, linkLib.getName());

    // Try user-provided framework search paths first; frameworks contain
    // binaries as well as modules.
    for (auto &frameworkDir : searchPathOpts.FrameworkSearchPaths) {
      path = frameworkDir.Path;
      llvm::sys::path::append(path, frameworkPart.str());
      success = loadRuntimeLib(path);
      if (success)
        break;
    }

    // If that fails, let loadRuntimeLib search for system frameworks.
    if (!success)
      success = loadRuntimeLib(frameworkPart);
    break;
  }
  }

  return success;
}

bool swift::immediate::tryLoadLibraries(ArrayRef<LinkLibrary> LinkLibraries,
                                        SearchPathOptions SearchPathOpts,
                                        DiagnosticEngine &Diags) {
  SmallVector<bool, 4> LoadedLibraries;
  LoadedLibraries.append(LinkLibraries.size(), false);

  // Libraries are not sorted in the topological order of dependencies, and we
  // don't know the dependencies in advance.  Try to load all libraries until
  // we stop making progress.
  bool HadProgress;
  do {
    HadProgress = false;
    for (unsigned i = 0; i != LinkLibraries.size(); ++i) {
      if (!LoadedLibraries[i] &&
          tryLoadLibrary(LinkLibraries[i], SearchPathOpts)) {
        LoadedLibraries[i] = true;
        HadProgress = true;
      }
    }
  } while (HadProgress);

  return std::all_of(LoadedLibraries.begin(), LoadedLibraries.end(),
                     [](bool Value) { return Value; });
}

static void linkerDiagnosticHandlerNoCtx(const llvm::DiagnosticInfo &DI) {
  if (DI.getSeverity() != llvm::DS_Error)
    return;

  std::string MsgStorage;
  {
    llvm::raw_string_ostream Stream(MsgStorage);
    llvm::DiagnosticPrinterRawOStream DP(Stream);
    DI.print(DP);
  }
  llvm::errs() << "Error linking swift modules\n";
  llvm::errs() << MsgStorage << "\n";
}



static void linkerDiagnosticHandler(const llvm::DiagnosticInfo &DI,
                                    void *Context) {
  // This assert self documents our precondition that Context is always
  // nullptr. It seems that parts of LLVM are using the flexibility of having a
  // context. We don't really care about this.
  assert(Context == nullptr && "We assume Context is always a nullptr");

  return linkerDiagnosticHandlerNoCtx(DI);
}

bool swift::immediate::linkLLVMModules(llvm::Module *Module,
                                       std::unique_ptr<llvm::Module> SubModule
                            // TODO: reactivate the linker mode if it is
                            // supported in llvm again. Otherwise remove the
                            // commented code completely.
                            /*, llvm::Linker::LinkerMode LinkerMode */)
{
  llvm::LLVMContext &Ctx = SubModule->getContext();
  auto OldHandler = Ctx.getDiagnosticHandlerCallBack();
  void *OldDiagnosticContext = Ctx.getDiagnosticContext();
  Ctx.setDiagnosticHandlerCallBack(linkerDiagnosticHandler, nullptr);
  bool Failed = llvm::Linker::linkModules(*Module, std::move(SubModule));
  Ctx.setDiagnosticHandlerCallBack(OldHandler, OldDiagnosticContext);
  return !Failed;
}

bool swift::immediate::IRGenImportedModules(
    CompilerInstance &CI, llvm::Module &Module,
    llvm::SmallPtrSetImpl<swift::ModuleDecl *> &ImportedModules,
    SmallVectorImpl<llvm::Function *> &InitFns, IRGenOptions &IRGenOpts,
    const SILOptions &SILOpts) {
  swift::ModuleDecl *M = CI.getMainModule();

  // Perform autolinking.
  SmallVector<LinkLibrary, 4> AllLinkLibraries(IRGenOpts.LinkLibraries);
  auto addLinkLibrary = [&](LinkLibrary linkLib) {
    AllLinkLibraries.push_back(linkLib);
  };

  M->collectLinkLibraries(addLinkLibrary);

  tryLoadLibraries(AllLinkLibraries, CI.getASTContext().SearchPathOpts,
                   CI.getDiags());

  ImportedModules.insert(M);
  if (!CI.hasSourceImport())
    return false;

  // IRGen the modules this module depends on. This is only really necessary
  // for imported source, but that's a very convenient thing to do in -i mode.
  // FIXME: Crawling all loaded modules is a hack.
  // FIXME: And re-doing SILGen, SIL-linking, SIL diagnostics, and IRGen is
  // expensive, because it's not properly being limited to new things right now.
  bool hadError = false;
  for (auto &entry : CI.getASTContext().LoadedModules) {
    swift::ModuleDecl *import = entry.second;
    if (!ImportedModules.insert(import).second)
      continue;

    std::unique_ptr<SILModule> SILMod = performSILGeneration(import,
                                                             CI.getSILOptions());
    if (runSILDiagnosticPasses(*SILMod)) {
      hadError = true;
      break;
    }
    runSILLoweringPasses(*SILMod);

    const auto PSPs = CI.getPrimarySpecificPathsForAtMostOnePrimary();
    // FIXME: We shouldn't need to use the global context here, but
    // something is persisting across calls to performIRGeneration.
    auto SubModule = performIRGeneration(
        IRGenOpts, import, std::move(SILMod), import->getName().str(), PSPs,
        getGlobalLLVMContext(), ArrayRef<std::string>());

    if (CI.getASTContext().hadError()) {
      hadError = true;
      break;
    }

    if (!linkLLVMModules(&Module, std::move(SubModule)
                         // TODO: reactivate the linker mode if it is
                         // supported in llvm again. Otherwise remove the
                         // commented code completely.
                         /*, llvm::Linker::DestroySource */)) {
      hadError = true;
      break;
    }

    // FIXME: This is an ugly hack; need to figure out how this should
    // actually work.
    SmallVector<char, 20> NameBuf;
    StringRef InitFnName = (import->getName().str() + ".init").toStringRef(NameBuf);
    llvm::Function *InitFn = Module.getFunction(InitFnName);
    if (InitFn)
      InitFns.push_back(InitFn);
  }

  return hadError;
}

int swift::RunImmediately(CompilerInstance &CI, const ProcessCmdLine &CmdLine,
                          IRGenOptions &IRGenOpts, const SILOptions &SILOpts) {
  ASTContext &Context = CI.getASTContext();
  
  // IRGen the main module.
  auto *swiftModule = CI.getMainModule();
  const auto PSPs = CI.getPrimarySpecificPathsForAtMostOnePrimary();
  // FIXME: We shouldn't need to use the global context here, but
  // something is persisting across calls to performIRGeneration.
  auto ModuleOwner = performIRGeneration(
      IRGenOpts, swiftModule, CI.takeSILModule(), swiftModule->getName().str(),
      PSPs, getGlobalLLVMContext(), ArrayRef<std::string>());
  auto *Module = ModuleOwner.get();

  if (Context.hadError())
    return -1;

  // Load libSwiftCore to setup process arguments.
  //
  // This must be done here, before any library loading has been done, to avoid
  // racing with the static initializers in user code.
  auto stdlib = loadSwiftRuntime(Context.SearchPathOpts.RuntimeLibraryPath);
  if (!stdlib) {
    CI.getDiags().diagnose(SourceLoc(),
                           diag::error_immediate_mode_missing_stdlib);
    return -1;
  }

  // Setup interpreted process arguments.
  using ArgOverride = void (*)(const char **, int);
#if defined(_WIN32)
  auto module = static_cast<HMODULE>(stdlib);
  auto emplaceProcessArgs = reinterpret_cast<ArgOverride>(
    GetProcAddress(module, "_swift_stdlib_overrideUnsafeArgvArgc"));
  if (emplaceProcessArgs == nullptr)
    return -1;
#else
  auto emplaceProcessArgs
          = (ArgOverride)dlsym(stdlib, "_swift_stdlib_overrideUnsafeArgvArgc");
  if (dlerror())
    return -1;
#endif

  SmallVector<const char *, 32> argBuf;
  for (size_t i = 0; i < CmdLine.size(); ++i) {
    argBuf.push_back(CmdLine[i].c_str());
  }
  argBuf.push_back(nullptr);

  (*emplaceProcessArgs)(argBuf.data(), CmdLine.size());

  SmallVector<llvm::Function*, 8> InitFns;
  llvm::SmallPtrSet<swift::ModuleDecl *, 8> ImportedModules;
  if (IRGenImportedModules(CI, *Module, ImportedModules, InitFns,
                           IRGenOpts, SILOpts))
    return -1;

  llvm::PassManagerBuilder PMBuilder;
  PMBuilder.OptLevel = 2;
  PMBuilder.Inliner = llvm::createFunctionInliningPass(200);

  // Build the ExecutionEngine.
  llvm::EngineBuilder builder(std::move(ModuleOwner));
  std::string ErrorMsg;
  llvm::TargetOptions TargetOpt;
  std::string CPU;
  std::string Triple;
  std::vector<std::string> Features;
  std::tie(TargetOpt, CPU, Features, Triple)
    = getIRTargetOptions(IRGenOpts, swiftModule->getASTContext());
  builder.setRelocationModel(llvm::Reloc::PIC_);
  builder.setTargetOptions(TargetOpt);
  builder.setMCPU(CPU);
  builder.setMAttrs(Features);
  builder.setErrorStr(&ErrorMsg);
  builder.setEngineKind(llvm::EngineKind::JIT);
  llvm::ExecutionEngine *EE = builder.create();
  if (!EE) {
    llvm::errs() << "Error loading JIT: " << ErrorMsg;
    return -1;
  }

  LLVM_DEBUG(llvm::dbgs() << "Module to be executed:\n";
             Module->dump());

  EE->finalizeObject();
  
  // Run the generated program.
  for (auto InitFn : InitFns) {
    LLVM_DEBUG(llvm::dbgs() << "Running initialization function "
                            << InitFn->getName() << '\n');
    EE->runFunctionAsMain(InitFn, CmdLine, nullptr);
  }

  LLVM_DEBUG(llvm::dbgs() << "Running static constructors\n");
  EE->runStaticConstructorsDestructors(false);
  LLVM_DEBUG(llvm::dbgs() << "Running main\n");
  llvm::Function *EntryFn = Module->getFunction("main");
  return EE->runFunctionAsMain(EntryFn, CmdLine, nullptr);
}
