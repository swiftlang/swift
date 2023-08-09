//===--- SwiftMaterializationUnit.cpp - JIT Swift ASTs ----------*- C++ -*-===//
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
// Defines the `SwiftMaterializationUnit` class, which allows you to JIT
// individual Swift AST declarations.
//
//===----------------------------------------------------------------------===//

#include <memory>
#include <optional>
#include <string>

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITLink/JITLink.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/EPCIndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/ObjectTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorAddress.h"
#include "llvm/ExecutionEngine/Orc/Shared/MemoryFlags.h"
#include "llvm/Support/Error.h"

#include "swift/AST/IRGenRequests.h"
#include "swift/AST/SILGenRequests.h"
#include "swift/AST/TBDGenRequests.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Immediate/SwiftMaterializationUnit.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Subsystems.h"

using namespace swift;

/// The suffix appended to function bodies when creating lazy reexports
static const std::string ManglingSuffix = "$impl";

/// Mangle a function for a lazy reexport
static std::string mangleFunctionBody(const StringRef Unmangled) {
  return Unmangled.str() + ManglingSuffix;
}

/// Whether a function name is mangled to be a lazy reexport
static bool isMangled(const StringRef Symbol) {
  return Symbol.endswith(ManglingSuffix);
}

/// Demangle a lazy reexport
static StringRef demangleFunctionBody(const StringRef Mangled) {
  return Mangled.drop_back(ManglingSuffix.size());
}

llvm::Expected<std::unique_ptr<SwiftJIT>>
SwiftJIT::Create(CompilerInstance &CI) {

  auto J = CreateLLJIT(CI);
  if (!J)
    return J.takeError();

  // Create generator to resolve symbols defined in current process
  auto &ES = (*J)->getExecutionSession();
  auto EPCIU =
      llvm::orc::EPCIndirectionUtils::Create(ES.getExecutorProcessControl());
  if (!EPCIU)
    return EPCIU.takeError();

  (*EPCIU)->createLazyCallThroughManager(
      ES, llvm::pointerToJITTargetAddress(&handleLazyCompilationFailure));

  if (auto Err = setUpInProcessLCTMReentryViaEPCIU(**EPCIU))
    return std::move(Err);
  return std::unique_ptr<SwiftJIT>(
      new SwiftJIT(std::move(*J), std::move(*EPCIU)));
}

SwiftJIT::~SwiftJIT() {
  if (auto Err = EPCIU->cleanup())
    J->getExecutionSession().reportError(std::move(Err));
}

llvm::orc::LLJIT &SwiftJIT::getJIT() { return *J; }

llvm::orc::JITDylib &SwiftJIT::getMainJITDylib() {
  return J->getMainJITDylib();
}

llvm::Error SwiftJIT::initialize(llvm::orc::JITDylib &JD) {
  return J->initialize(JD);
}

llvm::Error SwiftJIT::deinitialize(llvm::orc::JITDylib &JD) {
  return J->deinitialize(JD);
}

llvm::Expected<llvm::orc::ExecutorAddr> SwiftJIT::lookup(llvm::StringRef Name) {
  return J->lookup(Name);
}

llvm::Expected<llvm::orc::ExecutorAddr>
SwiftJIT::lookupLinkerMangled(llvm::StringRef Name) {
  return J->lookupLinkerMangled(Name);
}

llvm::orc::SymbolStringPtr SwiftJIT::mangleAndIntern(StringRef Name) {
  return J->mangleAndIntern(Name);
}

llvm::orc::SymbolStringPtr SwiftJIT::intern(StringRef Name) {
  return J->getExecutionSession().intern(Name);
}

llvm::orc::IRCompileLayer &SwiftJIT::getIRCompileLayer() {
  return J->getIRCompileLayer();
}

llvm::orc::ObjectTransformLayer &SwiftJIT::getObjTransformLayer() {
  return J->getObjTransformLayer();
}

llvm::Expected<std::unique_ptr<llvm::orc::LLJIT>>
SwiftJIT::CreateLLJIT(CompilerInstance &CI) {
  llvm::TargetOptions TargetOpt;
  std::string CPU;
  std::string Triple;
  std::vector<std::string> Features;
  const auto &Invocation = CI.getInvocation();
  const auto &IRGenOpts = Invocation.getIRGenOptions();
  auto &Ctx = CI.getASTContext();
  std::tie(TargetOpt, CPU, Features, Triple) =
      getIRTargetOptions(IRGenOpts, Ctx);
  auto JTMB = llvm::orc::JITTargetMachineBuilder(llvm::Triple(Triple))
                  .setRelocationModel(llvm::Reloc::PIC_)
                  .setOptions(std::move(TargetOpt))
                  .setCPU(std::move(CPU))
                  .addFeatures(Features)
                  .setCodeGenOptLevel(llvm::CodeGenOpt::Default);
  auto J = llvm::orc::LLJITBuilder()
               .setJITTargetMachineBuilder(std::move(JTMB))
               .create();
  if (!J)
    return J.takeError();
  auto G = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
      (*J)->getDataLayout().getGlobalPrefix());
  if (!G)
    return G.takeError();
  (*J)->getMainJITDylib().addGenerator(std::move(*G));
  return J;
}

static llvm::Error
renameFunctionBodies(llvm::orc::MaterializationResponsibility &MR,
                     llvm::jitlink::LinkGraph &G) {

  using namespace llvm;
  using namespace llvm::orc;

  llvm::DenseSet<StringRef> ToRename;
  for (auto &KV : MR.getSymbols()) {
    const auto &Name = *KV.first;
    if (isMangled(Name))
      // All mangled functions we are responsible for
      // materializing must be mangled at the object levels
      ToRename.insert(demangleFunctionBody(Name));
  }
  for (auto &Sec : G.sections()) {
    // Skip non-executable sections.
    if ((Sec.getMemProt() & MemProt::Exec) == MemProt::None)
      continue;

    for (auto *Sym : Sec.symbols()) {

      // Skip all anonymous and non-callables.
      if (!Sym->hasName() || !Sym->isCallable())
        continue;

      if (ToRename.count(Sym->getName())) {
        // FIXME: Get rid of the temporary when Swift's llvm-project is
        // updated to LLVM 17.
        auto NewName = G.allocateString(mangleFunctionBody(Sym->getName()));
        Sym->setName({NewName.data(), NewName.size()});
      }
    }
  }

  return llvm::Error::success();
}

void SwiftJIT::Plugin::modifyPassConfig(
    llvm::orc::MaterializationResponsibility &MR, llvm::jitlink::LinkGraph &G,
    llvm::jitlink::PassConfiguration &PassConfig) {
  PassConfig.PrePrunePasses.push_back(
      [&](llvm::jitlink::LinkGraph &G) { return renameFunctionBodies(MR, G); });
}

llvm::Error
SwiftJIT::Plugin::notifyFailed(llvm::orc::MaterializationResponsibility &MR) {
  return llvm::Error::success();
}

llvm::Error
SwiftJIT::Plugin::notifyRemovingResources(llvm::orc::ResourceKey K) {
  return llvm::Error::success();
}

void SwiftJIT::Plugin::notifyTransferringResources(
    llvm::orc::ResourceKey DstKey, llvm::orc::ResourceKey SrcKey) {}

void SwiftJIT::handleLazyCompilationFailure() {
  llvm::errs() << "Lazy compilation error\n";
  exit(1);
}

SwiftJIT::SwiftJIT(std::unique_ptr<llvm::orc::LLJIT> J,
                   std::unique_ptr<llvm::orc::EPCIndirectionUtils> EPCIU)
    : J(std::move(J)), EPCIU(std::move(EPCIU)),
      LCTM(this->EPCIU->getLazyCallThroughManager()),
      ISM(this->EPCIU->createIndirectStubsManager()) {

  static_cast<llvm::orc::ObjectLinkingLayer &>(this->J->getObjLinkingLayer())
      .addPlugin(std::make_unique<Plugin>());
}

/// IRGen the provided `SILModule` with the specified options.
/// Returns `std::nullopt` if a compiler error is encountered
static std::optional<GeneratedModule>
generateModule(const CompilerInstance &CI, std::unique_ptr<SILModule> SM) {
  // TODO: Use OptimizedIRRequest for this.
  const auto &Context = CI.getASTContext();
  auto *swiftModule = CI.getMainModule();
  const auto PSPs = CI.getPrimarySpecificPathsForAtMostOnePrimary();
  const auto &Invocation = CI.getInvocation();
  const auto &TBDOpts = Invocation.getTBDGenOptions();
  const auto &IRGenOpts = Invocation.getIRGenOptions();

  // Lower the SIL module to LLVM IR
  auto GenModule = performIRGeneration(
      swiftModule, IRGenOpts, TBDOpts, std::move(SM),
      swiftModule->getName().str(), PSPs, ArrayRef<std::string>());

  if (Context.hadError()) {
    return std::nullopt;
  }

  assert(GenModule && "Emitted no diagnostics but IR generation failed?");
  auto *Module = GenModule.getModule();

  // Run LLVM passes on the resulting module
  performLLVM(IRGenOpts, Context.Diags, /*diagMutex*/ nullptr,
              /*hash*/ nullptr, Module, GenModule.getTargetMachine(),
              CI.getPrimarySpecificPathsForAtMostOnePrimary().OutputFilename,
              CI.getOutputBackend(), Context.Stats);

  if (Context.hadError()) {
    return std::nullopt;
  }

  return GenModule;
}

/// Log a compilation error to standard error
static void logError(llvm::Error Err) {
  logAllUnhandledErrors(std::move(Err), llvm::errs(), "");
}

std::unique_ptr<SwiftMaterializationUnit>
SwiftMaterializationUnit::Create(SwiftJIT &JIT, CompilerInstance &CI) {
  auto *M = CI.getMainModule();
  TBDGenOptions Opts;
  Opts.PublicSymbolsOnly = false;
  auto TBDDesc = TBDGenDescriptor::forModule(M, std::move(Opts));
  SymbolSourceMapRequest SourceReq{TBDDesc};
  auto Sources =
      llvm::cantFail(M->getASTContext().evaluator(std::move(SourceReq)));
  llvm::orc::SymbolFlagsMap PublicInterface;
  for (const auto &Entry : *Sources.storage) {
    const auto &Source = Entry.getValue();
    if (Source.kind != SymbolSource::Kind::SIL) {
      continue;
    }
    auto Ref = Source.getSILDeclRef();
    if (Ref.getDefinitionLinkage() != SILLinkage::Public)
      continue;
    const auto &SymbolName = Entry.getKey();
    const auto Flags =
        llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable;
    PublicInterface[JIT.intern(SymbolName)] = Flags;
  }
  return std::unique_ptr<SwiftMaterializationUnit>(new SwiftMaterializationUnit(
      JIT, CI, std::move(Sources), std::move(PublicInterface)));
}

StringRef SwiftMaterializationUnit::getName() const {
  return "SwiftMaterializationUnit";
}

SwiftMaterializationUnit::SwiftMaterializationUnit(
    SwiftJIT &JIT, CompilerInstance &CI, SymbolSourceMap Sources,
    llvm::orc::SymbolFlagsMap Symbols)
    : MaterializationUnit({std::move(Symbols), nullptr}),
      Sources(std::move(Sources)), JIT(JIT), CI(CI) {}

void SwiftMaterializationUnit::materialize(
    std::unique_ptr<llvm::orc::MaterializationResponsibility> R) {
  SILRefsToEmit Refs;
  const auto &RS = R->getRequestedSymbols();
  for (auto &Sym : RS) {
    const auto &Source = Sources.storage->find(*Sym)->getValue();
    auto Ref = Source.getSILDeclRef();
    if (auto *AFD = Ref.getAbstractFunctionDecl()) {
      AFD->getTypecheckedBody();
      if (CI.getASTContext().hadError()) {
        R->failMaterialization();
        return;
      }
    }
    Refs.push_back(std::move(Ref));
  }
  auto SM = performASTLowering(CI, std::move(Refs));
  runSILDiagnosticPasses(*SM);
  runSILLoweringPasses(*SM);
  auto GM = generateModule(CI, std::move(SM));
  if (!GM) {
    R->failMaterialization();
    return;
  }
  auto *Module = GM->getModule();
  // Now we must register all other public symbols defined by
  // the module with the JIT
  llvm::orc::SymbolFlagsMap LazilyDiscoveredSymbols;
  // Register all global values, including global
  // variables and functions
  for (const auto &GV : Module->global_values()) {
    // Ignore all symbols that will not appear in symbol table
    if (GV.hasLocalLinkage() || GV.hasAppendingLinkage() ||
        GV.isDeclaration()) {
      continue;
    }
    auto Name = GV.getName();
    auto MangledName = JIT.mangleAndIntern(Name);
    if (RS.contains(MangledName)) {
      continue;
    }
    LazilyDiscoveredSymbols[MangledName] =
        llvm::JITSymbolFlags::fromGlobalValue(GV);
  }
  llvm::orc::SymbolFlagsMap UnrequestedSymbols;
  for (auto &[Sym, Flags] : R->getSymbols()) {
    if (!RS.contains(Sym) && !LazilyDiscoveredSymbols.count(Sym)) {
      UnrequestedSymbols[Sym] = Flags;
    }
  }
  std::unique_ptr<MaterializationUnit> UnrequestedMU(
      new SwiftMaterializationUnit(JIT, CI, std::move(Sources),
                                   std::move(UnrequestedSymbols)));
  if (auto Err = R->replace(std::move(UnrequestedMU))) {
    logError(std::move(Err));
    R->failMaterialization();
    return;
  }
  if (auto Err = R->defineMaterializing(std::move(LazilyDiscoveredSymbols))) {
    logError(std::move(Err));
    R->failMaterialization();
    return;
  }
  auto TSM = std::move(*GM).intoThreadSafeContext();
  JIT.getIRCompileLayer().emit(std::move(R), std::move(TSM));
}

llvm::Error
SwiftJIT::addSwift(llvm::orc::JITDylib &JD,
                   std::unique_ptr<llvm::orc::MaterializationUnit> MU) {
  // Create stub map.
  llvm::orc::SymbolAliasMap Stubs;
  for (auto &[Name, Flags] : MU->getSymbols()) {
    if (isMangled(*Name)) {
      // Create a stub for mangled functions
      auto OriginalName = demangleFunctionBody(*Name);
      Stubs.insert(
          {J->getExecutionSession().intern(OriginalName), {Name, Flags}});
    }
  }
  assert(ISM.get() && "No ISM?");

  if (!Stubs.empty())
    if (auto Err = JD.define(lazyReexports(LCTM, *ISM, JD, std::move(Stubs))))
      return Err;

  return JD.define(std::move(MU));
}

void SwiftMaterializationUnit::discard(const llvm::orc::JITDylib &JD,
                                       const llvm::orc::SymbolStringPtr &Sym) {}
