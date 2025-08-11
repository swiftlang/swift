//===--- Link.cpp - Link in transparent SILFunctions from module ----------===//
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

#include "swift/AST/ProtocolConformance.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/Serialization/SerializedModuleLoader.h"

using namespace swift;

static llvm::cl::opt<bool> LinkEmbeddedRuntime("link-embedded-runtime",
                                               llvm::cl::init(true));

static llvm::cl::opt<bool> LinkUsedFunctions("link-used-functions",
                                               llvm::cl::init(true));

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

/// Copies code from the standard library into the user program to enable
/// optimizations.
class SILLinker : public SILModuleTransform {
  SILModule::LinkingMode LinkMode;

public:
  explicit SILLinker(SILModule::LinkingMode LinkMode) : LinkMode(LinkMode) {}

  void run() override {
    SILModule &M = *getModule();
    for (auto &Fn : M)
      if (M.linkFunction(&Fn, LinkMode))
        invalidateAnalysis(&Fn, SILAnalysis::InvalidationKind::Everything);

    // In embedded Swift, the stdlib contains all the runtime functions needed
    // (swift_retain, etc.). Link them in so they can be referenced in IRGen.
    if (M.getOptions().EmbeddedSwift && LinkEmbeddedRuntime) {
      linkEmbeddedRuntimeFromStdlib();
      linkEmbeddedConcurrency();
    }

    // In embedded Swift, we need to explicitly link any @_used globals and
    // functions from imported modules.
    if (M.getOptions().EmbeddedSwift && LinkUsedFunctions) {
      linkUsedGlobalsAndFunctions();
    }
  }

  void linkEmbeddedRuntimeFromStdlib() {
    using namespace RuntimeConstants;
#define FUNCTION(ID, MODULE, NAME, CC, AVAILABILITY, RETURNS, ARGS, ATTRS,     \
                 EFFECT, MEMORY_EFFECTS)                                       \
  linkEmbeddedRuntimeFunctionByName(#NAME, EFFECT);                            \
  if (getModule()->getASTContext().hadError())                                 \
    return;

#define RETURNS(...)
#define ARGS(...)
#define NO_ARGS
#define ATTRS(...)
#define NO_ATTRS
#define EFFECT(...) { __VA_ARGS__ }
#define MEMORY_EFFECTS(...)
#define UNKNOWN_MEMEFFECTS

#include "swift/Runtime/RuntimeFunctions.def"

      // swift_retainCount is not part of private contract between the compiler and runtime, but we still need to link it
      linkEmbeddedRuntimeFunctionByName("swift_retainCount", { RefCounting });
  }

  void linkEmbeddedConcurrency() {
    using namespace RuntimeConstants;

    // Note: we ignore errors here, because, depending on the exact situation
    //
    //    (a) We might not have Concurrency anyway, and
    //
    //    (b) The Impl function might be implemented in C++.
    //
    // Also, the hook Impl functions are marked as internal, unlike the
    // runtime functions, which are public.

#define SWIFT_CONCURRENCY_HOOK(RETURNS, NAME, ...)                  \
  linkUsedFunctionByName(#NAME "Impl", SILLinkage::HiddenExternal)
#define SWIFT_CONCURRENCY_HOOK0(RETURNS, NAME)                      \
  linkUsedFunctionByName(#NAME "Impl", SILLinkage::HiddenExternal)

    #include "swift/Runtime/ConcurrencyHooks.def"

    linkUsedFunctionByName("swift_task_asyncMainDrainQueueImpl",
                           SILLinkage::HiddenExternal);
    linkUsedFunctionByName("_swift_task_enqueueOnExecutor",
                           SILLinkage::HiddenExternal);
    linkUsedFunctionByName("swift_createDefaultExecutors",
                           SILLinkage::HiddenExternal);
    linkUsedFunctionByName("swift_getDefaultExecutor",
                           SILLinkage::HiddenExternal);
    linkEmbeddedRuntimeWitnessTables();
  }

  void linkEmbeddedRuntimeFunctionByName(StringRef name,
                                         ArrayRef<RuntimeEffect> effects) {
    SILModule &M = *getModule();

    bool allocating = false;
    for (RuntimeEffect rt : effects)
      if (rt == RuntimeEffect::Allocating || rt == RuntimeEffect::Deallocating)
        allocating = true;

    // Don't link allocating runtime functions in -no-allocations mode.
    if (M.getOptions().NoAllocations && allocating) return;

    // Swift Runtime functions are all expected to be SILLinkage::PublicExternal
    linkUsedFunctionByName(name, SILLinkage::PublicExternal);
  }

  void linkEmbeddedRuntimeWitnessTables() {
    SILModule &M = *getModule();

    auto *mainActor = M.getASTContext().getMainActorDecl();
    if (mainActor) {
      for (auto *PC : mainActor->getAllConformances()) {
        auto *ProtoDecl = PC->getProtocol();
        if (ProtoDecl->getName().str() == "Actor") {
          M.linkWitnessTable(PC, SILModule::LinkingMode::LinkAll);
          if (auto *WT = M.lookUpWitnessTable(PC)) {
            WT->setLinkage(SILLinkage::Public);
          }
        }
      }
    }
  }

  SILFunction *linkUsedFunctionByName(StringRef name,
                                      std::optional<SILLinkage> Linkage) {
    SILModule &M = *getModule();

    // Bail if function is already loaded.
    if (auto *Fn = M.lookUpFunction(name)) return Fn;

    SILFunction *Fn = M.getSILLoader()->lookupSILFunction(name, Linkage);
    if (!Fn) return nullptr;

    if (M.linkFunction(Fn, LinkMode))
      invalidateAnalysis(Fn, SILAnalysis::InvalidationKind::Everything);

    // Make sure that dead-function-elimination doesn't remove the explicitly
    // linked functions.
    //
    // TODO: lazily emit runtime functions in IRGen so that we don't have to
    //       rely on dead-stripping in the linker to remove unused runtime
    //       functions.
    if (Fn->isDefinition())
      Fn->setLinkage(SILLinkage::Public);

    return Fn;
  }

  SILGlobalVariable *linkUsedGlobalVariableByName(StringRef name) {
    SILModule &M = *getModule();

    // Bail if runtime function is already loaded.
    if (auto *GV = M.lookUpGlobalVariable(name)) return GV;

    SILGlobalVariable *GV = M.getSILLoader()->lookupSILGlobalVariable(name);
    if (!GV) return nullptr;

    // Make sure that dead-function-elimination doesn't remove the explicitly
    // linked global variable.
    if (GV->isDefinition())
      GV->setLinkage(SILLinkage::Public);

    return GV;
  }

  void linkUsedGlobalsAndFunctions() {
    SmallVector<VarDecl *, 32> Globals;
    SmallVector<AbstractFunctionDecl *, 32> Functions;
    collectUsedDeclsFromLoadedModules(Globals, Functions);

    for (auto *G : Globals) {
      auto declRef = SILDeclRef(G, SILDeclRef::Kind::Func);
      linkUsedGlobalVariableByName(declRef.mangle());
    }

    for (auto *F : Functions) {
      auto declRef = SILDeclRef(F, SILDeclRef::Kind::Func);
      auto *Fn = linkUsedFunctionByName(declRef.mangle(), /*Linkage*/{});

      // If we have @_cdecl or @_silgen_name, also link the foreign thunk
      if (Fn->hasCReferences()) {
        auto declRef = SILDeclRef(F, SILDeclRef::Kind::Func, /*isForeign*/true);
        linkUsedFunctionByName(declRef.mangle(), /*Linkage*/{});
      }
    }
  }

  void collectUsedDeclsFromLoadedModules(
      SmallVectorImpl<VarDecl *> &Globals,
      SmallVectorImpl<AbstractFunctionDecl *> &Functions) {
    SILModule &M = *getModule();

    for (const auto &Entry : M.getASTContext().getLoadedModules()) {
      for (auto File : Entry.second->getFiles()) {
        if (auto LoadedAST = dyn_cast<SerializedASTFile>(File)) {
          auto matcher = [](const DeclAttributes &attrs) -> bool {
            return attrs.hasAttribute<UsedAttr>();
          };

          SmallVector<Decl *, 32> Decls;
          LoadedAST->getTopLevelDeclsWhereAttributesMatch(Decls, matcher);

          for (Decl *D : Decls) {
            if (AbstractFunctionDecl *F = dyn_cast<AbstractFunctionDecl>(D)) {
              Functions.push_back(F);
            } else if (VarDecl *G = dyn_cast<VarDecl>(D)) {
              Globals.push_back(G);
            } else {
              assert(false && "only funcs and globals can be @_used");
            }
          }
        }
      }
    }
  }

};
} // end anonymous namespace


SILTransform *swift::createMandatorySILLinker() {
  return new SILLinker(SILModule::LinkingMode::LinkNormal);
}

SILTransform *swift::createPerformanceSILLinker() {
  return new SILLinker(SILModule::LinkingMode::LinkAll);
}
