//===--- SILModule.cpp - SILModule implementation -------------------------===//
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

#define DEBUG_TYPE "sil-module"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Substitution.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "Linker.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include <functional>
using namespace swift;
using namespace Lowering;

class SILModule::SerializationCallback : public SerializedSILLoader::Callback {
  void didDeserialize(ModuleDecl *M, SILFunction *fn) override {
    updateLinkage(fn);
  }

  void didDeserialize(ModuleDecl *M, SILGlobalVariable *var) override {
    updateLinkage(var);
    
    // For globals we currently do not support available_externally.
    // In the interpreter it would result in two instances for a single global:
    // one in the imported module and one in the main module.
    var->setDeclaration(true);
  }

  void didDeserialize(ModuleDecl *M, SILVTable *vtable) override {
    // TODO: should vtables get linkage?
    //updateLinkage(vtable);
  }

  void didDeserialize(ModuleDecl *M, SILWitnessTable *wt) override {
    updateLinkage(wt);
  }

  template <class T> void updateLinkage(T *decl) {
    switch (decl->getLinkage()) {
    case SILLinkage::Public:
      decl->setLinkage(SILLinkage::PublicExternal);
      return;
    case SILLinkage::Hidden:
      decl->setLinkage(SILLinkage::HiddenExternal);
      return;
    case SILLinkage::Shared:
      decl->setLinkage(SILLinkage::SharedExternal);
      return;
    case SILLinkage::Private:
        decl->setLinkage(SILLinkage::PrivateExternal);
        return;
    case SILLinkage::PublicExternal:
    case SILLinkage::HiddenExternal:
    case SILLinkage::SharedExternal:
    case SILLinkage::PrivateExternal:
      return;
    }
  }
};

SILModule::SILModule(ModuleDecl *SwiftModule, SILOptions &Options,
                     const DeclContext *associatedDC, bool wholeModule,
                     bool wholeModuleSerialized)
    : TheSwiftModule(SwiftModule), AssociatedDeclContext(associatedDC),
      Stage(SILStage::Raw), Callback(new SILModule::SerializationCallback()),
      wholeModule(wholeModule), WholeModuleSerialized(wholeModuleSerialized),
      Options(Options), Types(*this) {}

SILModule::~SILModule() {
  // Decrement ref count for each SILGlobalVariable with static initializers.
  for (SILGlobalVariable &v : silGlobals)
    if (v.getInitializer())
      v.getInitializer()->decrementRefCount();

  // Drop everything functions in this module reference.
  //
  // This is necessary since the functions may reference each other.  We don't
  // need to worry about sil_witness_tables since witness tables reference each
  // other via protocol conformances and sil_vtables don't reference each other
  // at all.
  for (SILFunction &F : *this)
    F.dropAllReferences();
}

void *SILModule::allocate(unsigned Size, unsigned Align) const {
  if (getASTContext().LangOpts.UseMalloc)
    return AlignedAlloc(Size, Align);

  return BPA.Allocate(Size, Align);
}

void *SILModule::allocateInst(unsigned Size, unsigned Align) const {
  return AlignedAlloc(Size, Align);
}

void SILModule::deallocateInst(SILInstruction *I) {
  AlignedFree(I);
}

SILWitnessTable *
SILModule::createWitnessTableDeclaration(ProtocolConformance *C,
                                         SILLinkage linkage) {
  // If we are passed in a null conformance (a valid value), just return nullptr
  // since we cannot map a witness table to it.
  if (!C)
    return nullptr;

  // Extract the base NormalProtocolConformance.
  NormalProtocolConformance *NormalC = C->getRootNormalConformance();

  return SILWitnessTable::create(*this, linkage, NormalC);
}

SILWitnessTable *
SILModule::lookUpWitnessTable(ProtocolConformanceRef C,
                              bool deserializeLazily) {
  // If we have an abstract conformance passed in (a legal value), just return
  // nullptr.
  if (!C.isConcrete())
    return nullptr;

  return lookUpWitnessTable(C.getConcrete());
}

SILWitnessTable *
SILModule::lookUpWitnessTable(const ProtocolConformance *C,
                              bool deserializeLazily) {
  assert(C && "null conformance passed to lookUpWitnessTable");

  const NormalProtocolConformance *NormalC = C->getRootNormalConformance();
  // Attempt to lookup the witness table from the table.
  auto found = WitnessTableMap.find(NormalC);
  if (found == WitnessTableMap.end()) {
#ifndef NDEBUG
    // Make sure that all witness tables are in the witness table lookup
    // cache.
    //
    // This code should not be hit normally since we add witness tables to the
    // lookup cache when we create them. We don't just assert here since there
    // is the potential for a conformance without a witness table to be passed
    // to this function.
    for (SILWitnessTable &WT : witnessTables)
      assert(WT.getConformance() != NormalC &&
             "Found witness table that is not"
             " in the witness table lookup cache.");
#endif
    return nullptr;
  }

  SILWitnessTable *wtable = found->second;
  assert(wtable != nullptr && "Should never map a conformance to a null witness"
                          " table.");

  // If we have a definition, return it.
  if (wtable->isDefinition())
    return wtable;

  // Otherwise try to deserialize it. If we succeed return the deserialized
  // function.
  //
  // *NOTE* In practice, wtable will be deserializedTable, but I do not want to rely
  // on that behavior for now.
  if (deserializeLazily)
    if (auto deserialized = getSILLoader()->lookupWitnessTable(wtable))
      return deserialized;

  // If we fail, just return the declaration.
  return wtable;
}

SILDefaultWitnessTable *
SILModule::lookUpDefaultWitnessTable(const ProtocolDecl *Protocol,
                                     bool deserializeLazily) {
  // Note: we only ever look up default witness tables in the translation unit
  // that is currently being compiled, since they SILGen generates them when it
  // visits the protocol declaration, and IRGen emits them when emitting the
  // protocol descriptor metadata for the protocol.

  auto found = DefaultWitnessTableMap.find(Protocol);
  if (found == DefaultWitnessTableMap.end()) {
    if (deserializeLazily) {
      SILLinkage linkage =
        getSILLinkage(getDeclLinkage(Protocol), ForDefinition);
      SILDefaultWitnessTable *wtable =
        SILDefaultWitnessTable::create(*this, linkage, Protocol);
      wtable = getSILLoader()->lookupDefaultWitnessTable(wtable);
      if (wtable)
        DefaultWitnessTableMap[Protocol] = wtable;
      return wtable;
    }

    return nullptr;
  }

  return found->second;
}

SILDefaultWitnessTable *
SILModule::createDefaultWitnessTableDeclaration(const ProtocolDecl *Protocol,
                                                SILLinkage Linkage) {
  return SILDefaultWitnessTable::create(*this, Linkage, Protocol);
}

void SILModule::deleteWitnessTable(SILWitnessTable *Wt) {
  NormalProtocolConformance *Conf = Wt->getConformance();
  assert(lookUpWitnessTable(Conf, false) == Wt);
  WitnessTableMap.erase(Conf);
  witnessTables.erase(Wt);
}

SILFunction *SILModule::getOrCreateFunction(SILLocation loc,
                                            StringRef name,
                                            SILLinkage linkage,
                                            CanSILFunctionType type,
                                            IsBare_t isBareSILFunction,
                                            IsTransparent_t isTransparent,
                                            IsSerialized_t isSerialized,
                                            IsThunk_t isThunk,
                                            SILFunction::ClassVisibility_t CV) {
  if (auto fn = lookUpFunction(name)) {
    assert(fn->getLoweredFunctionType() == type);
    assert(fn->getLinkage() == linkage ||
           stripExternalFromLinkage(fn->getLinkage()) == linkage);
    return fn;
  }

  auto fn = SILFunction::create(*this, linkage, name, type, nullptr,
                                loc, isBareSILFunction, isTransparent,
                                isSerialized, isThunk, CV);
  fn->setDebugScope(new (*this) SILDebugScope(loc, fn));
  return fn;
}

static SILFunction::ClassVisibility_t getClassVisibility(SILDeclRef constant) {
  if (!constant.hasDecl())
    return SILFunction::NotRelevant;

  // If this declaration is a function which goes into a vtable, then it's
  // symbol must be as visible as its class. Derived classes even have to put
  // all less visible methods of the base class into their vtables.

  auto *FD = dyn_cast<AbstractFunctionDecl>(constant.getDecl());
  if (!FD)
    return SILFunction::NotRelevant;

  DeclContext *context = FD->getDeclContext();

  // Methods from extensions don't go into vtables (yet).
  if (context->isExtensionContext())
    return SILFunction::NotRelevant;

  auto *classType = context->getAsClassOrClassExtensionContext();
  if (!classType || classType->isFinal())
    return SILFunction::NotRelevant;

  if (FD->isFinal() && !FD->getOverriddenDecl())
    return SILFunction::NotRelevant;

  assert(FD->getEffectiveAccess() <= classType->getEffectiveAccess() &&
         "class must be as visible as its members");

  switch (classType->getEffectiveAccess()) {
    case Accessibility::Private:
    case Accessibility::FilePrivate:
      return SILFunction::NotRelevant;
    case Accessibility::Internal:
      return SILFunction::InternalClass;
    case Accessibility::Public:
    case Accessibility::Open:
      return SILFunction::PublicClass;
  }

  llvm_unreachable("Unhandled Accessibility in switch.");
}

static bool verifySILSelfParameterType(SILDeclRef DeclRef,
                                       SILFunction *F, CanSILFunctionType FTy) {
  SILModule &M = F->getModule();
  SILParameterInfo PInfo = FTy->getSelfParameter();
  CanType CTy = PInfo.getType();
  SILType Ty = SILType::getPrimitiveObjectType(CTy);

  // We do not care about trivial parameters (for now). There seem to be
  // cases where we lower them as unowned.
  //
  // *NOTE* We do not run this check when we have a generic type since
  // *generic types do not have type lowering and are always treated as
  // *non-trivial since we do not know the type.
  if (CTy->hasArchetype() || CTy->hasTypeParameter() ||
      M.getTypeLowering(Ty).isTrivial())
    return true;

  // If this function is a constructor or destructor, bail. These have @owned
  // parameters.
  if (DeclRef.isConstructor() || DeclRef.isDestructor())
    return true;

  // Otherwise, if this function type has a guaranteed self parameter type,
  // make sure that we have a +0 self param.
  return !FTy->getExtInfo().hasGuaranteedSelfParam() ||
          PInfo.isGuaranteed() || PInfo.isIndirectMutating();
}

SILFunction *SILModule::getOrCreateFunction(SILLocation loc,
                                            SILDeclRef constant,
                                            ForDefinition_t forDefinition) {

  auto name = constant.mangle();
  auto constantType = Types.getConstantType(constant).castTo<SILFunctionType>();
  SILLinkage linkage = constant.getLinkage(forDefinition);

  if (auto fn = lookUpFunction(name)) {
    assert(fn->getLoweredFunctionType() == constantType);
    assert(fn->getLinkage() == linkage ||
           (forDefinition == ForDefinition_t::NotForDefinition &&
            fn->getLinkage() ==
                constant.getLinkage(ForDefinition_t::ForDefinition)));
    if (forDefinition) {
      // In all the cases where getConstantLinkage returns something
      // different for ForDefinition, it returns an available-externally
      // linkage.
      if (isAvailableExternally(fn->getLinkage())) {
        fn->setLinkage(constant.getLinkage(ForDefinition));
      }
    }
    return fn;
  }

  IsTransparent_t IsTrans = constant.isTransparent()
                            ? IsTransparent
                            : IsNotTransparent;
  IsSerialized_t IsSer = constant.isSerialized();

  EffectsKind EK = constant.hasEffectsAttribute()
                   ? constant.getEffectsAttribute()
                   : EffectsKind::Unspecified;

  Inline_t inlineStrategy = InlineDefault;
  if (constant.isNoinline())
    inlineStrategy = NoInline;
  else if (constant.isAlwaysInline())
    inlineStrategy = AlwaysInline;

  auto *F = SILFunction::create(*this, linkage, name,
                                constantType, nullptr,
                                None, IsNotBare, IsTrans, IsSer, IsNotThunk,
                                getClassVisibility(constant),
                                inlineStrategy, EK);
  F->setDebugScope(new (*this) SILDebugScope(loc, F));

  F->setGlobalInit(constant.isGlobal());
  if (constant.hasDecl()) {
    auto decl = constant.getDecl();

    if (constant.isForeign && decl->hasClangNode())
      F->setClangNodeOwner(decl);

    auto Attrs = decl->getAttrs();
    for (auto *A : Attrs.getAttributes<SemanticsAttr>())
      F->addSemanticsAttr(cast<SemanticsAttr>(A)->Value);

    for (auto *A : Attrs.getAttributes<SpecializeAttr>()) {
      auto *SA = cast<SpecializeAttr>(A);
      auto kind = SA->getSpecializationKind() ==
                          SpecializeAttr::SpecializationKind::Full
                      ? SILSpecializeAttr::SpecializationKind::Full
                      : SILSpecializeAttr::SpecializationKind::Partial;
      F->addSpecializeAttr(SILSpecializeAttr::create(
          *this, SA->getRequirements(), SA->isExported(), kind));
    }
  }

  // If this function has a self parameter, make sure that it has a +0 calling
  // convention. This cannot be done for general function types, since
  // function_ref's SILFunctionTypes do not have archetypes associated with
  // it.
  CanSILFunctionType FTy = F->getLoweredFunctionType();
  if (FTy->hasSelfParam()) {
    (void)&verifySILSelfParameterType;
    assert(verifySILSelfParameterType(constant, F, FTy) &&
           "Invalid signature for SIL Self parameter type");
  }
                                      

  return F;
}


SILFunction *SILModule::getOrCreateSharedFunction(SILLocation loc,
                                                  StringRef name,
                                                  CanSILFunctionType type,
                                                  IsBare_t isBareSILFunction,
                                                  IsTransparent_t isTransparent,
                                                  IsSerialized_t isSerialized,
                                                  IsThunk_t isThunk) {
  return getOrCreateFunction(loc, name, SILLinkage::Shared,
                             type, isBareSILFunction, isTransparent, isSerialized,
                             isThunk, SILFunction::NotRelevant);
}

SILFunction *SILModule::createFunction(
    SILLinkage linkage, StringRef name, CanSILFunctionType loweredType,
    GenericEnvironment *genericEnv, Optional<SILLocation> loc,
    IsBare_t isBareSILFunction, IsTransparent_t isTrans,
    IsSerialized_t isSerialized, IsThunk_t isThunk,
    SILFunction::ClassVisibility_t classVisibility,
    Inline_t inlineStrategy, EffectsKind EK, SILFunction *InsertBefore,
    const SILDebugScope *DebugScope) {
  return SILFunction::create(*this, linkage, name, loweredType, genericEnv, loc,
                             isBareSILFunction, isTrans, isSerialized, isThunk,
                             classVisibility, inlineStrategy, EK, InsertBefore,
                             DebugScope);
}

const IntrinsicInfo &SILModule::getIntrinsicInfo(Identifier ID) {
  unsigned OldSize = IntrinsicIDCache.size();
  IntrinsicInfo &Info = IntrinsicIDCache[ID];

  // If the element was is in the cache, return it.
  if (OldSize == IntrinsicIDCache.size())
    return Info;

  // Otherwise, lookup the ID and Type and store them in the map.
  StringRef NameRef = getBuiltinBaseName(getASTContext(), ID.str(), Info.Types);
  Info.ID = (llvm::Intrinsic::ID)getLLVMIntrinsicID(NameRef);

  return Info;
}

const BuiltinInfo &SILModule::getBuiltinInfo(Identifier ID) {
  unsigned OldSize = BuiltinIDCache.size();
  BuiltinInfo &Info = BuiltinIDCache[ID];

  // If the element was is in the cache, return it.
  if (OldSize == BuiltinIDCache.size())
    return Info;

  // Otherwise, lookup the ID and Type and store them in the map.
  // Find the matching ID.
  StringRef OperationName =
    getBuiltinBaseName(getASTContext(), ID.str(), Info.Types);

  // Several operation names have suffixes and don't match the name from
  // Builtins.def, so handle those first.
  if (OperationName.startswith("fence_"))
    Info.ID = BuiltinValueKind::Fence;
  else if (OperationName.startswith("cmpxchg_"))
    Info.ID = BuiltinValueKind::CmpXChg;
  else if (OperationName.startswith("atomicrmw_"))
    Info.ID = BuiltinValueKind::AtomicRMW;
  else if (OperationName.startswith("atomicload_"))
    Info.ID = BuiltinValueKind::AtomicLoad;
  else if (OperationName.startswith("atomicstore_"))
    Info.ID = BuiltinValueKind::AtomicStore;
  else if (OperationName.startswith("allocWithTailElems_"))
    Info.ID = BuiltinValueKind::AllocWithTailElems;
  else {
    // Switch through the rest of builtins.
    Info.ID = llvm::StringSwitch<BuiltinValueKind>(OperationName)
#define BUILTIN(ID, Name, Attrs) \
      .Case(Name, BuiltinValueKind::ID)
#include "swift/AST/Builtins.def"
      .Default(BuiltinValueKind::None);
  }

  return Info;
}

SILFunction *SILModule::lookUpFunction(SILDeclRef fnRef) {
  auto name = fnRef.mangle();
  return lookUpFunction(name);
}

bool SILModule::linkFunction(SILFunction *Fun, SILModule::LinkingMode Mode) {
  return SILLinkerVisitor(*this, getSILLoader(), Mode).processFunction(Fun);
}

bool SILModule::linkFunction(StringRef Name, SILModule::LinkingMode Mode) {
  return SILLinkerVisitor(*this, getSILLoader(), Mode).processFunction(Name);
}

SILFunction *SILModule::findFunction(StringRef Name, SILLinkage Linkage) {
  assert((Linkage == SILLinkage::Public ||
          Linkage == SILLinkage::PublicExternal) &&
         "Only a lookup of public functions is supported currently");

  SILFunction *F = nullptr;

  // First, check if there is a function with a required name in the
  // current module.
  SILFunction *CurF = lookUpFunction(Name);

  // Nothing to do if the current module has a required function
  // with a proper linkage already.
  if (CurF && CurF->getLinkage() == Linkage) {
    F = CurF;
  } else {
    assert((!CurF || CurF->getLinkage() != Linkage) &&
           "hasFunction should be only called for functions that are not "
           "contained in the SILModule yet or do not have a required linkage");
  }

  if (!F) {
    SILLinkerVisitor Visitor(*this, getSILLoader(),
                             SILModule::LinkingMode::LinkNormal);
    if (CurF) {
      // Perform this lookup only if a function with a given
      // name is present in the current module.
      // This is done to reduce the amount of IO from the
      // swift module file.
      if (!Visitor.hasFunction(Name, Linkage))
        return nullptr;
      // The function in the current module will be changed.
      F = CurF;
    }

    // If function with a given name wasn't seen anywhere yet
    // or if it is known to exist, perform a lookup.
    if (!F) {
      // Try to load the function from other modules.
      F = Visitor.lookupFunction(Name, Linkage);
      // Bail if nothing was found and we are not sure if
      // this function exists elsewhere.
      if (!F)
        return nullptr;
      assert(F && "SILFunction should be present in one of the modules");
      assert(F->getLinkage() == Linkage && "SILFunction has a wrong linkage");
    }
  }

  // If a function exists already and it is a non-optimizing
  // compilation, simply convert it into an external declaration,
  // so that a compiled version from the shared library is used.
  if (F->isDefinition() &&
      F->getModule().getOptions().Optimization <
          SILOptions::SILOptMode::Optimize) {
    F->convertToDeclaration();
  }
  if (F->isExternalDeclaration())
    F->setSerialized(IsSerialized_t::IsNotSerialized);
  F->setLinkage(Linkage);
  return F;
}

bool SILModule::hasFunction(StringRef Name) {
  if (lookUpFunction(Name))
    return true;
  SILLinkerVisitor Visitor(*this, getSILLoader(),
                           SILModule::LinkingMode::LinkNormal);
  return Visitor.hasFunction(Name);
}

void SILModule::linkAllWitnessTables() {
  getSILLoader()->getAllWitnessTables();
}

void SILModule::linkAllVTables() {
  getSILLoader()->getAllVTables();
}

void SILModule::invalidateSILLoaderCaches() {
  getSILLoader()->invalidateCaches();
}

void SILModule::removeFromZombieList(StringRef Name) {
  if (auto *Zombie = ZombieFunctionTable.lookup(Name)) {
    ZombieFunctionTable.erase(Name);
    zombieFunctions.remove(Zombie);
  }
}

/// Erase a function from the module.
void SILModule::eraseFunction(SILFunction *F) {

  assert(! F->isZombie() && "zombie function is in list of alive functions");
  if (F->isInlined() || F->isExternallyUsedSymbol()) {
    
    // The owner of the function's Name is the FunctionTable key. As we remove
    // the function from the table we have to store the name string elsewhere:
    // in zombieFunctionNames.
    StringRef copiedName = F->getName().copy(zombieFunctionNames);
    FunctionTable.erase(F->getName());
    F->Name = copiedName;
    
    // The function is dead, but we need it later (at IRGen) for debug info
    // or vtable stub generation. So we move it into the zombie list.
    getFunctionList().remove(F);
    zombieFunctions.push_back(F);
    ZombieFunctionTable[copiedName] = F;
    F->setZombie();

    // This opens dead-function-removal opportunities for called functions.
    // (References are not needed anymore.)
    F->dropAllReferences();
  } else {
    FunctionTable.erase(F->getName());
    getFunctionList().erase(F);
  }
}

void SILModule::invalidateFunctionInSILCache(SILFunction *F) {
  getSILLoader()->invalidateFunction(F);
}

/// Erase a global SIL variable from the module.
void SILModule::eraseGlobalVariable(SILGlobalVariable *G) {
  GlobalVariableMap.erase(G->getName());
  getSILGlobalList().erase(G);
}

SILVTable *SILModule::lookUpVTable(const ClassDecl *C) {
  if (!C)
    return nullptr;

  // First try to look up R from the lookup table.
  auto R = VTableMap.find(C);
  if (R != VTableMap.end())
    return R->second;

  // If that fails, try to deserialize it. If that fails, return nullptr.
  SILVTable *Vtbl =
      SILLinkerVisitor(*this, getSILLoader(), SILModule::LinkingMode::LinkAll)
          .processClassDecl(C);
  if (!Vtbl)
    return nullptr;

  // If we succeeded, map C -> VTbl in the table and return VTbl.
  VTableMap[C] = Vtbl;
  return Vtbl;
}

SerializedSILLoader *SILModule::getSILLoader() {
  // If the SILLoader is null, create it.
  if (!SILLoader)
    SILLoader = SerializedSILLoader::create(getASTContext(), this,
                                            Callback.get());
  // Return the SerializedSILLoader.
  return SILLoader.get();
}

/// \brief Given a conformance \p C and a protocol requirement \p Requirement,
/// search the witness table for the conformance and return the witness thunk
/// for the requirement.
std::pair<SILFunction *, SILWitnessTable *>
SILModule::lookUpFunctionInWitnessTable(ProtocolConformanceRef C,
                                        SILDeclRef Requirement) {
  // Look up the witness table associated with our protocol conformance from the
  // SILModule.
  auto Ret = lookUpWitnessTable(C);

  // If no witness table was found, bail.
  if (!Ret) {
    DEBUG(llvm::dbgs() << "        Failed speculative lookup of witness for: ";
          C.dump(); Requirement.dump());
    return std::make_pair(nullptr, nullptr);
  }

  // Okay, we found the correct witness table. Now look for the method.
  for (auto &Entry : Ret->getEntries()) {
    // Look at method entries only.
    if (Entry.getKind() != SILWitnessTable::WitnessKind::Method)
      continue;

    SILWitnessTable::MethodWitness MethodEntry = Entry.getMethodWitness();
    // Check if this is the member we were looking for.
    if (MethodEntry.Requirement != Requirement)
      continue;

    return std::make_pair(MethodEntry.Witness, Ret);
  }

  return std::make_pair(nullptr, nullptr);
}

/// \brief Given a protocol \p Protocol and a requirement \p Requirement,
/// search the protocol's default witness table and return the default
/// witness thunk for the requirement.
std::pair<SILFunction *, SILDefaultWitnessTable *>
SILModule::lookUpFunctionInDefaultWitnessTable(const ProtocolDecl *Protocol,
                                               SILDeclRef Requirement,
                                               bool deserializeLazily) {
  // Look up the default witness table associated with our protocol from the
  // SILModule.
  auto Ret = lookUpDefaultWitnessTable(Protocol, deserializeLazily);

  // If no default witness table was found, bail.
  //
  // FIXME: Could be an assert if we fix non-single-frontend mode to link
  // together serialized SIL emitted by each translation unit.
  if (!Ret) {
    DEBUG(llvm::dbgs() << "        Failed speculative lookup of default "
          "witness for " << Protocol->getName() << " ";
          Requirement.dump());
    return std::make_pair(nullptr, nullptr);
  }

  // Okay, we found the correct default witness table. Now look for the method.
  for (auto &Entry : Ret->getEntries()) {
    // Ignore dummy entries semitted for non-method requirements, as well as
    // requirements without default implementations.
    if (!Entry.isValid())
      continue;

    // Check if this is the member we were looking for.
    if (Entry.getRequirement() != Requirement)
      continue;

    return std::make_pair(Entry.getWitness(), Ret);
  }

  // This requirement doesn't have a default implementation.
  return std::make_pair(nullptr, nullptr);
}

static ClassDecl *getClassDeclSuperClass(ClassDecl *Class) {
  Type T = Class->getSuperclass();
  if (!T)
    return nullptr;
  return T->getCanonicalType()->getClassOrBoundGenericClass();
}

SILFunction *
SILModule::
lookUpFunctionInVTable(ClassDecl *Class, SILDeclRef Member) {
  // Until we reach the top of the class hierarchy...
  while (Class) {
    // Try to lookup a VTable for Class from the module...
    auto *Vtbl = lookUpVTable(Class);

    // Bail, if the lookup of VTable fails.
    if (!Vtbl) {
      return nullptr;
    }

    // Ok, we have a VTable. Try to lookup the SILFunction implementation from
    // the VTable.
    if (SILFunction *F = Vtbl->getImplementation(*this, Member))
      return F;

    // If we fail to lookup the SILFunction, again skip Class and attempt to
    // resolve the method in the VTable of the super class of Class if such a
    // super class exists.
    Class = getClassDeclSuperClass(Class);
  }

  return nullptr;
}


void SILModule::
registerDeleteNotificationHandler(DeleteNotificationHandler* Handler) {
  // Ask the handler (that can be an analysis, a pass, or some other data
  // structure) if it wants to receive delete notifications.
  if (Handler->needsNotifications()) {
    NotificationHandlers.insert(Handler);
  }
}

void SILModule::
removeDeleteNotificationHandler(DeleteNotificationHandler* Handler) {
  NotificationHandlers.remove(Handler);
}

void SILModule::notifyDeleteHandlers(ValueBase *V) {
  for (auto *Handler : NotificationHandlers) {
    Handler->handleDeleteNotification(V);
  }
}

// TODO: We should have an "isNoReturn" bit on Swift's BuiltinInfo, but for
// now, let's recognize noreturn intrinsics and builtins specially here.
bool SILModule::isNoReturnBuiltinOrIntrinsic(Identifier Name) {
  const auto &IntrinsicInfo = getIntrinsicInfo(Name);
  if (IntrinsicInfo.ID != llvm::Intrinsic::not_intrinsic) {
    return IntrinsicInfo.hasAttribute(llvm::Attribute::NoReturn);
  }
  const auto &BuiltinInfo = getBuiltinInfo(Name);
  switch (BuiltinInfo.ID) {
  default:
    return false;
  case BuiltinValueKind::Unreachable:
  case BuiltinValueKind::CondUnreachable:
  case BuiltinValueKind::UnexpectedError:
  case BuiltinValueKind::ErrorInMain:
    return true;
  }
}
