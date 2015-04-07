//===--- SILModule.cpp - SILModule implementation -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-module"
#include "swift/SIL/SILModule.h"
#include "Linker.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILExternalSource.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;
using namespace Lowering;

namespace swift {
  /// SILTypeList - The uniqued backing store for the SILValue type list.  This
  /// is only exposed out of SILValue as an ArrayRef of types, so it should
  /// never be used outside of libSIL.
  class SILTypeList : public llvm::FoldingSetNode {
  public:
    unsigned NumTypes;
    SILType Types[1];  // Actually variable sized.

    void Profile(llvm::FoldingSetNodeID &ID) const {
      for (unsigned i = 0, e = NumTypes; i != e; ++i) {
        ID.AddPointer(Types[i].getOpaqueValue());
      }
    }
  };
} // end namespace swift.

void SILExternalSource::anchor() {
}

/// SILTypeListUniquingType - This is the type of the folding set maintained by
/// SILModule that these things are uniqued into.
typedef llvm::FoldingSet<SILTypeList> SILTypeListUniquingType;

class SILModule::SerializationCallback : public SerializedSILLoader::Callback {
  void didDeserialize(Module *M, SILFunction *fn) override {
    updateLinkage(fn);
  }

  void didDeserialize(Module *M, SILGlobalVariable *var) override {
    updateLinkage(var);
    
    // For globals we currently do not support available_externally.
    // In the interpreter it would result in two instances for a single global:
    // one in the imported module and one in the main module.
    var->setDeclaration(true);
  }

  void didDeserialize(Module *M, SILVTable *vtable) override {
    // TODO: should vtables get linkage?
    //updateLinkage(vtable);
  }

  void didDeserialize(Module *M, SILWitnessTable *wt) override {
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

SILModule::SILModule(Module *SwiftModule, SILOptions &Options,
                     const DeclContext *associatedDC,
                     bool wholeModule)
  : TheSwiftModule(SwiftModule), AssociatedDeclContext(associatedDC),
    Stage(SILStage::Raw), Callback(new SILModule::SerializationCallback()),
    wholeModule(wholeModule), Options(Options), Types(*this) {
  TypeListUniquing = new SILTypeListUniquingType();
}

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

  delete (SILTypeListUniquingType*)TypeListUniquing;
}

SILWitnessTable *
SILModule::createWitnessTableDeclaration(ProtocolConformance *C,
                                         SILLinkage linkage) {
  // If we are passed in a null conformance (a valid value), just return nullptr
  // since we can not map a witness table to it.
  if (!C)
    return nullptr;

  // Extract the base NormalProtocolConformance.
  NormalProtocolConformance *NormalC = C->getRootNormalConformance();

  SILWitnessTable *WT = SILWitnessTable::create(*this,
                                                linkage,
                                                NormalC);
  return WT;
}

std::pair<SILWitnessTable *, ArrayRef<Substitution>>
SILModule::
lookUpWitnessTable(const ProtocolConformance *C, bool deserializeLazily) {
  // If we have a null conformance passed in (a legal value), just return
  // nullptr.
  ArrayRef<Substitution> Subs;
  if (!C)
    return {nullptr, Subs};

  // Walk down to the base NormalProtocolConformance.
  const ProtocolConformance *ParentC = C;
  while (!isa<NormalProtocolConformance>(ParentC)) {
    switch (ParentC->getKind()) {
    case ProtocolConformanceKind::Normal:
      llvm_unreachable("should have exited the loop?!");
    case ProtocolConformanceKind::Inherited:
      ParentC = cast<InheritedProtocolConformance>(ParentC)
        ->getInheritedConformance();
      break;
    case ProtocolConformanceKind::Specialized: {
      auto SC = cast<SpecializedProtocolConformance>(ParentC);
      ParentC = SC->getGenericConformance();
      assert(Subs.empty() && "multiple conformance specializations?!");
      Subs = SC->getGenericSubstitutions();
      break;
    }
    }
  }
  const NormalProtocolConformance *NormalC
    = cast<NormalProtocolConformance>(ParentC);

  // If the normal conformance is for a generic type, and we didn't hit a
  // specialized conformance, collect the substitutions from the generic type.
  // FIXME: The AST should do this for us.
  if (NormalC->getType()->isSpecialized() && Subs.empty()) {
    Subs = NormalC->getType()
      ->gatherAllSubstitutions(NormalC->getDeclContext()->getParentModule(),
                               nullptr);
  }

  // Attempt to lookup the witness table from the table.
  auto found = WitnessTableLookupCache.find(NormalC);
  if (found == WitnessTableLookupCache.end()) {
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
    return {nullptr, Subs};
  }

  SILWitnessTable *wT = found->second;
  assert(wT != nullptr && "Should never map a conformance to a null witness"
                          " table.");

  // If we have a definition, return it.
  if (wT->isDefinition())
    return {wT, Subs};

  // Otherwise try to deserialize it. If we succeed return the deserialized
  // function.
  //
  // *NOTE* In practice, wT will be deserializedTable, but I do not want to rely
  // on that behavior for now.
  if (deserializeLazily)
    if (auto deserializedTable = getSILLoader()->lookupWitnessTable(wT))
      return {deserializedTable, Subs};

  // If we fail, just return the declaration.
  return {wT, Subs};
}

SILFunction *SILModule::getOrCreateFunction(SILLocation loc,
                                            StringRef name,
                                            SILLinkage linkage,
                                            CanSILFunctionType type,
                                            IsBare_t isBareSILFunction,
                                            IsTransparent_t isTransparent,
                                            IsFragile_t isFragile,
                                            IsThunk_t isThunk,
                                            SILFunction::ClassVisibility_t CV) {
  if (auto fn = lookUpFunction(name)) {
    assert(fn->getLoweredFunctionType() == type);
    assert(fn->getLinkage() == linkage);
    return fn;
  }

  auto fn = SILFunction::create(*this, linkage, name, type, nullptr,
                                loc, isBareSILFunction, isTransparent,
                                isFragile, isThunk, CV);
  fn->setDebugScope(new (*this) SILDebugScope(loc, *fn));
  return fn;
}

static SILFunction::ClassVisibility_t getClassVisibility(SILDeclRef constant) {
  if (!constant.hasDecl())
    return SILFunction::NotRelevant;

  // If this decleration is a function which goes into a vtable, then it's
  // symbol must be as visible as its class. Derived classes even have to put
  // all less visible methods of the base class into their vtables.

  auto *FD = dyn_cast<AbstractFunctionDecl>(constant.getDecl());
  if (!FD)
    return SILFunction::NotRelevant;

  DeclContext *context = FD->getDeclContext();

  // Methods from extensions don't go into vtables (yet).
  if (context->isExtensionContext())
    return SILFunction::NotRelevant;

  auto *classType = context->isClassOrClassExtensionContext();
  if (!classType || classType->isFinal())
    return SILFunction::NotRelevant;

  if (FD->isFinal() && !FD->getOverriddenDecl())
    return SILFunction::NotRelevant;

  assert(FD->getEffectiveAccess() <= classType->getEffectiveAccess() &&
         "class must be as visible as its members");

  switch (classType->getEffectiveAccess()) {
    case Accessibility::Private:
      return SILFunction::NotRelevant;
    case Accessibility::Internal:
      return SILFunction::InternalClass;
    case Accessibility::Public:
      return SILFunction::PublicClass;
  }
}

SILFunction *SILModule::getOrCreateFunction(SILLocation loc,
                                            SILDeclRef constant,
                                            ForDefinition_t forDefinition) {

  SmallVector<char, 128> buffer;
  auto name = constant.mangle(buffer);
  auto constantType = Types.getConstantType(constant).castTo<SILFunctionType>();
  SILLinkage linkage = constant.getLinkage(forDefinition);

  if (auto fn = lookUpFunction(name)) {
    assert(fn->getLoweredFunctionType() == constantType);
    assert(fn->getLinkage() == linkage);
    return fn;
  }

  IsTransparent_t IsTrans = constant.isTransparent()?
                              IsTransparent : IsNotTransparent;
  IsFragile_t IsFrag = IsNotFragile;
  if (IsTrans == IsTransparent && (linkage == SILLinkage::Public
                                   || linkage == SILLinkage::PublicExternal)) {
    IsFrag = IsFragile;
  }

  EffectsKind EK = constant.hasEffectsAttribute() ?
  constant.getEffectsAttribute() : EffectsKind::Unspecified;

  Inline_t inlineStrategy = InlineDefault;
  if (constant.isNoinline())
    inlineStrategy = NoInline;
  else if (constant.isAlwaysInline())
    inlineStrategy = AlwaysInline;



  auto *F = SILFunction::create(*this, linkage, name,
                                constantType, nullptr,
                                None, IsNotBare, IsTrans, IsFrag, IsNotThunk,
                                getClassVisibility(constant),
                                inlineStrategy, EK);

  if (forDefinition == ForDefinition_t::ForDefinition)
    F->setDebugScope(new (*this) SILDebugScope(loc, *F));

  F->setGlobalInit(constant.isGlobal());
  if (constant.hasDecl())
    if (auto SemanticsA =
        constant.getDecl()->getAttrs().getAttribute<SemanticsAttr>())
      F->setSemanticsAttr(SemanticsA->Value);

  ValueDecl *VD = nullptr;
  if (constant.hasDecl())
    VD = constant.getDecl();

  F->setDeclContext(VD);

  return F;
}


SILFunction *SILModule::getOrCreateSharedFunction(SILLocation loc,
                                                  StringRef name,
                                                  CanSILFunctionType type,
                                                  IsBare_t isBareSILFunction,
                                                  IsTransparent_t isTransparent,
                                                  IsFragile_t isFragile,
                                                  IsThunk_t isThunk) {
  return getOrCreateFunction(loc, name, SILLinkage::Shared,
                             type, isBareSILFunction, isTransparent, isFragile,
                             isThunk, SILFunction::NotRelevant);
}

ArrayRef<SILType> ValueBase::getTypes() const {
  // No results.
  if (TypeOrTypeList.isNull())
    return ArrayRef<SILType>();
  // Arbitrary list of results.
  if (auto *TypeList = TypeOrTypeList.dyn_cast<SILTypeList*>())
    return ArrayRef<SILType>(TypeList->Types, TypeList->NumTypes);
  // Single result.
  return TypeOrTypeList.get<SILType>();
}



/// getSILTypeList - Get a uniqued pointer to a SIL type list.  This can only
/// be used by SILValue.
SILTypeList *SILModule::getSILTypeList(ArrayRef<SILType> Types) const {
  assert(Types.size() > 1 && "Shouldn't use type list for 0 or 1 types");
  auto UniqueMap = (SILTypeListUniquingType*)TypeListUniquing;

  llvm::FoldingSetNodeID ID;
  for (auto T : Types) {
    ID.AddPointer(T.getOpaqueValue());
  }

  // If we already have this type list, just return it.
  void *InsertPoint = 0;
  if (SILTypeList *TypeList = UniqueMap->FindNodeOrInsertPos(ID, InsertPoint))
    return TypeList;

  // Otherwise, allocate a new one.
  void *NewListP = BPA.Allocate(sizeof(SILTypeList)+
                                sizeof(SILType)*(Types.size()-1),
                                alignof(SILTypeList));
  SILTypeList *NewList = new (NewListP) SILTypeList();
  NewList->NumTypes = Types.size();
  std::copy(Types.begin(), Types.end(), NewList->Types);

  UniqueMap->InsertNode(NewList, InsertPoint);
  return NewList;
}

const IntrinsicInfo &SILModule::getIntrinsicInfo(Identifier ID) {
  unsigned OldSize = IntrinsicIDCache.size();
  IntrinsicInfo &Info = IntrinsicIDCache[ID];

  // If the element was is in the cache, return it.
  if (OldSize == IntrinsicIDCache.size())
    return Info;

  // Otherwise, lookup the ID and Type and store them in the map.
  StringRef NameRef = getBuiltinBaseName(getASTContext(), ID.str(), Info.Types);
  Info.ID =
    (llvm::Intrinsic::ID)getLLVMIntrinsicID(NameRef, !Info.Types.empty());

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
  llvm::SmallString<32> name;
  fnRef.mangle(name);
  return lookUpFunction(name);
}

bool SILModule::linkFunction(SILFunction *Fun, SILModule::LinkingMode Mode) {
  return SILLinkerVisitor(*this, getSILLoader(), Mode,
                          ExternalSource).processFunction(Fun);
}

bool SILModule::linkFunction(SILDeclRef Decl, SILModule::LinkingMode Mode) {
  return SILLinkerVisitor(*this, getSILLoader(), Mode,
                          ExternalSource).processDeclRef(Decl);
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
    F->setZombie();

    // This opens dead-function-removal opportunities for called functions.
    // (References are not needed anymore.)
    F->dropAllReferences();
  } else {
    FunctionTable.erase(F->getName());
    getFunctionList().erase(F);
  }
}

SILVTable *SILModule::lookUpVTable(const ClassDecl *C) {
  if (!C)
    return nullptr;

  // First try to look up R from the lookup table.
  auto R = VTableLookupTable.find(C);
  if (R != VTableLookupTable.end())
    return R->second;

  // If that fails, try to deserialize it. If that fails, return nullptr.
  SILVTable *Vtbl = SILLinkerVisitor(*this, getSILLoader(),
                                     SILModule::LinkingMode::LinkAll,
                                     ExternalSource).processClassDecl(C);
  if (!Vtbl)
    return nullptr;

  // If we succeeded, map C -> VTbl in the table and return VTbl.
  VTableLookupTable[C] = Vtbl;
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


/// \brief Given a protocol \p Proto, a member method \p Member and a concrete
/// class type \p ConcreteTy, search the witness tables and return the static
/// function that matches the member with any specializations may be
/// required. Notice that we do not scan the class hierarchy, just the concrete
/// class type.
std::tuple<SILFunction *, SILWitnessTable *, ArrayRef<Substitution>>
SILModule::lookUpFunctionInWitnessTable(const ProtocolConformance *C,
                                        SILDeclRef Member) {
  // Look up the witness table associated with our protocol conformance from the
  // SILModule.
  auto Ret = lookUpWitnessTable(C);

  // If no witness table was found, bail.
  if (!Ret.first) {
    DEBUG(llvm::dbgs() << "        Failed speculative lookup of witness for: ";
          C->dump());
    return std::make_tuple(nullptr, nullptr, ArrayRef<Substitution>());
  }

  // Okay, we found the correct witness table. Now look for the method.
  for (auto &Entry : Ret.first->getEntries()) {
    // Look at method entries only.
    if (Entry.getKind() != SILWitnessTable::WitnessKind::Method)
      continue;

    SILWitnessTable::MethodWitness MethodEntry = Entry.getMethodWitness();
    // Check if this is the member we were looking for.
    if (MethodEntry.Requirement != Member)
      continue;

    return std::make_tuple(MethodEntry.Witness, Ret.first, Ret.second);
  }

  return std::make_tuple(nullptr, nullptr, ArrayRef<Substitution>());
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
