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

#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILExternalSource.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumFuncLinked, "Number of SIL functions linked");

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
      decl->setLinkage(SILLinkage::Shared);
      return;
    case SILLinkage::Private: // ?
    case SILLinkage::PublicExternal:
    case SILLinkage::HiddenExternal:
      return;
    }
  }
};

SILModule::SILModule(Module *SwiftModule)
  : TheSwiftModule(SwiftModule), Stage(SILStage::Raw),
    Callback(new SILModule::SerializationCallback()), Types(*this) {
  TypeListUniquing = new SILTypeListUniquingType();
  SILLoader = SerializedSILLoader::create(getASTContext(), this,
                                          Callback.get());

}

SILModule::~SILModule() {
  delete (SILTypeListUniquingType*)TypeListUniquing;
}

SILWitnessTable *
SILModule::createWitnessTableDeclaration(ProtocolConformance *C) {
  // Walk down to the base NormalProtocolConformance.
  ProtocolConformance *ParentC = C;
  ArrayRef<Substitution> Subs;
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
  NormalProtocolConformance *NormalC
    = cast<NormalProtocolConformance>(ParentC);

  SILWitnessTable *WT = SILWitnessTable::create(*this,
                                                SILLinkage::PublicExternal,
                                                NormalC);
  return WT;
}

std::pair<SILWitnessTable *, ArrayRef<Substitution>>
SILModule::lookUpWitnessTable(const ProtocolConformance *C) {
  // Walk down to the base NormalProtocolConformance.
  const ProtocolConformance *ParentC = C;
  ArrayRef<Substitution> Subs;
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
  if (auto deserializedTable = SILLoader->lookupWitnessTable(wT))
    return {deserializedTable, Subs};

  // If we fail, just return the declaration.
  return {wT, Subs};
}

SILFunction *SILModule::getOrCreateSharedFunction(SILLocation loc,
                                                  StringRef name,
                                                  CanSILFunctionType type,
                                                  IsBare_t isBareSILFunction,
                                                IsTransparent_t isTransparent) {
  auto linkage = SILLinkage::Shared;

  if (auto fn = lookUpFunction(name)) {
    assert(fn->getLoweredFunctionType() == type);
    assert(fn->getLinkage() == linkage);
    return fn;
  }

  return SILFunction::create(*this, linkage, name, type, nullptr,
                             loc, isBareSILFunction, isTransparent);
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

namespace {

/// Visitor that knows how to link in dependencies of SILInstructions.
class SILLinkerVisitor : public SILInstructionVisitor<SILLinkerVisitor, bool> {
  using LinkingMode = SILModule::LinkingMode;

  /// The SILLoader that this visitor is using to link.
  SerializedSILLoader *Loader;

  /// The external SIL source to use when linking this module.
  SILExternalSource *ExternalSource = nullptr;

  /// Worklist of SILFunctions we are processing.
  llvm::SmallVector<SILFunction *, 128> Worklist;

  /// A list of callees of the current instruction being visited. cleared after
  /// every instruction is visited.
  llvm::SmallVector<SILFunction *, 4> CalleeFunctions;

  /// The current linking mode.
  LinkingMode Mode;
public:

  SILLinkerVisitor(SerializedSILLoader *L, SILModule::LinkingMode M,
                   SILExternalSource *E = nullptr)
    : Loader(L), ExternalSource(E), Worklist(), CalleeFunctions(), Mode(M) { }

  /// Process F, recursively deserializing any thing F may reference.
  bool process(SILFunction *F) {
    if (Mode == LinkingMode::LinkNone)
      return false;

    auto NewFn = Loader->lookupSILFunction(F);
    if (!NewFn || NewFn->empty())
      return false;

    ++NumFuncLinked;

    Worklist.push_back(NewFn);
    while (!Worklist.empty()) {
      auto Fn = Worklist.pop_back_val();
      for (auto &BB : *Fn) {
        for (auto &I : BB) {
          // Should we try linking?
          if (visit(&I)) {
            for (auto F : CalleeFunctions) {
              // The ExternalSource may wish to rewrite non-empty bodies.
              if (!F->empty() && ExternalSource)
                if (auto NewFn = ExternalSource->lookupSILFunction(F)) {
                  NewFn->verify();
                  Worklist.push_back(NewFn);
                  ++NumFuncLinked;
                  continue;
                }

              F->setBare(IsBare);

              if (F->empty())
                if (auto NewFn = Loader->lookupSILFunction(F)) {
                  NewFn->verify();
                  Worklist.push_back(NewFn);
                  ++NumFuncLinked;
                }
            }
            CalleeFunctions.clear();
          } else {
            assert(CalleeFunctions.empty() && "CalleeFunctions should always "
                   "be empty if visit does not return true.");
          }
        }
      }
    }

    // If we return true, we deserialized at least one function.
    return true;
  }

  /// We do not want to visit callee functions if we just have a value base.
  bool visitValueBase(ValueBase *V) { return false; }

  bool visitApplyInst(ApplyInst *AI) {
    // If we don't have a function ref inst, just return false. We do not have
    // interesting callees.
    auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee().getDef());
    if (!FRI)
      return false;

    // Ok we have a function ref inst, grab the callee.
    SILFunction *Callee = FRI->getReferencedFunction();

    // If the linking mode is not link all, AI is not transparent, and the
    // callee is not shared, we don't want to perform any linking.
    if (!isLinkAll() && !AI->isTransparent() &&
        Callee->getLinkage() != SILLinkage::Shared)
      return false;

    // Otherwise we want to try and link in the callee... Add it to the callee
    // list and return true.
    addCalleeFunction(Callee);
    return true;
  }

  bool visitPartialApplyInst(PartialApplyInst *PAI) {
    auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee().getDef());
    if (!FRI)
      return false;

    SILFunction *Callee = FRI->getReferencedFunction();
    if (!isLinkAll() && !Callee->isTransparent() &&
        Callee->getLinkage() != SILLinkage::Shared)
      return false;

    addCalleeFunction(Callee);
    return true;
  }

  bool visitFunctionRefInst(FunctionRefInst *FRI) {
    if (!isLinkAll())
      return false;

    addCalleeFunction(FRI->getReferencedFunction());
    return true;
  }

private:
  /// Add a function to our callee list for processing.
  void addCalleeFunction(SILFunction *F) {
    CalleeFunctions.push_back(F);
  }

  /// Is the current mode link all? Link all implies we should try and link
  /// everything, not just transparent/shared functions.
  bool isLinkAll() const { return Mode == LinkingMode::LinkAll; }
};

} // end anonymous namespace.

bool SILModule::linkFunction(SILFunction *Fun, SILModule::LinkingMode Mode) {
  return SILLinkerVisitor(SILLoader, Mode, ExternalSource).process(Fun);
}
