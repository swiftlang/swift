//===--- ObjectOutliner.cpp - Outline heap objects -----------------------===//
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

#define DEBUG_TYPE "objectoutliner"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/AST/ASTMangler.h"
#include "llvm/Support/Debug.h"
using namespace swift;

namespace {

class ObjectOutliner {
  NominalTypeDecl *ArrayDecl = nullptr;
  int GlobIdx = 0;

  bool isCOWType(SILType type) {
    return type.getNominalOrBoundGenericNominal() == ArrayDecl;
  }

  bool isValidUseOfObject(SILInstruction *Val, bool isCOWObject,
                          ApplyInst **FindStringCall = nullptr);

  bool getObjectInitVals(SILValue Val,
                         llvm::DenseMap<VarDecl *, StoreInst *> &MemberStores,
                         llvm::SmallVectorImpl<StoreInst *> &TailStores,
                         ApplyInst **FindStringCall);
  bool handleTailAddr(int TailIdx, SILInstruction *I,
                      llvm::SmallVectorImpl<StoreInst *> &TailStores);

  bool
  optimizeObjectAllocation(AllocRefInst *ARI,
                           llvm::SmallVector<SILInstruction *, 4> &ToRemove);
  void replaceFindStringCall(ApplyInst *FindStringCall);

public:
  ObjectOutliner(NominalTypeDecl *ArrayDecl) : ArrayDecl(ArrayDecl) { }

  bool run(SILFunction *F);
};

bool ObjectOutliner::run(SILFunction *F) {
  bool hasChanged = false;

  for (auto &BB : *F) {
    auto Iter = BB.begin();

    // We can't remove instructions willy-nilly as we iterate because
    // that might cause a pointer to the next instruction to become
    // garbage, causing iterator invalidations (and crashes).
    // Instead, we collect in a list the instructions we want to remove
    // and erase the BB they belong to at the end of the loop, once we're
    // sure it's safe to do so.
    llvm::SmallVector<SILInstruction *, 4> ToRemove;

    while (Iter != BB.end()) {
      SILInstruction *I = &*Iter;
      Iter++;
      if (auto *ARI = dyn_cast<AllocRefInst>(I)) {
        hasChanged |= optimizeObjectAllocation(ARI, ToRemove);
      }
    }
    for (auto *I : ToRemove)
      I->eraseFromParent();
  }
  return hasChanged;
}

/// Get all stored properties of a class, including it's super classes.
static void getFields(ClassDecl *Cl, SmallVectorImpl<VarDecl *> &Fields) {
  if (ClassDecl *SuperCl = Cl->getSuperclassDecl()) {
    getFields(SuperCl, Fields);
  }
  for (VarDecl *Field : Cl->getStoredProperties()) {
    Fields.push_back(Field);
  }
}

/// Check if \p V is a valid instruction for a static initializer, including
/// all its operands.
static bool isValidInitVal(SILValue V) {
  if (auto I = dyn_cast<SingleValueInstruction>(V)) {
    if (!SILGlobalVariable::isValidStaticInitializerInst(I, I->getModule()))
      return false;

    for (Operand &Op : I->getAllOperands()) {
      if (!isValidInitVal(Op.get()))
        return false;
    }
    return true;
  }
  return false;
}

/// Check if a use of an object may prevent outlining the object.
///
/// If \p isCOWObject is true, then the object reference is wrapped into a
/// COW container. Currently this is just Array<T>.
/// If a use is a call to the findStringSwitchCase semantic call, the apply
/// is returned in \p FindStringCall.
bool ObjectOutliner::isValidUseOfObject(SILInstruction *I, bool isCOWObject,
                                      ApplyInst **FindStringCall) {
  switch (I->getKind()) {
  case SILInstructionKind::DebugValueAddrInst:
  case SILInstructionKind::DebugValueInst:
  case SILInstructionKind::LoadInst:
  case SILInstructionKind::DeallocRefInst:
  case SILInstructionKind::StrongRetainInst:
  case SILInstructionKind::StrongReleaseInst:
  case SILInstructionKind::FixLifetimeInst:
  case SILInstructionKind::SetDeallocatingInst:
    return true;

  case SILInstructionKind::ReturnInst:
  case SILInstructionKind::TryApplyInst:
  case SILInstructionKind::PartialApplyInst:
  case SILInstructionKind::StoreInst:
    /// We don't have a representation for COW objects in SIL, so we do some
    /// ad-hoc testing: We can ignore uses of a COW object if any use after
    /// this will do a uniqueness checking before the object is modified.
    return isCOWObject;

  case SILInstructionKind::ApplyInst:
    if (!isCOWObject)
      return false;
    // There should only be a single call to findStringSwitchCase. But even
    // if there are multiple calls, it's not problem - we'll just optimize the
    // last one we find.
    if (cast<ApplyInst>(I)->hasSemantics("findStringSwitchCase"))
      *FindStringCall = cast<ApplyInst>(I);
    return true;

  case SILInstructionKind::StructInst:
    if (isCOWType(cast<StructInst>(I)->getType())) {
      // The object is wrapped into a COW container.
      isCOWObject = true;
    }
    break;

  case SILInstructionKind::UncheckedRefCastInst:
  case SILInstructionKind::StructElementAddrInst:
  case SILInstructionKind::AddressToPointerInst:
    assert(!isCOWObject && "instruction cannot have a COW object as operand");
    break;

  case SILInstructionKind::TupleInst:
  case SILInstructionKind::TupleExtractInst:
  case SILInstructionKind::EnumInst:
    break;

  case SILInstructionKind::StructExtractInst:
    // To be on the safe side we don't consider the object as COW if it is
    // extracted again from the COW container: the uniqueness check may be
    // optimized away in this case.
    isCOWObject = false;
    break;

  case SILInstructionKind::BuiltinInst: {
    // Handle the case for comparing addresses. This occurs when the Array
    // comparison function is inlined.
    auto *BI = cast<BuiltinInst>(I);
    BuiltinValueKind K = BI->getBuiltinInfo().ID;
    if (K == BuiltinValueKind::ICMP_EQ || K == BuiltinValueKind::ICMP_NE)
      return true;
    return false;
  }

  default:
    return false;
  }

  auto SVI = cast<SingleValueInstruction>(I);
  for (Operand *Use : getNonDebugUses(SVI)) {
    if (!isValidUseOfObject(Use->getUser(), isCOWObject, FindStringCall))
      return false;
  }
  return true;
}

/// Handle the address of a tail element.
bool ObjectOutliner::handleTailAddr(int TailIdx, SILInstruction *TailAddr,
                              llvm::SmallVectorImpl<StoreInst *> &TailStores) {
  if (TailIdx >= 0 && TailIdx < (int)TailStores.size()) {
    if (auto *SI = dyn_cast<StoreInst>(TailAddr)) {
      if (!isValidInitVal(SI->getSrc()) || TailStores[TailIdx])
        return false;
      TailStores[TailIdx] = SI;
      return true;
    }
  }
  return isValidUseOfObject(TailAddr, /*isCOWObject*/false);
}

/// Get the init values for an object's stored properties and its tail elements.
bool ObjectOutliner::getObjectInitVals(SILValue Val,
                        llvm::DenseMap<VarDecl *, StoreInst *> &MemberStores,
                        llvm::SmallVectorImpl<StoreInst *> &TailStores,
                        ApplyInst **FindStringCall) {
  for (Operand *Use : Val->getUses()) {
    SILInstruction *User = Use->getUser();
    if (auto *UC = dyn_cast<UpcastInst>(User)) {
      // Upcast is transparent.
      if (!getObjectInitVals(UC, MemberStores, TailStores, FindStringCall))
        return false;
    } else if (auto *REA = dyn_cast<RefElementAddrInst>(User)) {
      // The address of a stored property.
      for (Operand *ElemAddrUse : REA->getUses()) {
        SILInstruction *ElemAddrUser = ElemAddrUse->getUser();
        if (auto *SI = dyn_cast<StoreInst>(ElemAddrUser)) {
          if (!isValidInitVal(SI->getSrc()) || MemberStores[REA->getField()])
            return false;
          MemberStores[REA->getField()] = SI;
        } else if (!isValidUseOfObject(ElemAddrUser, /*isCOWObject*/false)) {
          return false;
        }
      }
    } else if (auto *RTA = dyn_cast<RefTailAddrInst>(User)) {
      // The address of a tail element.
      for (Operand *TailUse : RTA->getUses()) {
        SILInstruction *TailUser = TailUse->getUser();
        if (auto *IA = dyn_cast<IndexAddrInst>(TailUser)) {
          // An index_addr yields the address of any tail element. Only if the
          // second operand (the index) is an integer literal we can figure out
          // which tail element is refereneced.
          int TailIdx = -1;
          if (auto *Index = dyn_cast<IntegerLiteralInst>(IA->getIndex()))
            TailIdx = Index->getValue().getZExtValue();

          for (Operand *IAUse : IA->getUses()) {
            if (!handleTailAddr(TailIdx, IAUse->getUser(), TailStores))
              return false;
          }
        // Without an index_addr it's the first tail element.
        } else if (!handleTailAddr(/*TailIdx*/0, TailUser, TailStores)) {
          return false;
        }
      }
    } else if (!isValidUseOfObject(User, /*isCOWObject*/false, FindStringCall)) {
      return false;
    }
  }
  return true;
}

class GlobalVariableMangler : public Mangle::ASTMangler {
public:
  std::string mangleOutlinedVariable(SILFunction *F, int &uniqueIdx) {
    std::string GlobName;
    do {
      beginManglingWithoutPrefix();
      appendOperator(F->getName());
      appendOperator("Tv", Index(uniqueIdx++));
      GlobName = finalize();
    } while (F->getModule().lookUpGlobalVariable(GlobName));

    return GlobName;
  }
};

/// Try to convert an object allocation into a statically initialized object.
///
/// In general this works for any class, but in practice it will only kick in
/// for array buffer objects. The use cases are array literals in a function.
/// For example:
///     func getarray() -> [Int] {
///       return [1, 2, 3]
///     }
bool ObjectOutliner::optimizeObjectAllocation(
    AllocRefInst *ARI, llvm::SmallVector<SILInstruction *, 4> &ToRemove) {

  if (ARI->isObjC())
    return false;

  // Check how many tail allocated elements are on the object.
  ArrayRef<Operand> TailCounts = ARI->getTailAllocatedCounts();
  SILType TailType;
  unsigned NumTailElems = 0;

  // We only support a single tail allocated arrays.
  // Stdlib's tail allocated arrays don't have any side-effects in the
  // constructor if the element type is trivial.
  // TODO: also exclude custom tail allocated arrays which might have
  // side-effects in the destructor.
  if (TailCounts.size() != 1)
      return false;

  // The number of tail allocated elements must be constant.
  if (auto *ILI = dyn_cast<IntegerLiteralInst>(TailCounts[0].get())) {
    if (ILI->getValue().getActiveBits() > 20)
      return false;
    NumTailElems = ILI->getValue().getZExtValue();
    TailType = ARI->getTailAllocatedTypes()[0];
  } else {
    return false;
  }

  SILType Ty = ARI->getType();
  ClassDecl *Cl = Ty.getClassOrBoundGenericClass();
  if (!Cl)
    return false;
  llvm::SmallVector<VarDecl *, 16> Fields;
  getFields(Cl, Fields);

  // Get the initialization stores of the object's properties and tail
  // allocated elements. Also check if there are any "bad" uses of the object.
  llvm::DenseMap<VarDecl *, StoreInst *> MemberStores;
  llvm::SmallVector<StoreInst *, 16> TailStores;
  TailStores.resize(NumTailElems);
  ApplyInst *FindStringCall = nullptr;
  if (!getObjectInitVals(ARI, MemberStores, TailStores, &FindStringCall))
    return false;

  // Is there a store for all the class properties?
  if (MemberStores.size() != Fields.size())
    return false;

  // Is there a store for all tail allocated elements?
  for (auto V : TailStores) {
    if (!V)
      return false;
  }

  DEBUG(llvm::dbgs() << "Outline global variable in " <<
        ARI->getFunction()->getName() << '\n');

  SILModule *Module = &ARI->getFunction()->getModule();
  assert(!Cl->isResilient(Module->getSwiftModule(),
                          ResilienceExpansion::Minimal) &&
    "constructor call of resilient class should prevent static allocation");

  // Create a name for the outlined global variable.
  GlobalVariableMangler Mangler;
  std::string GlobName =
    Mangler.mangleOutlinedVariable(ARI->getFunction(), GlobIdx);

  SILGlobalVariable *Glob =
    SILGlobalVariable::create(*Module, SILLinkage::Private, IsNotSerialized,
                              GlobName, ARI->getType());

  // Schedule all init values for cloning into the initializer of Glob.
  StaticInitCloner Cloner(Glob);
  for (VarDecl *Field : Fields) {
    StoreInst *MemberStore = MemberStores[Field];
    Cloner.add(cast<SingleValueInstruction>(MemberStore->getSrc()));
  }
  for (StoreInst *TailStore : TailStores) {
    Cloner.add(cast<SingleValueInstruction>(TailStore->getSrc()));
  }

  // Create the class property initializers
  llvm::SmallVector<SILValue, 16> ObjectArgs;
  for (VarDecl *Field : Fields) {
    StoreInst *MemberStore = MemberStores[Field];
    assert(MemberStore);
    ObjectArgs.push_back(Cloner.clone(
                           cast<SingleValueInstruction>(MemberStore->getSrc())));
    ToRemove.push_back(MemberStore);
  }
  // Create the initializers for the tail elements.
  unsigned NumBaseElements = ObjectArgs.size();
  for (StoreInst *TailStore : TailStores) {
    ObjectArgs.push_back(Cloner.clone(
                           cast<SingleValueInstruction>(TailStore->getSrc())));
    ToRemove.push_back(TailStore);
  }
  // Create the initializer for the object itself.
  SILBuilder StaticInitBuilder(Glob);
  StaticInitBuilder.createObject(ArtificialUnreachableLocation(),
                                 ARI->getType(), ObjectArgs, NumBaseElements);

  // Replace the alloc_ref by global_value + strong_retain instructions.
  SILBuilder B(ARI);
  GlobalValueInst *GVI = B.createGlobalValue(ARI->getLoc(), Glob);
  B.createStrongRetain(ARI->getLoc(), GVI, B.getDefaultAtomicity());
  llvm::SmallVector<Operand *, 8> Worklist(ARI->use_begin(), ARI->use_end());
  while (!Worklist.empty()) {
    auto *Use = Worklist.pop_back_val();
    SILInstruction *User = Use->getUser();
    switch (User->getKind()) {
      case SILInstructionKind::SetDeallocatingInst:
        // set_deallocating is a replacement for a strong_release. Therefore
        // we have to insert a strong_release to balance the strong_retain which
        // we inserted after the global_value instruction.
        B.setInsertionPoint(User);
        B.createStrongRelease(User->getLoc(), GVI, B.getDefaultAtomicity());
        LLVM_FALLTHROUGH;
      case SILInstructionKind::DeallocRefInst:
        ToRemove.push_back(User);
        break;
      default:
        Use->set(GVI);
    }
  }
  if (FindStringCall && NumTailElems > 16) {
    assert(&*std::next(ARI->getIterator()) != FindStringCall &&
           "FindStringCall must not be the next instruction after ARI because "
           "deleting it would invalidate the instruction iterator");
    replaceFindStringCall(FindStringCall);
  }

  ToRemove.push_back(ARI);
  return true;
}

/// Replaces a call to _findStringSwitchCase with a call to
/// _findStringSwitchCaseWithCache which builds a cache (e.g. a Dictionary) and
/// stores it into a global variable. Then subsequent calls to this function can
/// do a fast lookup using the cache.
void ObjectOutliner::replaceFindStringCall(ApplyInst *FindStringCall) {
  // Find the replacement function in the swift stdlib.
  SmallVector<ValueDecl *, 1> results;
  SILModule *Module = &FindStringCall->getFunction()->getModule();
  Module->getASTContext().lookupInSwiftModule("_findStringSwitchCaseWithCache",
                                              results);
  if (results.size() != 1)
    return;

  auto *FD = dyn_cast<FuncDecl>(results.front());
  if (!FD)
    return;

  SILDeclRef declRef(FD, SILDeclRef::Kind::Func);
  SILFunction *replacementFunc = Module->getOrCreateFunction(
      FindStringCall->getLoc(), declRef, NotForDefinition);

  SILFunctionType *FTy = replacementFunc->getLoweredFunctionType();
  if (FTy->getNumParameters() != 3)
    return;

  SILType cacheType = FTy->getParameters()[2].getSILStorageType().getObjectType();
  NominalTypeDecl *cacheDecl = cacheType.getNominalOrBoundGenericNominal();
  if (!cacheDecl)
    return;


  assert(!cacheDecl->isResilient(Module->getSwiftModule(),
                                 ResilienceExpansion::Minimal));

  SILType wordTy = cacheType.getFieldType(
                            cacheDecl->getStoredProperties().front(), *Module);

  GlobalVariableMangler Mangler;
  std::string GlobName =
    Mangler.mangleOutlinedVariable(FindStringCall->getFunction(), GlobIdx);

  // Create an "opaque" global variable which is passed as inout to
  // _findStringSwitchCaseWithCache and into which the function stores the
  // "cache".
  SILGlobalVariable *CacheVar =
    SILGlobalVariable::create(*Module, SILLinkage::Private, IsNotSerialized,
                              GlobName, cacheType);

  SILLocation Loc = FindStringCall->getLoc();
  SILBuilder StaticInitBuilder(CacheVar);
  auto *Zero = StaticInitBuilder.createIntegerLiteral(Loc, wordTy, 0);
  StaticInitBuilder.createStruct(ArtificialUnreachableLocation(), cacheType,
                                 {Zero, Zero});

  SILBuilder B(FindStringCall);
  GlobalAddrInst *CacheAddr = B.createGlobalAddr(FindStringCall->getLoc(),
                                                 CacheVar);
  FunctionRefInst *FRI = B.createFunctionRef(FindStringCall->getLoc(),
                                             replacementFunc);
  ApplyInst *NewCall = B.createApply(FindStringCall->getLoc(), FRI,
                                     FindStringCall->getSubstitutions(),
                                     { FindStringCall->getArgument(0),
                                       FindStringCall->getArgument(1),
                                       CacheAddr },
                                     FindStringCall->isNonThrowing());

  FindStringCall->replaceAllUsesWith(NewCall);
  FindStringCall->eraseFromParent();
}

class ObjectOutlinerPass : public SILFunctionTransform
{
  void run() override {
    SILFunction *F = getFunction();
    ObjectOutliner Outliner(F->getModule().getASTContext().getArrayDecl());
    if (Outliner.run(F)) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createObjectOutliner() {
  return new ObjectOutlinerPass();
}
