//===---- SILCombinerVisitors.cpp - SILCombiner Visitor Impl -*- C++ -*----===//
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

#define DEBUG_TYPE "sil-combine"
#include "SILCombiner.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "swift/SILAnalysis/CFG.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"

using namespace swift;
using namespace swift::PatternMatch;

SILInstruction *SILCombiner::visitStructExtractInst(StructExtractInst *SEI) {
  // If our operand has archetypes or our field is not trivial, do not do
  // anything.
  SILValue Op = SEI->getOperand();
  SILType OpType = Op.getType();
  if (OpType.hasArchetype() || OpType.isTrivial(SEI->getModule()))
    return nullptr;

  // (struct_extract (unchecked_ref_bit_cast X->Y x) #z)
  //    ->
  // (unchecked_ref_bit_cast X->Z x)
  //
  // Where #z is a Z typed field of single field struct Y.
  auto *URBCI = dyn_cast<UncheckedRefBitCastInst>(Op);
  if (!URBCI)
    return nullptr;

  // If we only have one stored property, then we are layout compatible with
  // that property and can perform the operation.
  StructDecl *S = SEI->getStructDecl();
  auto R = S->getStoredProperties();
  auto B = R.begin();
  if (B == R.end())
    return nullptr;
  ++B;
  if (B != R.end())
    return nullptr;

  return Builder.createUncheckedRefBitCast(SEI->getLoc(), URBCI->getOperand(),
                                           SEI->getType());
}

SILInstruction*
SILCombiner::visitAllocExistentialBoxInst(AllocExistentialBoxInst *AEBI) {

  // Optimize away the pattern below that happens when exceptions are created
  // and in some cases, due to inlining, are not needed.
  //
  //   %6 = alloc_existential_box $ErrorType, $ColorError
  //   %7 = enum $VendingMachineError, #ColorError.Red
  //   store %7 to %6#1 : $*ColorError
  //   debug_value %6#0 : $ErrorType
  //   strong_release %6#0 : $ErrorType

  StoreInst *SingleStore = nullptr;
  StrongReleaseInst *SingleRelease = nullptr;

  // For each user U of the alloc_existential_box...
  for (auto U : getNonDebugUses(*AEBI)) {
    // Record stores into the box.
    if (auto *SI = dyn_cast<StoreInst>(U->getUser())) {
      // If this is not the only store into the box then bail out.
      if (SingleStore) return nullptr;
      SingleStore = SI;
      continue;
    }

    // Record releases of the box.
    if (auto *RI = dyn_cast<StrongReleaseInst>(U->getUser())) {
      // If this is not the only release of the box then bail out.
      if (SingleRelease) return nullptr;
      SingleRelease = RI;
      continue;
    }

    // If there are other users to the box then bail out.
    return nullptr;
  }

  if (SingleStore && SingleRelease) {
    // Release the value that was stored into the existential box. The box
    // is going away so we need to release the stored value now.
    Builder.setInsertionPoint(SingleStore);
    Builder.createReleaseValue(AEBI->getLoc(), SingleStore->getSrc());

    // Erase the instruction that stores into the box and the release that
    // releases the box, and finally, release the box.
    eraseInstFromFunction(*SingleRelease);
    eraseInstFromFunction(*SingleStore);
    return eraseInstFromFunction(*AEBI);
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitSwitchEnumAddrInst(SwitchEnumAddrInst *SEAI) {
  // Promote switch_enum_addr to switch_enum if the enum is loadable.
  //   switch_enum_addr %ptr : $*Optional<SomeClass>, case ...
  //     ->
  //   %value = load %ptr
  //   switch_enum %value
  SILType Ty = SEAI->getOperand().getType();
  if (!Ty.isLoadable(SEAI->getModule()))
    return nullptr;

  SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> Cases;
  for (int i = 0, e = SEAI->getNumCases(); i < e; ++i)
    Cases.push_back(SEAI->getCase(i));


  SILBasicBlock *Default = SEAI->hasDefault() ? SEAI->getDefaultBB() : 0;
  LoadInst *EnumVal = Builder.createLoad(SEAI->getLoc(), SEAI->getOperand());
  EnumVal->setDebugScope(SEAI->getDebugScope());
  Builder.createSwitchEnum(SEAI->getLoc(), EnumVal, Default, Cases)
    ->setDebugScope(SEAI->getDebugScope());
  return eraseInstFromFunction(*SEAI);
}

SILInstruction *SILCombiner::visitSelectEnumAddrInst(SelectEnumAddrInst *SEAI) {
  // Canonicalize a select_enum_addr: if the default refers to exactly one case,
  // then replace the default with that case.
  if (SEAI->hasDefault()) {
    NullablePtr<EnumElementDecl> elementDecl = SEAI->getUniqueCaseForDefault();
    if (elementDecl.isNonNull()) {
      // Construct a new instruction by copying all the case entries.
      SmallVector<std::pair<EnumElementDecl *, SILValue>, 4> CaseValues;
      for (int idx = 0, numIdcs = SEAI->getNumCases(); idx < numIdcs; idx++) {
        CaseValues.push_back(SEAI->getCase(idx));
      }
      // Add the default-entry of the original instruction as case-entry.
      CaseValues.push_back(
          std::make_pair(elementDecl.get(), SEAI->getDefaultResult()));

      return Builder.createSelectEnumAddr(SEAI->getLoc(),
                                          SEAI->getEnumOperand(),
                                          SEAI->getType(), SILValue(),
                                          CaseValues);
    }
  }

  // Promote select_enum_addr to select_enum if the enum is loadable.
  //   = select_enum_addr %ptr : $*Optional<SomeClass>, case ...
  //     ->
  //   %value = load %ptr
  //   = select_enum %value
  SILType Ty = SEAI->getEnumOperand().getType();
  if (!Ty.isLoadable(SEAI->getModule()))
    return nullptr;

  SmallVector<std::pair<EnumElementDecl*, SILValue>, 8> Cases;
  for (int i = 0, e = SEAI->getNumCases(); i < e; ++i)
    Cases.push_back(SEAI->getCase(i));

  SILValue Default = SEAI->hasDefault() ? SEAI->getDefaultResult() : SILValue();
  LoadInst *EnumVal = Builder.createLoad(SEAI->getLoc(),
                                          SEAI->getEnumOperand());
  EnumVal->setDebugScope(SEAI->getDebugScope());
  auto *I = Builder.createSelectEnum(SEAI->getLoc(), EnumVal, SEAI->getType(),
                                     Default, Cases);
  I->setDebugScope(SEAI->getDebugScope());
  return I;
}

SILInstruction *SILCombiner::visitSelectValueInst(SelectValueInst *SVI) {
  return nullptr;
}

SILInstruction *SILCombiner::visitSwitchValueInst(SwitchValueInst *SVI) {
  return nullptr;
}
SILInstruction *SILCombiner::visitAllocStackInst(AllocStackInst *AS) {
  // init_existential_addr instructions behave like memory allocation within
  // the allocated object. We can promote the init_existential_addr allocation
  // into a dedicated allocation.

  // Detect this pattern
  // %0 = alloc_stack $LogicValue
  // %1 = init_existential_addr %0#1 : $*LogicValue, $*Bool
  // ...
  // use of %1
  // ...
  // destroy_addr %0#1 : $*LogicValue
  // dealloc_stack %0#0 : $*@local_storage LogicValue
  //

  // At the same time also look for dead alloc_stack live ranges that are only
  // copied into.
  // %0 = alloc_stack
  // copy_addr %src, %0
  // destroy_addr %0#1 : $*LogicValue
  // dealloc_stack %0#0 : $*@local_storage LogicValue

  bool LegalUsers = true;
  InitExistentialAddrInst *IEI = nullptr;
  OpenExistentialAddrInst *OEI = nullptr;
  bool HaveSeenCopyInto = false;
  // Scan all of the uses of the AllocStack and check if it is not used for
  // anything other than the init_existential_addr/open_existential_addr container.
  for (Operand *Op: getNonDebugUses(*AS)) {
    // Destroy and dealloc are both fine.
    if (isa<DestroyAddrInst>(Op->getUser()) ||
        isa<DeinitExistentialAddrInst>(Op->getUser()) ||
        isa<DeallocStackInst>(Op->getUser()))
      continue;

    // Make sure there is exactly one init_existential_addr.
    if (auto *I = dyn_cast<InitExistentialAddrInst>(Op->getUser())) {
      if (IEI || HaveSeenCopyInto) {
        LegalUsers = false;
        break;
      }
      IEI = I;
      continue;
    }

    // Make sure there is exactly one open_existential_addr.
    if (auto *I = dyn_cast<OpenExistentialAddrInst>(Op->getUser())) {
      if (OEI) {
        LegalUsers = false;
        break;
      }

      // This open_existential should not have any uses except destroy_addr.
      for (auto Use: getNonDebugUses(*I)) {
        if (!isa<DestroyAddrInst>(Use->getUser())) {
          LegalUsers = false;
          break;
        }
      }

      if (!LegalUsers)
        break;

      OEI = I;
      continue;
    }

    if (auto *CopyAddr = dyn_cast<CopyAddrInst>(Op->getUser())) {
      if (IEI) {
        LegalUsers = false;
        break;
      }
      // Copies into the alloc_stack live range are safe.
      if (CopyAddr->getDest().getDef() == AS) {
        HaveSeenCopyInto = true;
        continue;
      }

      LegalUsers = false;
      break;
    }

    // All other instructions are illegal.
    LegalUsers = false;
    break;
  }

  // Save the original insertion point.
  auto OrigInsertionPoint = Builder.getInsertionPoint();

  // If the only users of the alloc_stack are alloc, destroy and
  // init_existential_addr then we can promote the allocation of the init
  // existential.
  if (LegalUsers && IEI && !OEI) {
    auto *ConcAlloc = Builder.createAllocStack(AS->getLoc(),
                                                IEI->getLoweredConcreteType());
    ConcAlloc->setDebugScope(AS->getDebugScope());
    SILValue(IEI, 0).replaceAllUsesWith(ConcAlloc->getAddressResult());
    eraseInstFromFunction(*IEI);


    for (Operand *Op: AS->getUses()) {
      if (auto *DA = dyn_cast<DestroyAddrInst>(Op->getUser())) {
        Builder.setInsertionPoint(DA);
        Builder.createDestroyAddr(DA->getLoc(), SILValue(ConcAlloc, 1))
          ->setDebugScope(DA->getDebugScope());
        eraseInstFromFunction(*DA);

      }
      if (auto *DS = dyn_cast<DeallocStackInst>(Op->getUser())) {
        Builder.setInsertionPoint(DS);
        Builder.createDeallocStack(DS->getLoc(), SILValue(ConcAlloc, 0))
          ->setDebugScope(DS->getDebugScope());
        eraseInstFromFunction(*DS);
      }
    }

    eraseInstFromFunction(*AS);
    // Restore the insertion point.
    Builder.setInsertionPoint(OrigInsertionPoint);
  }

  // Remove a dead live range that is only copied into.
  if (LegalUsers && HaveSeenCopyInto && !IEI) {
    SmallPtrSet<SILInstruction *, 16> ToDelete;

    for (auto *Op : AS->getUses()) {
      // Replace a copy_addr [take] %src ... by a destroy_addr %src if %src is
      // no the alloc_stack.
      // Otherwise, just delete the copy_addr.
      if (auto *CopyAddr = dyn_cast<CopyAddrInst>(Op->getUser())) {
        if (CopyAddr->isTakeOfSrc() && CopyAddr->getSrc().getDef() != AS) {
          Builder.setInsertionPoint(CopyAddr);
          Builder.createDestroyAddr(CopyAddr->getLoc(), CopyAddr->getSrc())
              ->setDebugScope(CopyAddr->getDebugScope());
        }
      }
      if (auto *OEAI = dyn_cast<OpenExistentialAddrInst>(Op->getUser())) {
        for (auto *Op : OEAI->getUses()) {
          assert(isa<DestroyAddrInst>(Op->getUser()) ||
                 isDebugInst(Op->getUser()) && "Unexpected instruction");
          ToDelete.insert(Op->getUser());
        }
      }
      assert(isa<CopyAddrInst>(Op->getUser()) ||
             isa<OpenExistentialAddrInst>(Op->getUser()) ||
             isa<DestroyAddrInst>(Op->getUser()) ||
             isa<DeallocStackInst>(Op->getUser()) ||
             isa<DeinitExistentialAddrInst>(Op->getUser()) ||
             isDebugInst(Op->getUser()) && "Unexpected instruction");
      ToDelete.insert(Op->getUser());
    }

    // Erase the 'live-range'
    for (auto *Inst : ToDelete) {
      Inst->replaceAllUsesWithUndef();
      eraseInstFromFunction(*Inst);
    }
    eraseInstFromFunction(*AS);

    // Restore the insertion point.
    Builder.setInsertionPoint(OrigInsertionPoint);
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitLoadInst(LoadInst *LI) {
  // (load (upcast-ptr %x)) -> (upcast-ref (load %x))
  if (auto *UI = dyn_cast<UpcastInst>(LI->getOperand())) {
    auto NewLI = Builder.createLoad(LI->getLoc(), UI->getOperand());
    NewLI->setDebugScope(LI->getDebugScope());
    return Builder.createUpcast(LI->getLoc(), NewLI, LI->getType());
  }

  // Given a load with multiple struct_extracts/tuple_extracts and no other
  // uses, canonicalize the load into several (struct_element_addr (load))
  // pairs.
  using ProjInstPairTy = std::pair<Projection, SILInstruction *>;

  // Go through the loads uses and add any users that are projections to the
  // projection list.
  llvm::SmallVector<ProjInstPairTy, 8> Projections;
  for (auto *UI : getNonDebugUses(*LI)) {
    auto *User = UI->getUser();

    // If we have any non SEI, TEI instruction, don't do anything here.
    if (!isa<StructExtractInst>(User) && !isa<TupleExtractInst>(User))
      return nullptr;

    auto P = Projection::valueProjectionForInstruction(User);
    Projections.push_back({P.getValue(), User});
  }

  // The reason why we sort the list is so that we will process projections with
  // the same value decl and tuples with the same indices together. This makes
  // it easy to reuse the load from the first such projection for all subsequent
  // projections on the same value decl or index.
  std::sort(Projections.begin(), Projections.end());

  // Go through our sorted list creating new GEPs only when we need to.
  Projection *LastProj = nullptr;
  LoadInst *LastNewLoad = nullptr;
  for (auto &Pair : Projections) {
    auto &Proj = Pair.first;
    auto *Inst = Pair.second;

    // If this projection is the same as the last projection we processed, just
    // replace all uses of the projection with the load we created previously.
    if (LastProj && Proj == *LastProj) {
      replaceInstUsesWith(*Inst, LastNewLoad, 0);
      eraseInstFromFunction(*Inst);
      continue;
    }

    // Ok, we have started to visit the range of instructions associated with
    // a new projection. Create the new address projection.
    auto I = Proj.createAddrProjection(Builder, LI->getLoc(), LI->getOperand());
    LastProj = &Proj;
    I.get()->setDebugScope(LI->getDebugScope());
    LastNewLoad = Builder.createLoad(LI->getLoc(), I.get());
    LastNewLoad->setDebugScope(LI->getDebugScope());
    replaceInstUsesWith(*Inst, LastNewLoad, 0);
    eraseInstFromFunction(*Inst);
  }

  // Erase the old load.
  return eraseInstFromFunction(*LI);
}

SILInstruction *SILCombiner::visitReleaseValueInst(ReleaseValueInst *RVI) {
  SILValue Operand = RVI->getOperand();
  SILType OperandTy = Operand.getType();

  // Destroy value of an enum with a trivial payload or no-payload is a no-op.
  if (auto *EI = dyn_cast<EnumInst>(Operand)) {
    if (!EI->hasOperand() ||
        EI->getOperand().getType().isTrivial(EI->getModule()))
      return eraseInstFromFunction(*RVI);

    // retain_value of an enum_inst where we know that it has a payload can be
    // reduced to a retain_value on the payload.
    if (EI->hasOperand()) {
      return Builder.createReleaseValue(RVI->getLoc(), EI->getOperand());
    }
  }

  // ReleaseValueInst of an unowned type is an unowned_release.
  if (OperandTy.is<UnownedStorageType>())
    return Builder.createUnownedRelease(RVI->getLoc(), Operand);

  // ReleaseValueInst of a reference type is a strong_release.
  if (OperandTy.isReferenceCounted(RVI->getModule()))
    return Builder.createStrongRelease(RVI->getLoc(), Operand);

  // ReleaseValueInst of a trivial type is a no-op.
  if (OperandTy.isTrivial(RVI->getModule()))
    return eraseInstFromFunction(*RVI);

  // Do nothing for non-trivial non-reference types.
  return nullptr;
}

SILInstruction *SILCombiner::visitRetainValueInst(RetainValueInst *RVI) {
  SILValue Operand = RVI->getOperand();
  SILType OperandTy = Operand.getType();

  // retain_value of an enum with a trivial payload or no-payload is a no-op +
  // RAUW.
  if (auto *EI = dyn_cast<EnumInst>(Operand)) {
    if (!EI->hasOperand() ||
        EI->getOperand().getType().isTrivial(RVI->getModule())) {
      return eraseInstFromFunction(*RVI);
    }

    // retain_value of an enum_inst where we know that it has a payload can be
    // reduced to a retain_value on the payload.
    if (EI->hasOperand()) {
      return Builder.createRetainValue(RVI->getLoc(), EI->getOperand());
    }
  }

  // RetainValueInst of an unowned type is an unowned_retain.
  if (OperandTy.is<UnownedStorageType>())
    return Builder.createUnownedRetain(RVI->getLoc(), Operand);

  // RetainValueInst of a reference type is a strong_release.
  if (OperandTy.isReferenceCounted(RVI->getModule())) {
    return Builder.createStrongRetain(RVI->getLoc(), Operand);
  }

  // RetainValueInst of a trivial type is a no-op + use propogation.
  if (OperandTy.isTrivial(RVI->getModule())) {
    return eraseInstFromFunction(*RVI);
  }

  // Sometimes in the stdlib due to hand offs, we will see code like:
  //
  // release_value %0
  // retain_value %0
  //
  // with the matching retain_value to the release_value in a predecessor basic
  // block and the matching release_value for the retain_value_retain in a
  // successor basic block.
  //
  // Due to the matching pairs being in different basic blocks, the ARC
  // Optimizer (which is currently local to one basic block does not handle
  // it). But that does not mean that we can not eliminate this pair with a
  // peephole.

  // If we are not the first instruction in this basic block...
  if (RVI != &*RVI->getParent()->begin()) {
    SILBasicBlock::iterator Pred = RVI;
    --Pred;

    // ...and the predecessor instruction is a release_value on the same value
    // as our retain_value...
    if (ReleaseValueInst *Release = dyn_cast<ReleaseValueInst>(&*Pred))
      // Remove them...
      if (Release->getOperand() == RVI->getOperand()) {
        eraseInstFromFunction(*Release);
        return eraseInstFromFunction(*RVI);
      }
  }

  return nullptr;
}


/// Check that this is a partial apply of a reabstraction thunk and return the
/// argument of the partial apply if it is.
static SILValue
isPartialApplyOfReabstractionThunk(PartialApplyInst *PAI, bool requireSingleUse) {
  if (requireSingleUse) {
    SILValue PAIVal(PAI, 0);
    if (!hasOneNonDebugUse(PAIVal))
      return SILValue();
  }

  if (PAI->getNumArguments() != 1)
    return SILValue();

  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
  if (!FRI)
    return SILValue();
  auto *Fun = FRI->getReferencedFunction();
  if (!Fun)
    return SILValue();

  // Make sure we have a reabstraction thunk.
  if (Fun->isThunk() != IsReabstractionThunk)
    return SILValue();

  // The argument should be a closure.
  auto Arg = PAI->getArgument(0);
  if (!Arg.getType().is<SILFunctionType>() ||
      !Arg.getType().isReferenceCounted(PAI->getFunction()->getModule()))
    return SILValue();

  return PAI->getArgument(0);
}

/// Remove pointless reabstraction thunk closures.
///   partial_apply %reabstraction_thunk_typeAtoB(
///      partial_apply %reabstraction_thunk_typeBtoA %closure_typeB))
///   ->
///   %closure_typeB
static bool foldInverseReabstractionThunks(PartialApplyInst *PAI,
                                           SILCombiner *Combiner) {
  auto PAIArg = isPartialApplyOfReabstractionThunk(PAI, false);
  if (!PAIArg)
    return false;

  auto *PAI2 = dyn_cast<PartialApplyInst>(PAIArg);
  if (!PAI2)
    return false;

  auto PAI2Arg = isPartialApplyOfReabstractionThunk(PAI2, true);
  if (!PAI2Arg)
    return false;

  // The types must match.
  if (PAI->getType() != PAI2->getArgument(0).getType())
    return false;

  // Replace the partial_apply(partial_apply(X)) by X and remove the
  // partial_applies.

  Combiner->replaceInstUsesWith(*PAI, PAI2->getArgument(0).getDef());
  Combiner->eraseInstFromFunction(*PAI);
  assert(hasNoUsesExceptDebug(PAI2) && "Should not have any uses");
  Combiner->eraseInstFromFunction(*PAI2);

  return true;
}

SILInstruction *SILCombiner::visitPartialApplyInst(PartialApplyInst *PAI) {
  // partial_apply without any substitutions or arguments is just a
  // thin_to_thick_function.
  if (!PAI->hasSubstitutions() && (PAI->getNumArguments() == 0))
    return Builder.createThinToThickFunction(PAI->getLoc(), PAI->getCallee(),
                                             PAI->getType());

  // partial_apply %reabstraction_thunk_typeAtoB(
  //    partial_apply %reabstraction_thunk_typeBtoA %closure_typeB))
  // -> %closure_typeB
  if (foldInverseReabstractionThunks(PAI, this))
    return nullptr;

  tryOptimizeApplyOfPartialApply(PAI);

  // Try to delete dead closures.
  tryDeleteDeadClosure(
      PAI, InstModCallbacks(
               [this](SILInstruction *DeadInst) {
                 eraseInstFromFunction(*DeadInst);
               },
               [this](SILInstruction *NewInst) { Worklist.add(NewInst); }));
  return nullptr;
}

static bool canCombinePartialApply(const PartialApplyInst *PAI) {
  // Only process partial apply if the callee is a known function.
  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
  if (!FRI)
    return false;

  // Make sure that the substitution list of the PAI does not contain any
  // archetypes.
  ArrayRef<Substitution> Subs = PAI->getSubstitutions();
  for (Substitution S : Subs)
    if (S.getReplacement()->getCanonicalType()->hasArchetype())
      return false;

  return true;
}

// Helper class performing the apply{partial_apply(x,y)}(z) -> apply(z,x,y)
// peephole.
class PartialApplyCombiner {
  // True if temporaries are not created yet.
  bool isFirstTime = true;

  // partial_apply which is being processed.
  PartialApplyInst *PAI;

  // Temporaries created as copies of alloc_stack arguments of
  // the partial_apply.
  SmallVector<SILValue, 8> Tmps;

  // Mapping from the original argument of partial_apply to
  // the temporary containing its copy.
  llvm::DenseMap<SILValue, SILValue> ArgToTmp;

  // Set of lifetime endpoints for this partial_apply.
  //
  // Used to find the last uses of partial_apply, which is need to insert
  // releases/destroys of temporaries as early as possible.  If no releases are
  // needed, Lifetime remains empty.
  ValueLifetime Lifetime;

  SILBuilder &Builder;

  CallGraph *CG;

  // Function referenced by partial_apply.
  FunctionRefInst *FRI;

  SILCombiner *SilCombiner;

  void processSingleApply(FullApplySite AI);
  void allocateTemporaries();
  void deallocateTemporaries();
  void releaseTemporaries();

public:
  PartialApplyCombiner(PartialApplyInst *PAI, SILBuilder &Builder,
                       CallGraph *CG, SILCombiner *SilCombiner)
      : isFirstTime(true), PAI(PAI), Builder(Builder), CG(CG),
        FRI(nullptr), SilCombiner(SilCombiner) {}
  SILInstruction *combine();
};

void PartialApplyCombiner::allocateTemporaries() {
  // Copy non-inout alloc_stack arguments of the partial_apply into
  // newly created temporaries and use these temporaries instead of
  // the original arguments afterwards.
  // This is done to "extend" the life-time of original alloc_stack
  // arguments, as they may be deallocated before the last use by one
  // of the apply instructions.
  // TODO:
  // Copy arguments of the partial_apply into new temporaries
  // only if the lifetime of arguments ends before their uses
  // by apply instructions.
  bool needsReleases = false;
  CanSILFunctionType PAITy =
      dyn_cast<SILFunctionType>(PAI->getCallee().getType().getSwiftType());

  // Emit a destroy value for each captured closure argument.
  ArrayRef<SILParameterInfo> Params = PAITy->getParameters();
  auto Args = PAI->getArguments();
  unsigned Delta = Params.size() - Args.size();

  for (unsigned AI = 0, AE = Args.size(); AI != AE; ++AI) {
    SILValue Arg = Args[AI];
    SILParameterInfo Param = Params[AI + Delta];
    if (Param.isIndirectInOut())
      continue;
    if (isa<AllocStackInst>(Arg)) {
      Builder.setInsertionPoint(PAI->getFunction()->begin()->begin());
      // Create a new temporary at the beginning of a function.
      auto *Tmp = Builder.createAllocStack(PAI->getLoc(), Arg.getType());
      Builder.setInsertionPoint(PAI);
      // Copy argument into this temporary.
      Builder.createCopyAddr(PAI->getLoc(), Arg, SILValue(Tmp, 1),
                              IsTake_t::IsNotTake,
                              IsInitialization_t::IsInitialization);

      Tmps.push_back(SILValue(Tmp, 0));
      if (!Arg.getType().isTrivial(PAI->getModule()))
        needsReleases = true;
      ArgToTmp.insert(std::make_pair(Arg, SILValue(Tmp, 0)));
    }
  }

  if (needsReleases) {
    // Compute the set of endpoints, which will be used
    // to insert releases of temporaries.
    ValueLifetimeAnalysis VLA(PAI);
    Lifetime = VLA.computeFromDirectUses();
  }
}

/// Emit dealloc_stack for all temporaries.
void PartialApplyCombiner::deallocateTemporaries() {
  // Insert dealloc_stack instructions.
  TinyPtrVector<SILBasicBlock *> ExitBBs;
  findAllNonFailureExitBBs(PAI->getFunction(), ExitBBs);

  for (auto Op : Tmps) {
    for (auto *ExitBB : ExitBBs) {
      auto *Term = ExitBB->getTerminator();
      Builder.setInsertionPoint(Term);
      Builder.createDeallocStack(PAI->getLoc(), Op);
    }
  }
}

/// Emit code to release/destory temporaries.
void PartialApplyCombiner::releaseTemporaries() {
  // Insert releases and destroy_addrs as early as possible,
  // because we don't want to keep objects alive longer than
  // its really needed.
  for (auto Op : Tmps) {
    auto TmpType = Op.getType().getObjectType();
    if (TmpType.isTrivial(PAI->getModule()))
      continue;
    for (auto *EndPoint : Lifetime.LastUsers) {
      Builder.setInsertionPoint(next(SILBasicBlock::iterator(EndPoint)));
      auto TmpAddr = SILValue(Op.getDef(), 1);
      if (!TmpType.isAddressOnly(PAI->getModule())) {
        auto *Load = Builder.createLoad(PAI->getLoc(), TmpAddr);
        Builder.createReleaseValue(PAI->getLoc(), Load);
      } else {
        Builder.createDestroyAddr(PAI->getLoc(), TmpAddr);
      }
    }
  }
}

/// Process an apply instruction which uses a partial_apply
/// as its callee.
void PartialApplyCombiner::processSingleApply(FullApplySite AI) {
  Builder.setInsertionPoint(AI.getInstruction());

  // Prepare the args.
  SmallVector<SILValue, 8> Args;
  // First the ApplyInst args.
  for (auto Op : AI.getArguments())
    Args.push_back(Op);

  SILInstruction *InsertionPoint = Builder.getInsertionPoint();
  // Next, the partial apply args.

  // Pre-process partial_apply arguments only once, lazily.
  if (isFirstTime) {
    isFirstTime = false;
    allocateTemporaries();
  }

  // Now, copy over the partial apply args.
  for (auto Op : PAI->getArguments()) {
    auto Arg = Op;
    // If there is new temporary for this argument, use it instead.
    if (isa<AllocStackInst>(Arg)) {
      if (ArgToTmp.count(Arg)) {
        auto Tmp = ArgToTmp.lookup(Arg);
        Op = SILValue(Tmp.getDef(), 1);
      }
    }
    Args.push_back(Op);
  }

  Builder.setInsertionPoint(InsertionPoint);

  // The thunk that implements the partial apply calls the closure function
  // that expects all arguments to be consumed by the function. However, the
  // captured arguments are not arguments of *this* apply, so they are not
  // pre-incremented. When we combine the partial_apply and this apply into
  // a new apply we need to retain all of the closure non-address type
  // arguments.
  for (auto Arg : PAI->getArguments())
    if (!Arg.getType().isAddress())
      Builder.emitRetainValueOperation(PAI->getLoc(), Arg);

  auto *F = FRI->getReferencedFunction();
  SILType FnType = F->getLoweredType();
  SILType ResultTy = F->getLoweredFunctionType()->getSILResult();
  ArrayRef<Substitution> Subs = PAI->getSubstitutions();
  if (!Subs.empty()) {
    FnType = FnType.substGenericArgs(PAI->getModule(), Subs);
    ResultTy = FnType.getAs<SILFunctionType>()->getSILResult();
  }

  FullApplySite NAI;
  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    NAI =
      Builder.createTryApply(AI.getLoc(), FRI, FnType, Subs, Args,
                              TAI->getNormalBB(), TAI->getErrorBB());
  else
    NAI =
      Builder.createApply(AI.getLoc(), FRI, FnType, ResultTy, Subs, Args,
                           cast<ApplyInst>(AI)->isNonThrowing());

  NAI.getInstruction()->setDebugScope(AI.getDebugScope());

  CallGraphEditor(CG).addEdgesForApply(NAI);

  // We also need to release the partial_apply instruction itself because it
  // is consumed by the apply_instruction.
  if (auto *TAI = dyn_cast<TryApplyInst>(AI)) {
    Builder.setInsertionPoint(TAI->getNormalBB()->begin());
    Builder.createStrongRelease(AI.getLoc(), PAI)
      ->setDebugScope(AI.getDebugScope());
    Builder.setInsertionPoint(TAI->getErrorBB()->begin());
    Builder.createStrongRelease(AI.getLoc(), PAI)
      ->setDebugScope(AI.getDebugScope());
    Builder.setInsertionPoint(AI.getInstruction());
  } else {
    Builder.createStrongRelease(AI.getLoc(), PAI)
      ->setDebugScope(AI.getDebugScope());
  }
  // Update the set endpoints.
  if (Lifetime.LastUsers.count(AI.getInstruction())) {
    Lifetime.LastUsers.remove(AI.getInstruction());
    Lifetime.LastUsers.insert(NAI.getInstruction());
  }

  SilCombiner->replaceInstUsesWith(*AI.getInstruction(), NAI.getInstruction());
  SilCombiner->eraseInstFromFunction(*AI.getInstruction());
}

/// Perform the apply{partial_apply(x,y)}(z) -> apply(z,x,y) peephole
/// by iterating over all uses of the partial_apply and searching
/// for the pattern to transform.
SILInstruction *PartialApplyCombiner::combine() {
  if (!canCombinePartialApply(PAI))
    return nullptr;

  // Only process partial apply if the callee is a known function.
  FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());

  // Iterate over all uses of the partial_apply
  // and look for applies that use it as a callee.
  for (auto Use : PAI->getUses()) {
    auto User = Use->getUser();
    // If this use of a partial_apply is not
    // an apply which uses it as a callee, bail.
    auto AI = FullApplySite::isa(User);
    if (!AI)
      continue;

    if (AI.getCallee() != PAI)
      continue;

    // We cannot handle generic apply yet. Bail.
    if (AI.hasSubstitutions())
      continue;

    processSingleApply(AI);
  }

  // release/destroy and deallocate introduced temporaries.
  if (!Tmps.empty()) {
    releaseTemporaries();
    deallocateTemporaries();
  }

  return nullptr;
}

/// Iterate over all uses of a given partial_apply and check
/// if any of those uses are apply instructions. Try to
/// combine those applies with this partial_apply.
SILInstruction *
SILCombiner::tryOptimizeApplyOfPartialApply(PartialApplyInst *PAI) {

  PartialApplyCombiner PACombiner(PAI, Builder, CG, this);
  return PACombiner.combine();
}

SILInstruction *
SILCombiner::optimizeApplyOfConvertFunctionInst(FullApplySite AI,
                                                ConvertFunctionInst *CFI) {
  // We only handle simplification of static function references. If we don't
  // have one, bail.
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CFI->getOperand());
  if (!FRI)
    return nullptr;

  // Grab our relevant callee types...
  CanSILFunctionType SubstCalleeTy = AI.getSubstCalleeType();
  auto ConvertCalleeTy =
      CFI->getOperand().getType().castTo<SILFunctionType>();

  // ... and make sure they have no unsubstituted generics. If they do, bail.
  if (SubstCalleeTy->hasArchetype() || ConvertCalleeTy->hasArchetype())
    return nullptr;

  // Ok, we can now perform our transformation. Grab AI's operands and the
  // relevant types from the ConvertFunction function type and AI.
  OperandValueArrayRef Ops = AI.getArgumentsWithoutIndirectResult();
  auto OldOpTypes = SubstCalleeTy->getParameterSILTypes();
  auto NewOpTypes = ConvertCalleeTy->getParameterSILTypes();

  assert(Ops.size() == OldOpTypes.size() &&
         "Ops and op types must have same size.");
  assert(Ops.size() == NewOpTypes.size() &&
         "Ops and op types must have same size.");

  llvm::SmallVector<SILValue, 8> Args;
  for (unsigned i = 0, e = Ops.size(); i != e; ++i) {
    SILValue Op = Ops[i];
    SILType OldOpType = OldOpTypes[i];
    SILType NewOpType = NewOpTypes[i];

    // Convert function takes refs to refs, address to addresses, and leaves
    // other types alone.
    if (OldOpType.isAddress()) {
      assert(NewOpType.isAddress() && "Addresses should map to addresses.");
      auto UAC = Builder.createUncheckedAddrCast(AI.getLoc(), Op, NewOpType);
      UAC->setDebugScope(AI.getDebugScope());
      Args.push_back(UAC);
    } else if (OldOpType.isHeapObjectReferenceType()) {
      assert(NewOpType.isHeapObjectReferenceType() &&
             "refs should map to refs.");
      auto URC = Builder.createUncheckedRefCast(AI.getLoc(), Op, NewOpType);
      URC->setDebugScope(AI.getDebugScope());
      Args.push_back(URC);
    } else {
      Args.push_back(Op);
    }
  }

  SILType CCSILTy = SILType::getPrimitiveObjectType(ConvertCalleeTy);
  // Create the new apply inst.
  SILInstruction *NAI;
  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    NAI = Builder.createTryApply(AI.getLoc(), FRI, CCSILTy,
                                 ArrayRef<Substitution>(), Args,
                                 TAI->getNormalBB(), TAI->getErrorBB());
  else
    NAI = Builder.createApply(AI.getLoc(), FRI, CCSILTy,
                              ConvertCalleeTy->getSILResult(),
                              ArrayRef<Substitution>(), Args,
                              cast<ApplyInst>(AI)->isNonThrowing());
  NAI->setDebugScope(AI.getDebugScope());
  return NAI;
}

typedef SmallVector<SILInstruction*, 4> UserListTy;
/// \brief Returns a list of instructions that project or perform reference
/// counting operations on the instruction or its uses in argument \p Inst.
/// The function returns False if there are non-ARC instructions.
static bool recursivelyCollectARCUsers(UserListTy &Uses, SILInstruction *Inst) {
  Uses.push_back(Inst);
  for (auto Inst : Inst->getUses()) {
    if (isa<RefCountingInst>(Inst->getUser()) ||
        isa<DebugValueInst>(Inst->getUser())) {
      Uses.push_back(Inst->getUser());
      continue;
    }
    SILInstruction *Proj;
    if ((Proj = dyn_cast<TupleExtractInst>(Inst->getUser())) ||
        (Proj = dyn_cast<StructExtractInst>(Inst->getUser())) ||
        (Proj = dyn_cast<PointerToAddressInst>(Inst->getUser())))
      if (recursivelyCollectARCUsers(Uses, Proj))
        continue;

    return false;
  }

  return true;
}

SILInstruction *
SILCombiner::optimizeConcatenationOfStringLiterals(ApplyInst *AI) {
  // String literals concatenation optimizer.
  return tryToConcatenateStrings(AI, Builder);
}

/// Find an init_open_existential_addr or open_existential_addr, which
/// is used to initialize a given alloc_stack value.
static SILValue getInitOrOpenExistential(AllocStackInst *ASI, SILValue &Src) {
  CopyAddrInst *FoundCAI = nullptr;
  InitExistentialAddrInst *FoundIEAI = nullptr;
  bool isLegal = true;
  // Check that this alloc_stack is initialized only once.
  for (auto Use : ASI->getUses()) {
    auto *User = Use->getUser();
    if (isa<DeallocStackInst>(User) ||
        isa<DestroyAddrInst>(User) || isa<WitnessMethodInst>(User) ||
        isa<DeinitExistentialAddrInst>(User) || isa<OpenExistentialAddrInst>(User) ||
        isa<ApplyInst>(User))
      continue;
    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      if (!FoundCAI && !FoundIEAI) {
        if (CAI->getDest().getDef() == ASI)
          FoundCAI = CAI;
      }
      continue;
    }
    else if (auto *IEAI = dyn_cast<InitExistentialAddrInst>(User)) {
      if (!FoundIEAI && !FoundCAI) {
        FoundIEAI = IEAI;
        continue;
      }
    }
    isLegal = false;
    break;
  }

  SILValue SrcValue;

  if (isLegal && FoundCAI) {
    // Try to derive the type from the copy_addr that was used to
    // initialize the alloc_stack.
    SrcValue = FoundCAI->getSrc();
    if (auto *ASI = dyn_cast<AllocStackInst>(SrcValue)) {
      SILValue Tmp;
      SrcValue = getInitOrOpenExistential(ASI, Tmp);
    }
  }

  if (isLegal && FoundIEAI) {
    SrcValue = FoundIEAI;
  }

  if (!SrcValue)
    return SILValue();

  if (auto *OEAI = dyn_cast<OpenExistentialAddrInst>(SrcValue)) {
    Src = OEAI->getOperand();
    return OEAI;
  }
  if (auto *IEAI = dyn_cast<InitExistentialAddrInst>(SrcValue)) {
    Src = IEAI->getOperand();
    return IEAI;
  }

  return SrcValue;
}

/// find the init_existential, which could be used to  determine a concrete
/// type of the \p Self.
static SILInstruction *findInitExistential(FullApplySite AI, SILValue Self,
                                           SILType &InstanceType) {
  SILInstruction *InitExistential = nullptr;

  if (auto *Instance = dyn_cast<AllocStackInst>(Self)) {
    SILValue Src;
    auto Existential = getInitOrOpenExistential(Instance, Src);
    if (Existential)
      Self = Existential;
  }

  if (auto *Instance = dyn_cast<OpenExistentialAddrInst>(Self)) {
    auto Op = Instance->getOperand();
    if (auto *ASI = dyn_cast<AllocStackInst>(Op)) {
      SILValue Src;
      if (getInitOrOpenExistential(ASI, Src)) {
        if (Src)
          Op = Src;
      }
    }

    for (auto Use : Op.getUses()) {
      SILValue User = Use->getUser();

      if (auto *IE = dyn_cast<InitExistentialAddrInst>(User)) {
        // IE should dominate Instance.
        // Without a DomTree we want to be very defensive
        // and only allow this optimization when it is used
        // inside the same BB.
        if (IE->getParent() != AI.getParent())
          continue;

        InstanceType = Instance->getType();
        InitExistential = IE;
      }
    }
  }

  if (auto *Instance = dyn_cast<OpenExistentialRefInst>(Self)) {
    if (auto *IE = dyn_cast<InitExistentialRefInst>(Instance->getOperand())) {
      // IE should dominate Instance.
      // Without a DomTree we want to be very defensive
      // and only allow this optimization when it is used
      // inside the same BB.
      if (IE->getParent() != AI.getParent())
        return nullptr;
      InstanceType = Instance->getType();
      InitExistential = IE;
    }
  }

  return InitExistential;
}

/// Create a new apply instructions that uses the concrete type instead
/// of the existential type.
SILInstruction *
SILCombiner::createApplyWithConcreteType(FullApplySite AI,
                                         SILValue NewSelf,
                                         SILValue Self,
                                         CanType ConcreteType,
                                         ProtocolConformance *Conformance,
                                         SILType InstanceType) {
  // Create a set of arguments.
  SmallVector<SILValue, 8> Args;
  for (auto Arg : AI.getArgumentsWithoutSelf()) {
    Args.push_back(Arg);
  }
  Args.push_back(NewSelf);

  // Form a new set of substitutions where Self is
  // replaced by a concrete type.
  SmallVector<Substitution, 8> Substitutions;
  for (auto Subst : AI.getSubstitutions()) {
    if (Subst.getReplacement().getCanonicalTypeOrNull() ==
        Self.getType().getSwiftRValueType()) {
      auto Conformances = AI.getModule().getASTContext()
                            .Allocate<ProtocolConformance*>(1);
      Conformances[0] = Conformance;
      Substitution NewSubst(Subst.getArchetype(),
                            ConcreteType,
                            Conformances);
      Substitutions.push_back(NewSubst);
    } else
      Substitutions.push_back(Subst);
  }

  SILType SubstCalleeType = AI.getSubstCalleeSILType();

  SILType NewSubstCalleeType;

  auto FnTy = AI.getCallee().getType().getAs<SILFunctionType>();
  if (FnTy && FnTy->isPolymorphic()) {
    // Handle polymorphic functions by properly substituting
    // their parameter types.
    CanSILFunctionType SFT = FnTy->substGenericArgs(
                                        AI.getModule(),
                                        AI.getModule().getSwiftModule(),
                                        Substitutions);
    NewSubstCalleeType = SILType::getPrimitiveObjectType(SFT);
  } else {
    TypeSubstitutionMap TypeSubstitutions;
    TypeSubstitutions[InstanceType.getSwiftType().getPointer()] = ConcreteType;
    NewSubstCalleeType = SubstCalleeType.subst(AI.getModule(),
                                               AI.getModule().getSwiftModule(),
                                               TypeSubstitutions);
  }

  FullApplySite NewAI;

  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    NewAI = Builder.createTryApply(AI.getLoc(), AI.getCallee(),
                                    NewSubstCalleeType,
                                    Substitutions, Args,
                                    TAI->getNormalBB(), TAI->getErrorBB());
  else
    NewAI = Builder.createApply(AI.getLoc(), AI.getCallee(),
                                 NewSubstCalleeType,
                                 AI.getType(), Substitutions, Args,
                                 cast<ApplyInst>(AI)->isNonThrowing());

  NewAI.getInstruction()->setDebugScope(AI.getDebugScope());

  if (isa<ApplyInst>(NewAI))
    replaceInstUsesWith(*AI.getInstruction(), NewAI.getInstruction(), 0);
  eraseInstFromFunction(*AI.getInstruction());

  CallGraphEditor(CG).addEdgesForApply(NewAI);

  return NewAI.getInstruction();
}

/// Derive a concrete type of self and conformance from the init_existential
/// instruction.
static std::pair<ProtocolConformance *, CanType>
getConformanceAndConcreteType(FullApplySite AI,
                              SILInstruction *InitExistential,
                              ProtocolDecl *Protocol,
                              SILValue &NewSelf,
                              ArrayRef<ProtocolConformance*> &Conformances) {
  // Try to derive the concrete type of self from the found init_existential.
  CanType ConcreteType;
  if (auto IE = dyn_cast<InitExistentialAddrInst>(InitExistential)) {
    Conformances = IE->getConformances();
    ConcreteType = IE->getFormalConcreteType();
    NewSelf = IE;
  } else if (auto IER = dyn_cast<InitExistentialRefInst>(InitExistential)) {
    Conformances = IER->getConformances();
    ConcreteType = IER->getFormalConcreteType();
    NewSelf = IER->getOperand();
  }

  if (Conformances.empty())
    return std::make_pair(nullptr, CanType());

  // If ConcreteType depends on any archetypes, then propagating it does not
  // help resolve witness table lookups. Catch these cases before calling
  // gatherAllSubstitutions, which only works on nominal types.
  if (ConcreteType->hasArchetype())
    return std::make_pair(nullptr, CanType());

  // Check the substitutions.
  auto ConcreteTypeSubsts = ConcreteType->gatherAllSubstitutions(
      AI.getModule().getSwiftModule(), nullptr);
  if (!ConcreteTypeSubsts.empty()) {
    // Bail if any generic types parameters of the concrete type are unbound.
    if (hasUnboundGenericTypes(ConcreteTypeSubsts))
      return std::make_pair(nullptr, CanType());
    // At this point we know that all replacements use concrete types
    // and therefore the whole Lookup type is concrete. So, we can
    // propagate it, because we know how to devirtualize it.
  }

  // Find the conformance related to witness_method.
  ProtocolConformance *Conformance = nullptr;
  for (auto Con : Conformances) {
    if (Con->getProtocol() == Protocol) {
      Conformance = Con;
      break;
    }
  }

  return std::make_pair(Conformance, ConcreteType);
}

/// Propagate information about a concrete type from init_existential_addr
/// or init_existential_ref into witness_method conformances and into
/// apply instructions.
/// This helps the devirtualizer to replace witness_method by
/// class_method instructions and then devirtualize.
SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(FullApplySite AI,
    ProtocolDecl *Protocol,
    std::function<void(CanType , ProtocolConformance *)> Propagate) {

  // Get the self argument.
  SILValue Self;
  if (auto *Apply = dyn_cast<ApplyInst>(AI)) {
    if (Apply->hasSelfArgument())
      Self = Apply->getSelfArgument();
  } else if (auto *Apply = dyn_cast<TryApplyInst>(AI)) {
    if (Apply->hasSelfArgument())
      Self = Apply->getSelfArgument();
  }

  assert (Self && "Self argument should be present");

  // Try to find the init_existential, which could be used to
  // determine a concrete type of the self.
  SILType InstanceType;
  SILInstruction *InitExistential = findInitExistential(AI, Self, InstanceType);
  if (!InitExistential)
    return nullptr;

  // Try to derive the concrete type of self and a related conformance from
  // the found init_existential.
  ArrayRef<ProtocolConformance*> Conformances;
  auto NewSelf = SILValue();
  auto ConformanceAndConcreteType =
      getConformanceAndConcreteType(AI, InitExistential,
                                    Protocol, NewSelf, Conformances);
  auto ConcreteType = ConformanceAndConcreteType.second;
  auto Conformance = ConformanceAndConcreteType.first;
  if (!Conformance)
    return nullptr;

  // Propagate the concrete type into the callee-operand if required.
  Propagate(ConcreteType, Conformance);

  // Create a new apply instructions that uses the concrete type instead
  // of the existential type.
  return createApplyWithConcreteType(AI, NewSelf, Self,
                                     ConcreteType, Conformance, InstanceType);
}

SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(FullApplySite AI,
                                                    WitnessMethodInst *WMI) {
  // Check if it is legal to perform the propagation.
  if (WMI->getConformance())
    return nullptr;

  // Don't specialize Apply instructions that return the Self type.
  // Notice that it is sufficient to compare the return type to the
  // substituted type because types that depend on the Self type are
  // not allowed (for example [Self] is not allowed).
  if (AI.getType().getSwiftType().getLValueOrInOutObjectType() ==
      WMI->getLookupType())
    return nullptr;

  // We need to handle the Self return type.
  // In we find arguments that are not the 'self' argument and if
  // they are of the Self type then we abort the optimization.
  for (auto Arg : AI.getArgumentsWithoutSelf()) {
    if (Arg.getType().getSwiftType().getLValueOrInOutObjectType() ==
        WMI->getLookupType())
      return nullptr;
  }

  // Obtain the protocol whose which should be used by the conformance.
  auto *PD = WMI->getLookupProtocol();

  // Propagate the concrete type into a callee-operand, which is a
  // witness_method instruction.
  auto PropagateIntoOperand = [this, &WMI] (CanType ConcreteType,
                                            ProtocolConformance *Conformance) {
    SILValue OptionalExistential =
        WMI->hasOperand() ? WMI->getOperand() : SILValue();
    auto *NewWMI = Builder.createWitnessMethod(WMI->getLoc(),
                                                ConcreteType,
                                                Conformance, WMI->getMember(),
                                                WMI->getType(),
                                                OptionalExistential,
                                                WMI->isVolatile());
    replaceInstUsesWith(*WMI, NewWMI, 0);
    eraseInstFromFunction(*WMI);
  };

  // Try to perform the propagation.
  return propagateConcreteTypeOfInitExistential(AI, PD, PropagateIntoOperand);
}


SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(FullApplySite AI) {
  // Check if it is legal to perform the propagation.
  if (!AI.hasSubstitutions())
    return nullptr;
  auto *FRI = dyn_cast<FunctionRefInst>(AI.getCallee());
  if (!FRI)
    return nullptr;
  auto *Callee = FRI->getReferencedFunction();
  if (!Callee->getDeclContext())
    return nullptr;

  // Bail, if there is no self argument.
  SILValue Self;
  if (auto *Apply = dyn_cast<ApplyInst>(AI)) {
    if (Apply->hasSelfArgument())
      Self = Apply->getSelfArgument();
  } else if (auto *Apply = dyn_cast<TryApplyInst>(AI)) {
    if (Apply->hasSelfArgument())
      Self = Apply->getSelfArgument();
  }
  if (!Self)
    return nullptr;

  // We need to handle the Self return type.
  // In we find arguments that are not the 'self' argument and if
  // they are of the Self type then we abort the optimization.
  for (auto Arg : AI.getArgumentsWithoutSelf()) {
    if (Arg.getType().getSwiftType().getLValueOrInOutObjectType() ==
        AI.getArguments().back().getType().getSwiftRValueType())
      return nullptr;
  }

  // Obtain the protocol whose which should be used by the conformance.
  auto *AFD = dyn_cast<AbstractFunctionDecl>(Callee->getDeclContext());
  if (!AFD)
    return nullptr;
  auto *PD = AFD->getDeclContext()->isProtocolOrProtocolExtensionContext();


  // No need to propagate anything into the callee operand.
  auto PropagateIntoOperand = [] (CanType ConcreteType,
                                  ProtocolConformance *Conformance) {};

  // Try to perform the propagation.
  return propagateConcreteTypeOfInitExistential(AI, PD, PropagateIntoOperand);
}

/// Optimize thin_func_to_ptr->ptr_to_thin_func casts into a type substituted
/// apply.
/// This kind of code arises in generic materializeForSet code that was
/// specialized for a concrete type.
///
/// Note: this is not as general as it should be. The general solution is the
/// introduction of a partial_apply_thin_recoverable (an instruction that
/// partially applies a type and returns a thin_function) as suggested in
/// SILGenBuiltin.cpp.
///
/// %208 = thin_function_to_pointer %207 :
///  $@convention(thin) <τ_0_0> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer,
///                  @inout UnsafeMutableBufferPointer<τ_0_0>,
///                  @thick UnsafeMutableBufferPointer<τ_0_0>.Type) -> ()
///                  to $Builtin.RawPointer
/// %209 = pointer_to_thin_function %217 : $Builtin.RawPointer to
///  $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer,
///          @inout UnsafeMutableBufferPointer<Int>,
///          @thick UnsafeMutableBufferPointer<Int>.Type) -> ()
/// apply %209(%227, %200#1, %0, %224) : $@convention(thin) (Builtin.RawPointer,
///  @inout Builtin.UnsafeValueBuffer, @inout UnsafeMutableBufferPointer<Int>,
///  @thick UnsafeMutableBufferPointer<Int>.Type) -> ()
///
///  => apply %207<Int>(%227, ...)
static ApplyInst *optimizeCastThroughThinFunctionPointer(
    SILBuilder &Builder, ApplyInst *AI, FunctionRefInst *OrigThinFun,
    PointerToThinFunctionInst *CastedThinFun) {

  // The original function type needs to be polymorphic.
  auto ConvertCalleeTy = OrigThinFun->getType().castTo<SILFunctionType>();
  assert(ConvertCalleeTy);
  if (!ConvertCalleeTy->isPolymorphic())
    return nullptr;

  // Need to have four parameters.
  auto OrigParams = ConvertCalleeTy->getParameters();
  if (OrigParams.size() != 4)
    return nullptr;

  // There must only be one parameter to substitute.
  auto *ReferencedFunction = OrigThinFun->getReferencedFunction();
  assert(ReferencedFunction);
  if (ReferencedFunction->isExternalDeclaration())
    return nullptr;
  auto Params = ReferencedFunction->getContextGenericParams()->getParams();
  if (Params.size() != 1)
    return nullptr;

  // Get the concrete type from the casted to function.
  auto CastedFunTy = CastedThinFun->getType().castTo<SILFunctionType>();
  auto CastedParams = CastedFunTy->getParameters();
  if (CastedParams.size() != 4)
    return nullptr;

  // The fourth parameter is a metatype of a bound generic type. Use it to
  // obtain  the type substitutions to apply.
  auto MetaTy = dyn_cast<MetatypeType>(CastedParams[3].getType());
  if (!MetaTy)
    return nullptr;

  // Get the bound generic type from the metatype.
  auto BoundGenericInstTy = dyn_cast_or_null<BoundGenericType>(
      MetaTy->getInstanceType().getCanonicalTypeOrNull());
  if (!BoundGenericInstTy)
    return nullptr;

  // The bound generic type will carry the substitutions to apply.
  auto Subs = BoundGenericInstTy->getSubstitutions(
      AI->getModule().getSwiftModule(), nullptr);

  if (Subs.size() == 0)
    return nullptr;

  // We expect one type variable to be substituted. The substitution might have
  // recursive substitutions. Recognize and allow the case of one substitution
  // with recursive substitutions.
  // Container<T>
  //   T = ...
  //   T.A = ...
  //   T.A.C = ...
  if (Subs.size() != 1) {
    SmallPtrSet<ArchetypeType *, 16> Archetypes;
    bool SawPrimary = false;
    // Collect all the archetypes and make sure there is only one primary.
    for (unsigned i = 0, e = Subs.size(); i != e; ++i) {
      auto *AT = Subs[i].getArchetype();
      Archetypes.insert(AT);
      // Two primary arche types. We can't handle this case.
      if (SawPrimary && AT->isPrimary())
        return nullptr;
      else if (AT->isPrimary())
        SawPrimary = true;
    }

    // Make sure all the non primary archetypes have a parent archetype in the
    // set.
    for (unsigned i = 0, e = Subs.size(); i != e; ++i) {
      auto *AT = Subs[i].getArchetype();
      // Ignore the one primary archetype.
      if (AT->isPrimary())
        continue;
      if (!Archetypes.count(AT))
          return nullptr;
    }
  }

  SmallVector<SILValue, 16> Args;
  for (auto Arg : AI->getArguments())
    Args.push_back(Arg);

  auto NewSubstCalleeType =
      SILType::getPrimitiveObjectType(ConvertCalleeTy->substGenericArgs(
          AI->getModule(), AI->getModule().getSwiftModule(), Subs));

  ApplyInst *NewApply = Builder.createApply(
      AI->getLoc(), OrigThinFun, NewSubstCalleeType, AI->getType(), Subs, Args,
                                             AI->isNonThrowing());
  NewApply->setDebugScope(AI->getDebugScope());

  return NewApply;
}

/// \brief Check that all users of the apply are retain/release ignoring one
/// user.
static bool
hasOnlyRetainReleaseUsers(ApplyInst *AI, SILInstruction *IgnoreUser,
                          SmallVectorImpl<SILInstruction *> &Users) {
  for (auto *Use : getNonDebugUses(*AI)) {
    if (Use->getUser() == IgnoreUser)
      continue;

    if (!isa<RetainValueInst>(Use->getUser()) &&
        !isa<ReleaseValueInst>(Use->getUser()) &&
        !isa<StrongRetainInst>(Use->getUser()) &&
        !isa<StrongReleaseInst>(Use->getUser()))
      return false;

    Users.push_back(Use->getUser());
  }
  return true;
};

/// \brief We only know how to simulate reference call effects for unary
/// function calls that take their argument @owned or @guaranteed and return an
/// @owned value.
static bool knowHowToEmitReferenceCountInsts(ApplyInst *Call) {
  if (Call->getNumArguments() != 1)
    return false;

  FunctionRefInst *FRI = cast<FunctionRefInst>(Call->getCallee());
  SILFunction *F = FRI->getReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();

  // Look at the result type.
  auto ResultInfo = FnTy->getResult();
  if (ResultInfo.getConvention() != ResultConvention::Owned)
    return false;

  // Look at the parameter.
  auto Params = FnTy->getParameters();
  (void) Params;
  assert(Params.size() == 1 && "Expect one parameter");
  auto ParamConv = FnTy->getParameters()[0].getConvention();

  return ParamConv == ParameterConvention::Direct_Owned ||
         ParamConv == ParameterConvention::Direct_Guaranteed;
}

/// \brief Add reference counting operations equal to the effect of the call.
static void emitMatchingRCAdjustmentsForCall(ApplyInst *Call, SILValue OnX) {
  FunctionRefInst *FRI = cast<FunctionRefInst>(Call->getCallee());
  SILFunction *F = FRI->getReferencedFunction();
  auto FnTy = F->getLoweredFunctionType();
  auto ResultInfo = FnTy->getResult();
  (void) ResultInfo;

  assert(ResultInfo.getConvention() == ResultConvention::Owned &&
         "Expect a @owned return");
  assert(Call->getNumArguments() == 1 && "Expect a unary call");

  // Emit a retain for the @owned return.
  SILBuilderWithScope<> Builder(Call);
  Builder.createRetainValue(Call->getLoc(), OnX);

  // Emit a release for the @owned parameter, or none for a @guaranteed
  // parameter.
  auto Params = FnTy->getParameters();
  (void) Params;
  assert(Params.size() == 1 && "Expect one parameter");
  auto ParamInfo = FnTy->getParameters()[0].getConvention();
  assert(ParamInfo == ParameterConvention::Direct_Owned ||
         ParamInfo == ParameterConvention::Direct_Guaranteed);

  if (ParamInfo == ParameterConvention::Direct_Owned)
    Builder.createReleaseValue(Call->getLoc(), OnX);
}

static bool isCastTypeKnownToSucceed(SILType Type, SILModule &Mod) {
  auto *M = Mod.getSwiftModule();
  return M->getASTContext()
      .getBridgedToObjC(M, Type.getSwiftRValueType(), nullptr)
      .hasValue();
}

/// Replace an application of a cast composition f_inverse(f(x)) by x.
bool SILCombiner::optimizeIdentityCastComposition(ApplyInst *FInverse,
                                              StringRef FInverseName,
                                              StringRef FName) {
  // Needs to have a known semantics.
  if (!FInverse->hasSemantics(FInverseName))
    return false;

  // We need to know how to replace the call by reference counting instructions.
  if (!knowHowToEmitReferenceCountInsts(FInverse))
    return false;

  // We need to know that the cast will succeeed.
  if (!isCastTypeKnownToSucceed(FInverse->getArgument(0).getType(),
                                FInverse->getModule()) ||
      !isCastTypeKnownToSucceed(FInverse->getType(), FInverse->getModule()))
    return false;

  // Need to have a matching 'f'.
  auto *F = dyn_cast<ApplyInst>(FInverse->getArgument(0));
  if (!F)
    return false;
  if (!F->hasSemantics(FName))
    return false;
  if (!knowHowToEmitReferenceCountInsts(F))
    return false;

  // The types must match.
  if (F->getArgument(0).getType() != FInverse->getType())
    return false;

  // Retains, releases of the result of F.
  SmallVector<SILInstruction *, 16> RetainReleases;
  if (!hasOnlyRetainReleaseUsers(F, FInverse, RetainReleases))
    return false;

  // Okay, now we know we can remove the calls.
  auto X = F->getArgument(0);

  // Redirect f's result's retains/releases to affect x.
  for (auto *User : RetainReleases) {
    // X might not be strong_retain/release'able. Replace it by a
    // retain/release_value on X instead.
    if (isa<StrongRetainInst>(User)) {
      SILBuilderWithScope<>(User).createRetainValue(User->getLoc(), X);
      eraseInstFromFunction(*User);
      continue;
    }
    if (isa<StrongReleaseInst>(User)) {
      SILBuilderWithScope<>(User).createReleaseValue(User->getLoc(), X);
      eraseInstFromFunction(*User);
      continue;
    }
    User->setOperand(0, X);
  }

  // Simulate the reference count effects of the calls before removing
  // them.
  emitMatchingRCAdjustmentsForCall(F, X);
  emitMatchingRCAdjustmentsForCall(FInverse, X);

  // Replace users of f_inverse by x.
  replaceInstUsesWith(*FInverse, X.getDef());

  // Remove the calls.
  eraseInstFromFunction(*FInverse);
  eraseInstFromFunction(*F);

  return true;
}

SILInstruction *SILCombiner::visitApplyInst(ApplyInst *AI) {
  // apply{partial_apply(x,y)}(z) -> apply(z,x,y) is triggered
  // from visitPartialApplyInst(), so bail here.
  if (isa<PartialApplyInst>(AI->getCallee()))
    return nullptr;

  if (auto *CFI = dyn_cast<ConvertFunctionInst>(AI->getCallee()))
    return optimizeApplyOfConvertFunctionInst(AI, CFI);

  if (auto *CastedThinFun =
          dyn_cast<PointerToThinFunctionInst>(AI->getCallee()))
    if (auto *Ptr =
            dyn_cast<ThinFunctionToPointerInst>(CastedThinFun->getOperand()))
      if (auto *OrigThinFun = dyn_cast<FunctionRefInst>(Ptr->getOperand()))
        if (auto *NewAI = optimizeCastThroughThinFunctionPointer(
                Builder, AI, OrigThinFun, CastedThinFun)) {
          replaceInstUsesWith(*AI, NewAI, 0);
          eraseInstFromFunction(*AI);
          CallGraphEditor(CG).addEdgesForApply(NewAI);
          return nullptr;
        }

  // Optimize readonly functions with no meaningful users.
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (FRI &&
      FRI->getReferencedFunction()->getEffectsKind() < EffectsKind::ReadWrite) {
    UserListTy Users;
    if (recursivelyCollectARCUsers(Users, AI)) {
      // When deleting Apply instructions make sure to release any owned
      // arguments.
      auto FT = FRI->getFunctionType();
      for (int i = 0, e = AI->getNumArguments(); i < e; ++i) {
        SILParameterInfo PI = FT->getParameters()[i];
        auto Arg = AI->getArgument(i);
        if (PI.isConsumed() && !Arg.getType().isAddress())
          Builder.emitReleaseValueOperation(AI->getLoc(), Arg);
      }

      // Erase all of the reference counting instructions and the Apply itself.
      for (auto rit = Users.rbegin(), re = Users.rend(); rit != re; ++rit)
        eraseInstFromFunction(**rit);

      return nullptr;
    }
    // We found a user that we can't handle.
  }

  if (FRI) {
    auto *SF = FRI->getReferencedFunction();
    if (SF->getEffectsKind() < EffectsKind::ReadWrite) {
      // Try to optimize string concatenation.
      if (auto I = optimizeConcatenationOfStringLiterals(AI)) {
        return I;
      }
    }
    if (SF->hasSemanticsString("array.uninitialized")) {
      UserListTy Users;
      // If the uninitialized array is only written into then it can be removed.
      if (recursivelyCollectARCUsers(Users, AI)) {
        // Erase all of the reference counting instructions on the array and the
        // allocation-apply itself.
        for (auto rit = Users.rbegin(), re = Users.rend(); rit != re; ++rit)
          eraseInstFromFunction(**rit);
      }
    }
  }


  // (apply (thin_to_thick_function f)) to (apply f)
  if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(AI->getCallee())) {
    // TODO: Handle substitutions and indirect results
    if (AI->hasSubstitutions() || AI->hasIndirectResult())
      return nullptr;
    SmallVector<SILValue, 4> Arguments;
    for (auto &Op : AI->getArgumentOperands()) {
      Arguments.push_back(Op.get());
    }
    // The type of the substition is the source type of the thin to thick
    // instruction.
    SILType substTy = TTTFI->getOperand().getType();
    auto *NewAI = Builder.createApply(AI->getLoc(), TTTFI->getOperand(),
                                      substTy, AI->getType(),
                                      AI->getSubstitutions(), Arguments,
                                      AI->isNonThrowing());
    NewAI->setDebugScope(AI->getDebugScope());
    return NewAI;
  }

  // (apply (witness_method)) -> propagate information about
  // a concrete type from init_existential_addr or init_existential_ref.
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
    propagateConcreteTypeOfInitExistential(AI, WMI);
    return nullptr;
  }

  // (apply (function_ref method_from_protocol_extension)) ->
  // propagate information about a concrete type from init_existential_addr or
  // init_existential_ref.
  if (isa<FunctionRefInst>(AI->getCallee())) {
    if (propagateConcreteTypeOfInitExistential(AI)) {
      return nullptr;
    }
  }

  // Optimize f_inverse(f(x)) -> x.
  if (optimizeIdentityCastComposition(AI, "convertFromObjectiveC",
                                      "convertToObjectiveC"))
    return nullptr;
  if (optimizeIdentityCastComposition(AI, "convertToObjectiveC",
                                      "convertFromObjectiveC"))
    return nullptr;

  return nullptr;
}

SILInstruction *SILCombiner::visitTryApplyInst(TryApplyInst *AI) {
  // apply{partial_apply(x,y)}(z) -> apply(z,x,y) is triggered
  // from visitPartialApplyInst(), so bail here.
  if (isa<PartialApplyInst>(AI->getCallee()))
    return nullptr;

  if (auto *CFI = dyn_cast<ConvertFunctionInst>(AI->getCallee())) {
    return optimizeApplyOfConvertFunctionInst(AI, CFI);
  }

  // Optimize readonly functions with no meaningful users.
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());
  if (FRI &&
      FRI->getReferencedFunction()->getEffectsKind() < EffectsKind::ReadWrite) {
    UserListTy Users;
    if (recursivelyCollectARCUsers(Users, AI)) {
      // When deleting Apply instructions make sure to release any owned
      // arguments.
      auto FT = FRI->getFunctionType();
      for (int i = 0, e = AI->getNumArguments(); i < e; ++i) {
        SILParameterInfo PI = FT->getParameters()[i];
        auto Arg = AI->getArgument(i);
        if (PI.isConsumed() && !Arg.getType().isAddress())
          Builder.emitReleaseValueOperation(AI->getLoc(), Arg);
      }

      // Erase all of the reference counting instructions and the Apply itself.
      for (auto rit = Users.rbegin(), re = Users.rend(); rit != re; ++rit)
        eraseInstFromFunction(**rit);

      return nullptr;
    }
    // We found a user that we can't handle.
  }

  // (try_apply (thin_to_thick_function f)) to (try_apply f)
  if (auto *TTTFI = dyn_cast<ThinToThickFunctionInst>(AI->getCallee())) {
    // TODO: Handle substitutions and indirect results
    if (AI->hasSubstitutions() || AI->hasIndirectResult())
      return nullptr;
    SmallVector<SILValue, 4> Arguments;
    for (auto &Op : AI->getArgumentOperands()) {
      Arguments.push_back(Op.get());
    }
    // The type of the substitution is the source type of the thin to thick
    // instruction.
    SILType substTy = TTTFI->getOperand().getType();
    auto *NewAI = Builder.createTryApply(AI->getLoc(), TTTFI->getOperand(),
                                         substTy,
                                         AI->getSubstitutions(), Arguments,
                                         AI->getNormalBB(), AI->getErrorBB());
    NewAI->setDebugScope(AI->getDebugScope());
    return NewAI;
  }

  // (apply (witness_method)) -> propagate information about
  // a concrete type from init_existential_addr or init_existential_ref.
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
    propagateConcreteTypeOfInitExistential(AI, WMI);
    return nullptr;
  }

  // (apply (function_ref method_from_protocol_extension)) ->
  // propagate information about a concrete type from init_existential_addr or
  // init_existential_ref.
  if (isa<FunctionRefInst>(AI->getCallee())) {
    if (propagateConcreteTypeOfInitExistential(AI)) {
      return nullptr;
    }
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitCondFailInst(CondFailInst *CFI) {
  // Remove runtime asserts such as overflow checks and bounds checks.
  if (RemoveCondFails)
    return eraseInstFromFunction(*CFI);

  auto *I = dyn_cast<IntegerLiteralInst>(CFI->getOperand());
  if (!I)
    return nullptr;

  // Erase. (cond_fail 0)
  if (!I->getValue().getBoolValue())
    return eraseInstFromFunction(*CFI);

  // Remove any code that follows a (cond_fail 1) and set the block's
  // terminator to unreachable.

  // Nothing more to do here
  if (isa<UnreachableInst>(std::next(SILBasicBlock::iterator(CFI))))
    return nullptr;

  // Collect together all the instructions after this point
  llvm::SmallVector<SILInstruction *, 32> ToRemove;
  for (auto Inst = CFI->getParent()->rbegin(); &*Inst != CFI; ++Inst)
    ToRemove.push_back(&*Inst);

  for (auto *Inst : ToRemove) {
    // Replace any still-remaining uses with undef and erase.
    Inst->replaceAllUsesWithUndef();
    eraseInstFromFunction(*Inst);
  }

  // Add an `unreachable` to be the new terminator for this block
  Builder.setInsertionPoint(CFI->getParent());
  Builder.createUnreachable(ArtificialUnreachableLocation());

  return nullptr;
}

SILInstruction *SILCombiner::visitStrongRetainInst(StrongRetainInst *SRI) {
  // Retain of ThinToThickFunction is a no-op.
  if (isa<ThinToThickFunctionInst>(SRI->getOperand()))
    return eraseInstFromFunction(*SRI);

  if (isa<ObjCExistentialMetatypeToObjectInst>(SRI->getOperand()) ||
      isa<ObjCMetatypeToObjectInst>(SRI->getOperand()))
    return eraseInstFromFunction(*SRI);

  // Sometimes in the stdlib due to hand offs, we will see code like:
  //
  // strong_release %0
  // strong_retain %0
  //
  // with the matching strong_retain to the strong_release in a predecessor
  // basic block and the matching strong_release for the strong_retain in a
  // successor basic block.
  //
  // Due to the matching pairs being in different basic blocks, the ARC
  // Optimizer (which is currently local to one basic block does not handle
  // it). But that does not mean that we can not eliminate this pair with a
  // peephole.

  // If we are not the first instruction in this basic block...
  if (SRI != &*SRI->getParent()->begin()) {
    SILBasicBlock::iterator Pred = SRI;
    --Pred;

    // ...and the predecessor instruction is a strong_release on the same value
    // as our strong_retain...
    if (StrongReleaseInst *Release = dyn_cast<StrongReleaseInst>(&*Pred))
      // Remove them...
      if (Release->getOperand() == SRI->getOperand()) {
        eraseInstFromFunction(*Release);
        return eraseInstFromFunction(*SRI);
      }
  }

  return nullptr;
}

/// Simplify the following two frontend patterns:
///
///   %payload_addr = init_enum_data_addr %payload_allocation
///   store %payload to %payload_addr
///   inject_enum_addr %payload_allocation, $EnumType.case
///
///   inject_enum_add %nopayload_allocation, $EnumType.case
///
/// for a concrete enum type $EnumType.case to:
///
///   %1 = enum $EnumType, $EnumType.case, %payload
///   store %1 to %payload_addr
///
///   %1 = enum $EnumType, $EnumType.case
///   store %1 to %nopayload_addr
///
/// We leave the cleaning up to mem2reg.
SILInstruction *
SILCombiner::visitInjectEnumAddrInst(InjectEnumAddrInst *IEAI) {
  // Given an inject_enum_addr of a concrete type without payload, promote it to
  // a store of an enum. Mem2reg/load forwarding will clean things up for us. We
  // can't handle the payload case here due to the flow problems caused by the
  // dependency in between the enum and its data.

  assert(IEAI->getOperand().getType().isAddress() && "Must be an address");

  if (IEAI->getOperand().getType().isAddressOnly(IEAI->getModule())) {
    // Check for the following pattern inside the current basic block:
    // inject_enum_addr %payload_allocation, $EnumType.case1
    // ... no insns storing anything into %payload_allocation
    // select_enum_addr  %payload_allocation,
    //                   case $EnumType.case1: %Result1,
    //                   case case $EnumType.case2: %bResult2
    //                   ...
    //
    // Replace the select_enum_addr by %Result1

    auto *Term = IEAI->getParent()->getTerminator();
    if (isa<CondBranchInst>(Term) || isa<SwitchValueInst>(Term)) {
      auto BeforeTerm = prev(prev(IEAI->getParent()->end()));
      auto *SEAI = dyn_cast<SelectEnumAddrInst>(BeforeTerm);
      if (!SEAI)
        return nullptr;

      if (SEAI->getOperand() != IEAI->getOperand())
        return nullptr;

      SILBasicBlock::iterator II = IEAI;
      StoreInst *SI = nullptr;
      for (;;) {
        SILInstruction *CI = II;
        if (CI == SEAI)
          break;
        ++II;
        SI = dyn_cast<StoreInst>(CI);
        if (SI) {
          if (SI->getDest() == IEAI->getOperand())
            return nullptr;
        }
        // Allow all instructions inbetween, which don't have any dependency to
        // the store.
        if (AA->mayWriteToMemory(II, IEAI->getOperand()))
          return nullptr;
      }

      auto *InjectedEnumElement = IEAI->getElement();
      auto Result = SEAI->getCaseResult(InjectedEnumElement);

      // Replace select_enum_addr by the result
      replaceInstUsesWith(*SEAI, Result.getDef());
      return nullptr;
    }

    // Check for the following pattern inside the current basic block:
    // inject_enum_addr %payload_allocation, $EnumType.case1
    // ... no insns storing anything into %payload_allocation
    // switch_enum_addr  %payload_allocation,
    //                   case $EnumType.case1: %bbX,
    //                   case case $EnumType.case2: %bbY
    //                   ...
    //
    // Replace the switch_enum_addr by select_enum_addr, switch_value.
    if (auto *SEI = dyn_cast<SwitchEnumAddrInst>(Term)) {
      if (SEI->getOperand() != IEAI->getOperand())
        return nullptr;

      SILBasicBlock::iterator II = IEAI;
      StoreInst *SI = nullptr;
      for (;;) {
        SILInstruction *CI = II;
        if (CI == SEI)
          break;
        ++II;
        SI = dyn_cast<StoreInst>(CI);
        if (SI) {
          if (SI->getDest() == IEAI->getOperand())
            return nullptr;
        }
        // Allow all instructions inbetween, which don't have any dependency to
        // the store.
        if (AA->mayWriteToMemory(II, IEAI->getOperand()))
          return nullptr;
      }

      // Replace switch_enum_addr by a branch instruction.
      SILBuilderWithScope<1> B(SEI);
      SmallVector<std::pair<EnumElementDecl *, SILValue>, 8> CaseValues;
      SmallVector<std::pair<SILValue, SILBasicBlock *>, 8> CaseBBs;

      auto IntTy = SILType::getBuiltinIntegerType(32, B.getASTContext());

      for (int i = 0, e = SEI->getNumCases(); i < e; ++i) {
        auto Pair = SEI->getCase(i);
        auto *IL = B.createIntegerLiteral(SEI->getLoc(), IntTy, APInt(32, i, false));
        SILValue ILValue = SILValue(IL);
        CaseValues.push_back(std::make_pair(Pair.first, ILValue));
        CaseBBs.push_back(std::make_pair(ILValue, Pair.second));
      }

      SILValue DefaultValue;
      SILBasicBlock *DefaultBB = nullptr;

      if (SEI->hasDefault()) {
        auto *IL = B.createIntegerLiteral(SEI->getLoc(), IntTy, APInt(32, SEI->getNumCases(), false));
        DefaultValue = SILValue(IL);
        DefaultBB = SEI->getDefaultBB();
      }

      auto *SEAI = B.createSelectEnumAddr(SEI->getLoc(), SEI->getOperand(), IntTy, DefaultValue, CaseValues);

      B.createSwitchValue(SEI->getLoc(), SILValue(SEAI), DefaultBB, CaseBBs);

      return eraseInstFromFunction(*SEI);
    }

    return nullptr;
  }

  // If the enum does not have a payload create the enum/store since we don't
  // need to worry about payloads.
  if (!IEAI->getElement()->hasArgumentType()) {
    EnumInst *E =
      Builder.createEnum(IEAI->getLoc(), SILValue(), IEAI->getElement(),
                          IEAI->getOperand().getType().getObjectType());
    E->setDebugScope(IEAI->getDebugScope());
    Builder.createStore(IEAI->getLoc(), E, IEAI->getOperand())
      ->setDebugScope(IEAI->getDebugScope());
    return eraseInstFromFunction(*IEAI);
  }

  // Ok, we have a payload enum, make sure that we have a store previous to
  // us...
  SILBasicBlock::iterator II = IEAI;
  StoreInst *SI = nullptr;
  InitEnumDataAddrInst *DataAddrInst = nullptr;
  for (;;) {
    if (II == IEAI->getParent()->begin())
      return nullptr;
    --II;
    SI = dyn_cast<StoreInst>(&*II);
    if (SI) {
      // Find a Store whose destination is taken from an init_enum_data_addr
      // whose address is same allocation as our inject_enum_addr.
      DataAddrInst = dyn_cast<InitEnumDataAddrInst>(SI->getDest().getDef());
      if (DataAddrInst && DataAddrInst->getOperand() == IEAI->getOperand())
        break;
    }
    // Allow all instructions inbetween, which don't have any dependency to
    // the store.
    if (AA->mayWriteToMemory(II, IEAI->getOperand()))
      return nullptr;
  }
  // Found the store to this enum payload. Check if the store is the only use.
  if (!DataAddrInst->hasOneUse())
    return nullptr;

  // In that case, create the payload enum/store.
  EnumInst *E =
      Builder.createEnum(DataAddrInst->getLoc(), SI->getSrc(),
                          DataAddrInst->getElement(),
                          DataAddrInst->getOperand().getType().getObjectType());
  E->setDebugScope(DataAddrInst->getDebugScope());
  Builder.createStore(DataAddrInst->getLoc(), E, DataAddrInst->getOperand())
    ->setDebugScope(DataAddrInst->getDebugScope());
  // Cleanup.
  eraseInstFromFunction(*SI);
  eraseInstFromFunction(*DataAddrInst);
  return eraseInstFromFunction(*IEAI);
}

SILInstruction *
SILCombiner::
visitUnreachableInst(UnreachableInst *UI) {
  // Make sure that this unreachable instruction
  // is the last instruction in the basic block.
  if (UI->getParent()->getTerminator() == UI)
    return nullptr;

  // Collect together all the instructions after this point
  llvm::SmallVector<SILInstruction *, 32> ToRemove;
  for (auto Inst = UI->getParent()->rbegin(); &*Inst != UI; ++Inst)
    ToRemove.push_back(&*Inst);

  for (auto *Inst : ToRemove) {
    // Replace any still-remaining uses with undef values and erase.
    Inst->replaceAllUsesWithUndef();
    eraseInstFromFunction(*Inst);
  }

  return nullptr;
}

/// We really want to eliminate unchecked_take_enum_data_addr. Thus if we find
/// one go through all of its uses and see if they are all loads and address
/// projections (in many common situations this is true). If so, perform:
///
/// (load (unchecked_take_enum_data_addr x)) -> (unchecked_enum_data (load x))
///
/// FIXME: Implement this for address projections.
SILInstruction *
SILCombiner::
visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *TEDAI) {
  // If our TEDAI has no users, there is nothing to do.
  if (TEDAI->use_empty())
    return nullptr;

  // If our enum type is address only, we can not do anything here. The key
  // thing to remember is that an enum is address only if any of its cases are
  // address only. So we *could* have a loadable payload resulting from the
  // TEDAI without the TEDAI being loadable itself.
  if (TEDAI->getOperand().getType().isAddressOnly(TEDAI->getModule()))
    return nullptr;

  // For each user U of the take_enum_data_addr...
  for (auto U : getNonDebugUses(*TEDAI))
    // Check if it is load. If it is not a load, bail...
    if (!isa<LoadInst>(U->getUser()))
      return nullptr;

  // Grab the EnumAddr.
  SILLocation Loc = TEDAI->getLoc();
  SILDebugScope *Scope = TEDAI->getDebugScope();
  SILValue EnumAddr = TEDAI->getOperand();
  EnumElementDecl *EnumElt = TEDAI->getElement();
  SILType PayloadType = TEDAI->getType().getObjectType();

  // Go back through a second time now that we know all of our users are
  // loads. Perform the transformation on each load.
  for (auto U : getNonDebugUses(*TEDAI)) {
    // Grab the load.
    LoadInst *L = cast<LoadInst>(U->getUser());

    // Insert a new Load of the enum and extract the data from that.
    auto *Load = Builder.createLoad(Loc, EnumAddr);
    Load->setDebugScope(Scope);
    auto *D = Builder.createUncheckedEnumData(
        Loc, Load, EnumElt, PayloadType);
    D->setDebugScope(Scope);

    // Replace all uses of the old load with the data and erase the old load.
    replaceInstUsesWith(*L, D, 0);
    eraseInstFromFunction(*L);
  }

  return eraseInstFromFunction(*TEDAI);
}

SILInstruction *SILCombiner::visitStrongReleaseInst(StrongReleaseInst *SRI) {
  // Release of ThinToThickFunction is a no-op.
  if (isa<ThinToThickFunctionInst>(SRI->getOperand()))
    return eraseInstFromFunction(*SRI);

  if (isa<ObjCExistentialMetatypeToObjectInst>(SRI->getOperand()) ||
      isa<ObjCMetatypeToObjectInst>(SRI->getOperand()))
    return eraseInstFromFunction(*SRI);

  return nullptr;
}

SILInstruction *SILCombiner::visitCondBranchInst(CondBranchInst *CBI) {
  // cond_br(xor(x, 1)), t_label, f_label -> cond_br x, f_label, t_label
  SILValue X;
  if (match(CBI->getCondition(), m_ApplyInst(BuiltinValueKind::Xor,
                                             m_SILValue(X), m_One()))) {
    SmallVector<SILValue, 4> OrigTrueArgs, OrigFalseArgs;
    for (const auto &Op : CBI->getTrueArgs())
      OrigTrueArgs.push_back(Op);
    for (const auto &Op : CBI->getFalseArgs())
      OrigFalseArgs.push_back(Op);
    return Builder.createCondBranch(CBI->getLoc(), X,
                                    CBI->getFalseBB(), OrigFalseArgs,
                                    CBI->getTrueBB(), OrigTrueArgs);
  }

  // cond_br (select_enum) -> switch_enum
  // This pattern often occurs as a result of using optionals.
  if (auto *SEI = dyn_cast<SelectEnumInst>(CBI->getCondition())) {
    // No bb args should be passed
    if (!CBI->getTrueArgs().empty() || !CBI->getFalseArgs().empty())
      return nullptr;
    auto EnumOperandTy = SEI->getEnumOperand().getType();
    // Type should be loadable
    if (!EnumOperandTy.isLoadable(SEI->getModule()))
      return nullptr;

    // Result of the selec_enum should be a boolean.
    if (SEI->getType() != CBI->getCondition().getType())
      return nullptr;

    // If any of cond_br edges are critical edges, do not perform
    // the transformation, as SIL in canonical form may
    // only have critical edges that are originating from cond_br
    // instructions.
    if (!CBI->getTrueBB()->getSinglePredecessor())
      return nullptr;

    if (!CBI->getFalseBB()->getSinglePredecessor())
      return nullptr;

    SILBasicBlock *Default = nullptr;

    match_integer<0> Zero;

    if (SEI->hasDefault()) {
      bool isFalse = match(SEI->getDefaultResult(), Zero);
      Default = isFalse ? CBI->getFalseBB() : Default = CBI->getTrueBB();
    }

    // We can now convert cond_br(select_enum) into switch_enum
    SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 8> Cases;
    for (int i = 0, e = SEI->getNumCases(); i < e; ++i) {
      auto Pair = SEI->getCase(i);
      if (isa<IntegerLiteralInst>(Pair.second)) {
        bool isFalse = match(Pair.second, Zero);
        if (!isFalse && Default != CBI->getTrueBB()) {
          Cases.push_back(std::make_pair(Pair.first, CBI->getTrueBB()));
        }
        if (isFalse && Default != CBI->getFalseBB()) {
          Cases.push_back(std::make_pair(Pair.first, CBI->getFalseBB()));
        }
        continue;
      }

      return nullptr;
    }

    return Builder.createSwitchEnum(SEI->getLoc(), SEI->getEnumOperand(),
                                    Default, Cases);
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitSelectEnumInst(SelectEnumInst *SEI) {
  // Canonicalize a select_enum: if the default refers to exactly one case, then
  // replace the default with that case.
  if (SEI->hasDefault()) {
    NullablePtr<EnumElementDecl> elementDecl = SEI->getUniqueCaseForDefault();
    if (elementDecl.isNonNull()) {
      // Construct a new instruction by copying all the case entries.
      SmallVector<std::pair<EnumElementDecl *, SILValue>, 4> CaseValues;
      for (int idx = 0, numIdcs = SEI->getNumCases(); idx < numIdcs; idx++) {
        CaseValues.push_back(SEI->getCase(idx));
      }
      // Add the default-entry of the original instruction as case-entry.
      CaseValues.push_back(
          std::make_pair(elementDecl.get(), SEI->getDefaultResult()));

      return Builder.createSelectEnum(SEI->getLoc(), SEI->getEnumOperand(),
                                      SEI->getType(), SILValue(), CaseValues);
    }
  }

  // TODO: We should be able to flat-out replace the select_enum instruction
  // with the selected value in another pass. For parity with the enum_is_tag
  // combiner pass, handle integer literals for now.
  auto *EI = dyn_cast<EnumInst>(SEI->getEnumOperand());
  if (!EI)
    return nullptr;

  SILValue selected;
  for (unsigned i = 0, e = SEI->getNumCases(); i < e; ++i) {
    auto casePair = SEI->getCase(i);
    if (casePair.first == EI->getElement()) {
      selected = casePair.second;
      break;
    }
  }
  if (!selected)
    selected = SEI->getDefaultResult();

  if (auto *ILI = dyn_cast<IntegerLiteralInst>(selected)) {
    return Builder.createIntegerLiteral(ILI->getLoc(), ILI->getType(),
                                        ILI->getValue());
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitTupleExtractInst(TupleExtractInst *TEI) {
  // tuple_extract(apply([add|sub|...]overflow(x, 0)), 1) -> 0
  // if it can be proven that no overflow can happen.
  if (TEI->getFieldNo() != 1)
    return nullptr;

  if (auto *BI = dyn_cast<BuiltinInst>(TEI->getOperand()))
    if (!canOverflow(BI))
      return Builder.createIntegerLiteral(TEI->getLoc(), TEI->getType(),
                                          APInt(1, 0));
  return nullptr;
}

SILInstruction *SILCombiner::visitFixLifetimeInst(FixLifetimeInst *FLI) {
  // fix_lifetime(alloc_stack) -> fix_lifetime(load(alloc_stack))
  if (auto *AI = dyn_cast<AllocStackInst>(FLI->getOperand())) {
    if (FLI->getOperand().getType().isLoadable(FLI->getModule())) {
      auto Load = Builder.createLoad(FLI->getLoc(), SILValue(AI, 1));
      Load->setDebugScope(FLI->getDebugScope());
      return Builder.createFixLifetime(FLI->getLoc(), SILValue(Load, 0));
    }
  }
  return nullptr;
}

SILInstruction *
SILCombiner::
visitAllocRefDynamicInst(AllocRefDynamicInst *ARDI) {
  // %1 = metatype $X.Type
  // %2 = alloc_ref_dynamic %1 : $X.Type, Y
  // ->
  // alloc_ref X
  if (MetatypeInst *MI = dyn_cast<MetatypeInst>(ARDI->getOperand())) {
    auto &Mod = ARDI->getModule();
    auto SILInstanceTy = MI->getType().getMetatypeInstanceType(Mod);
    auto *ARI = Builder.createAllocRef(ARDI->getLoc(), SILInstanceTy,
                                       ARDI->isObjC());
    ARI->setDebugScope(ARDI->getDebugScope());
    return ARI;
  }

  // checked_cast_br [exact] $Y.Type to $X.Type, bbSuccess, bbFailure
  // ...
  // bbSuccess(%T: $X.Type)
  // alloc_ref_dynamic %T : $X.Type, $X
  // ->
  // alloc_ref $X
  if (isa<SILArgument>(ARDI->getOperand())) {
    auto *PredBB = ARDI->getParent()->getSinglePredecessor();
    if (!PredBB)
      return nullptr;
    auto *CCBI = dyn_cast<CheckedCastBranchInst>(PredBB->getTerminator());
    if (CCBI && CCBI->isExact() && ARDI->getParent() == CCBI->getSuccessBB()) {
      auto &Mod = ARDI->getModule();
      auto SILInstanceTy = CCBI->getCastType().getMetatypeInstanceType(Mod);
      auto *ARI = Builder.createAllocRef(ARDI->getLoc(), SILInstanceTy,
                                         ARDI->isObjC());
      ARI->setDebugScope(ARDI->getDebugScope());
      return ARI;
    }
  }
  return nullptr;
}

SILInstruction *SILCombiner::visitEnumInst(EnumInst *EI) {
  return nullptr;
}
