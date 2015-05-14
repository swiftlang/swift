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

  return new (SEI->getModule()) UncheckedRefBitCastInst(SEI->getLoc(),
                                                        URBCI->getOperand(),
                                                        SEI->getType());
}

static bool isFirstPayloadedCase(EnumDecl *E, EnumElementDecl *Elt) {
  for (EnumElementDecl *Iter : E->getAllElements())
    if (Iter->hasArgumentType())
      return Iter == Elt;
  return false;
}

SILInstruction *
SILCombiner::
visitUncheckedEnumDataInst(UncheckedEnumDataInst *UEDI) {
  // First to be safe, do not perform this optimization on unchecked_enum_data
  // on bounded generic nominal types.
  SILValue Op = UEDI->getOperand();
  SILType OpType = Op.getType();
  if (OpType.hasArchetype() || OpType.isTrivial(UEDI->getModule()))
    return nullptr;

  // (unchecked_enum_data (unchecked_ref_bit_cast X->Y x) #z)
  //    ->
  // (unchecked_ref_bit_cast X->Z x)
  //
  // Where #z is the payload of type Z of the first payloaded case of the enum
  // Y.
  auto *URBCI = dyn_cast<UncheckedRefBitCastInst>(Op);
  if (!URBCI)
    return nullptr;

  // A UEDI performs a layout compatible operation if it is extracting the first
  // argument case of the enum.
  EnumDecl *E = OpType.getEnumOrBoundGenericEnum();
  if (!isFirstPayloadedCase(E, UEDI->getElement()))
    return nullptr;

  return new (UEDI->getModule()) UncheckedRefBitCastInst(UEDI->getLoc(),
                                                         URBCI->getOperand(),
                                                         UEDI->getType());
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
  LoadInst *EnumVal = Builder->createLoad(SEAI->getLoc(), SEAI->getOperand());
  EnumVal->setDebugScope(SEAI->getDebugScope());
  Builder->createSwitchEnum(SEAI->getLoc(), EnumVal, Default, Cases)
    ->setDebugScope(SEAI->getDebugScope());
  return eraseInstFromFunction(*SEAI);
}

SILInstruction *SILCombiner::visitSelectEnumAddrInst(SelectEnumAddrInst *SEAI) {
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
  LoadInst *EnumVal = Builder->createLoad(SEAI->getLoc(),
                                          SEAI->getEnumOperand());
  EnumVal->setDebugScope(SEAI->getDebugScope());
  auto *I = SelectEnumInst::create(SEAI->getLoc(), EnumVal, SEAI->getType(),
                                   Default, Cases,
                                   *SEAI->getFunction());
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
  bool HaveSeenCopyInto = false;
  // Scan all of the uses of the AllocStack and check if it is not used for
  // anything other than the init_existential_addr container.
  for (Operand *Op: AS->getUses()) {
    // Destroy and dealloc are both fine.
    if (isa<DestroyAddrInst>(Op->getUser()) ||
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
  auto OrigInsertionPoint = Builder->getInsertionPoint();

  // If the only users of the alloc_stack are alloc, destroy and
  // init_existential_addr then we can promote the allocation of the init
  // existential.
  if (LegalUsers && IEI) {
    auto *ConcAlloc = Builder->createAllocStack(AS->getLoc(),
                                                IEI->getLoweredConcreteType());
    ConcAlloc->setDebugScope(AS->getDebugScope());
    SILValue(IEI, 0).replaceAllUsesWith(ConcAlloc->getAddressResult());
    eraseInstFromFunction(*IEI);


    for (Operand *Op: AS->getUses()) {
      if (auto *DA = dyn_cast<DestroyAddrInst>(Op->getUser())) {
        Builder->setInsertionPoint(DA);
        Builder->createDestroyAddr(DA->getLoc(), SILValue(ConcAlloc, 1))
          ->setDebugScope(DA->getDebugScope());
        eraseInstFromFunction(*DA);

      }
      if (auto *DS = dyn_cast<DeallocStackInst>(Op->getUser())) {
        Builder->setInsertionPoint(DS);
        Builder->createDeallocStack(DS->getLoc(), SILValue(ConcAlloc, 0))
          ->setDebugScope(DS->getDebugScope());
        eraseInstFromFunction(*DS);
      }
    }

    eraseInstFromFunction(*AS);
    // Restore the insertion point.
    Builder->setInsertionPoint(OrigInsertionPoint);
  }

  // Remove a dead live range that is only copied into.
  if (LegalUsers && HaveSeenCopyInto) {
    SmallPtrSet<SILInstruction *, 16> ToDelete;

    for (auto *Op : AS->getUses()) {
      // Replace a copy_addr [take] %src ... by a destroy_addr %src if %src is
      // no the alloc_stack.
      // Otherwise, just delete the copy_addr.
      if (auto *CopyAddr = dyn_cast<CopyAddrInst>(Op->getUser())) {
        if (CopyAddr->isTakeOfSrc() && CopyAddr->getSrc().getDef() != AS) {
          Builder->setInsertionPoint(CopyAddr);
          Builder->createDestroyAddr(CopyAddr->getLoc(), CopyAddr->getSrc())
              ->setDebugScope(CopyAddr->getDebugScope());
        }
      }
      assert(isa<CopyAddrInst>(Op->getUser()) ||
             isa<DestroyAddrInst>(Op->getUser()) ||
             isa<DeallocStackInst>(Op->getUser()) && "Unexpected instruction");
      ToDelete.insert(Op->getUser());
    }

    // Erase the 'live-range'
    for (auto *Inst : ToDelete)
      eraseInstFromFunction(*Inst);
    eraseInstFromFunction(*AS);

    // Restore the insertion point.
    Builder->setInsertionPoint(OrigInsertionPoint);
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitLoadInst(LoadInst *LI) {
  // (load (upcast-ptr %x)) -> (upcast-ref (load %x))
  if (auto *UI = dyn_cast<UpcastInst>(LI->getOperand())) {
    auto NewLI = Builder->createLoad(LI->getLoc(), UI->getOperand());
    NewLI->setDebugScope(LI->getDebugScope());
    return new (UI->getModule()) UpcastInst(LI->getLoc(), NewLI,
                                            LI->getType());
  }

  // Given a load with multiple struct_extracts/tuple_extracts and no other
  // uses, canonicalize the load into several (struct_element_addr (load))
  // pairs.
  using ProjInstPairTy = std::pair<Projection, SILInstruction *>;

  // Go through the loads uses and add any users that are projections to the
  // projection list.
  llvm::SmallVector<ProjInstPairTy, 8> Projections;
  for (auto *UI : LI->getUses()) {
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
    auto I = Proj.createAddrProjection(*Builder, LI->getLoc(), LI->getOperand());
    LastProj = &Proj;
    I.get()->setDebugScope(LI->getDebugScope());
    LastNewLoad = Builder->createLoad(LI->getLoc(), I.get());
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
      return new (RVI->getModule()) ReleaseValueInst(RVI->getLoc(),
                                                     EI->getOperand());
    }
  }

  // ReleaseValueInst of a reference type is a strong_release.
  if (OperandTy.isReferenceCounted(RVI->getModule()))
    return new (RVI->getModule()) StrongReleaseInst(RVI->getLoc(), Operand);

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
      return new (RVI->getModule()) RetainValueInst(RVI->getLoc(),
                                                    EI->getOperand());
    }
  }

  // RetainValueInst of a reference type is a strong_release.
  if (OperandTy.isReferenceCounted(RVI->getModule())) {
    return new (RVI->getModule()) StrongRetainInst(RVI->getLoc(), Operand);
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

SILInstruction *SILCombiner::visitPartialApplyInst(PartialApplyInst *PAI) {
  // partial_apply without any substitutions or arguments is just a
  // thin_to_thick_function.
  if (!PAI->hasSubstitutions() && (PAI->getNumArguments() == 0))
    return new (PAI->getModule()) ThinToThickFunctionInst(PAI->getLoc(),
                                                          PAI->getCallee(),
                                                          PAI->getType());

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

static bool canCombineApplyOfPartialApply(const ApplyInst *AI, const PartialApplyInst *PAI) {
  // Don't handle generic applys.
  if (AI->hasSubstitutions())
    return false;

  // Make sure that the substitution list of the PAI does not contain any
  // archetypes.
  ArrayRef<Substitution> Subs = PAI->getSubstitutions();
  for (Substitution S : Subs)
    if (S.getReplacement()->getCanonicalType()->hasArchetype())
      return false;

  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
  if (!FRI)
    return false;
  return true;
}

static bool useDoesNotKeepClosureAlive(const SILInstruction *I) {
  switch (I->getKind()) {
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongReleaseInst:
  case ValueKind::RetainValueInst:
  case ValueKind::ReleaseValueInst:
  case ValueKind::DebugValueInst:
    return true;
  /*
  case ValueKind::ApplyInst: {
    if (auto *AI = dyn_cast<ApplyInst>(I)) {
      if (auto *PAI = dyn_cast<PartialApplyInst>(AI->getCallee())) {
        return canCombineApplyOfPartialApply(AI, PAI);
      }
    }
    return false;
  }
  */
  default:
    return false;
  }
}

/// Iterate over all uses of a given partial_apply and check
/// if any of those uses are apply instructions. Try to
/// combine those applies with this partial_apply.
SILInstruction *
SILCombiner::tryOptimizeApplyOfPartialApply(PartialApplyInst *PAI) {
  ArrayRef<Substitution> Subs = PAI->getSubstitutions();
  // Temporaries created as copies of alloc_stack arguments of
  // the partial_apply.
  SmallVector<SILValue, 8> AllocStackArgs;

  // Mapping from the original argument of partial_apply to
  // the temporary containing its copy.
  llvm::DenseMap<SILValue, SILValue> ArgToTmp;

  // Set of lifetime endpoints for this partial_apply.
  SmallPtrSet<SILInstruction *, 8> EndPoints;
  LifetimeTracker lifetimeTracker(PAI, [](const SILInstruction *I) -> bool {
    return !useDoesNotKeepClosureAlive(I);
  });

  bool isFirstTime = true;

  for (auto Use : PAI->getUses()) {
    auto User = Use->getUser();
    auto AI = dyn_cast<ApplyInst>(User);
    if (!AI)
      continue;

    if (AI->getCallee() != PAI)
      continue;

    // Skip any applies that cannot be processed.
    if (!canCombineApplyOfPartialApply(AI, PAI))
      continue;

    FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());

    Builder->setInsertionPoint(AI);

    // Prepare the args.
    SmallVector<SILValue, 8> Args;
    // First the ApplyInst args.
    for (auto Op : AI->getArguments())
      Args.push_back(Op);

    SILInstruction *InsertionPoint = Builder->getInsertionPoint();
    // Next, the partial apply args.

    // Pre-process partial_apply arguments once.
    if (isFirstTime) {
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
      isFirstTime = false;
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
          Builder->setInsertionPoint(PAI->getFunction()->begin()->begin());
          // Create a new temporary at the beginning of a function.
          auto *Tmp = Builder->createAllocStack(PAI->getLoc(), Arg.getType());
          Builder->setInsertionPoint(PAI);
          // Copy argument into this temporary.
          Builder->createCopyAddr(PAI->getLoc(), Arg, SILValue(Tmp, 1),
                                  IsTake_t::IsNotTake,
                                  IsInitialization_t::IsInitialization);

          AllocStackArgs.push_back(SILValue(Tmp, 0));
          if (!Arg.getType().isTrivial(PAI->getModule()))
            needsReleases = true;
          ArgToTmp.insert(std::make_pair(Arg, SILValue(Tmp, 0)));
        }
      }

      if (needsReleases) {
        // Compute the set of endpoints, which will be used
        // to insert releases of temporaries.
        lifetimeTracker.getEndpoints();
        for (auto *EndPoint : lifetimeTracker.getEndpoints()) {
          EndPoints.insert(EndPoint);
        }
      }
    }

    for (auto Op : PAI->getArguments()) {
      auto Arg = Op;
      if (isa<AllocStackInst>(Arg)) {
        if (ArgToTmp.count(Arg)) {
          // Use the new temporary as the argument of the new apply instruction.
          // auto Tmp = TrivialArgs[idx++];
          auto Tmp = ArgToTmp.lookup(Arg);
          Op = SILValue(Tmp.getDef(), 1);
        }
      }
      Args.push_back(Op);
    }

    Builder->setInsertionPoint(InsertionPoint);

    // The thunk that implements the partial apply calls the closure function
    // that expects all arguments to be consumed by the function. However, the
    // captured arguments are not arguments of *this* apply, so they are not
    // pre-incremented. When we combine the partial_apply and this apply into
    // a new apply we need to retain all of the closure non-address type
    // arguments.
    for (auto Arg : PAI->getArguments())
      if (!Arg.getType().isAddress())
        Builder->emitRetainValueOperation(PAI->getLoc(), Arg);

    SILFunction *F = FRI->getReferencedFunction();
    SILType FnType = F->getLoweredType();
    SILType ResultTy = F->getLoweredFunctionType()->getSILResult();
    if (!Subs.empty()) {
      FnType = FnType.substGenericArgs(PAI->getModule(), Subs);
      ResultTy = FnType.getAs<SILFunctionType>()->getSILResult();
    }

    ApplyInst *NAI =
        Builder->createApply(AI->getLoc(), FRI, FnType, ResultTy, Subs, Args);
    NAI->setDebugScope(AI->getDebugScope());

    if (CG)
      CG->addEdgesForApply(NAI);

    // We also need to release the partial_apply instruction itself because it
    // is consumed by the apply_instruction.
    Builder->createStrongRelease(AI->getLoc(), PAI)
        ->setDebugScope(AI->getDebugScope());

    if (EndPoints.count(AI)) {
      EndPoints.erase(AI);
      EndPoints.insert(NAI);
    }

    replaceInstUsesWith(*AI, NAI);
    eraseInstFromFunction(*AI);
  }

  if (!AllocStackArgs.empty()) {
    // Insert releases and destroy_addrs as early as possible,
    // because we don't want to keep objects alive longer than
    // its really needed.
    for (auto Op : AllocStackArgs) {
      auto TmpType = Op.getType().getObjectType();
      if (TmpType.isTrivial(PAI->getModule()))
        continue;
      for (auto *EndPoint : EndPoints) {
        Builder->setInsertionPoint(next(SILBasicBlock::iterator(EndPoint)));
        auto TmpAddr = SILValue(Op.getDef(), 1);
        if (!TmpType.isAddressOnly(PAI->getModule())) {
          auto *Load = Builder->createLoad(PAI->getLoc(), TmpAddr);
          Builder->createReleaseValue(PAI->getLoc(), Load);
        } else {
          Builder->createDestroyAddr(PAI->getLoc(), TmpAddr);
        }
      }
    }

    // Insert dealloc_stack instructions.
    TinyPtrVector<SILBasicBlock *> ExitBBs;
    findAllNonFailureExitBBs(PAI->getFunction(), ExitBBs);

    for (auto Op : AllocStackArgs) {
      for (auto *ExitBB : ExitBBs) {
        auto *Term = ExitBB->getTerminator();
        Builder->setInsertionPoint(Term);
        Builder->createDeallocStack(PAI->getLoc(), Op);
      }
    }
  }

  return nullptr;
}

SILInstruction *SILCombiner::optimizeBuiltinCanBeObjCClass(BuiltinInst *BI) {
  assert(BI->hasSubstitutions() && "Expected substitutions for canBeClass");

  auto const &Subs = BI->getSubstitutions();
  assert((Subs.size() == 1) &&
         "Expected one substitution in call to canBeClass");

  auto Ty = Subs[0].getReplacement()->getCanonicalType();
  switch (Ty->canBeClass()) {
  case TypeTraitResult::IsNot:
    return IntegerLiteralInst::create(BI->getLoc(), BI->getType(),
                                      APInt(8, 0), *BI->getFunction());
  case TypeTraitResult::Is:
    return IntegerLiteralInst::create(BI->getLoc(), BI->getType(),
                                      APInt(8, 1), *BI->getFunction());
  case TypeTraitResult::CanBe:
    return nullptr;
  }
}

SILInstruction *SILCombiner::optimizeBuiltinCompareEq(BuiltinInst *BI,
                                                      bool NegateResult) {
  // Canonicalize boolean comparisions.
  if (auto OpTy = BI->getArguments()[0].getType().getAs<BuiltinIntegerType>())
    if (OpTy->isFixedWidth(1))
      // cmp_eq %X, -1 -> xor (cmp_eq %X, 0), -1
      if (!NegateResult) {
        if (auto *ILOp = dyn_cast<IntegerLiteralInst>(BI->getArguments()[1]))
          if (ILOp->getValue().isAllOnesValue()) {
            auto X = BI->getArguments()[0];
            SILValue One(ILOp);
            SILValue Zero(
                Builder->createIntegerLiteral(BI->getLoc(), BI->getType(), 0));
            SILValue Inverted(Builder->createBuiltin(
                BI->getLoc(), BI->getName(), BI->getType(), {}, {X, Zero}));
            auto *Xor = Builder->createBuiltinBinaryFunction(
                BI->getLoc(), "xor", BI->getType(), BI->getType(),
                {Inverted, One});
            replaceInstUsesWith(*BI, Xor);
            return eraseInstFromFunction(*BI);
          }
      }
  IsZeroKind LHS = isZeroValue(BI->getArguments()[0]);
  IsZeroKind RHS = isZeroValue(BI->getArguments()[1]);

  // Can't handle unknown values.
  if (LHS == IsZeroKind::Unknown || RHS == IsZeroKind::Unknown)
    return nullptr;

  // Can't handle non-zero ptr values.
  if (LHS == IsZeroKind::NotZero && RHS == IsZeroKind::NotZero)
    return nullptr;

  // Set to true if both sides are zero. Set to false if only one side is zero.
  bool Val = (LHS == RHS) ^ NegateResult;

  return IntegerLiteralInst::create(BI->getLoc(), BI->getType(), APInt(1, Val),
                                    *BI->getFunction());
}

SILInstruction *
SILCombiner::optimizeApplyOfConvertFunctionInst(ApplyInst *AI,
                                                ConvertFunctionInst *CFI) {
  // We only handle simplification of static function references. If we don't
  // have one, bail.
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CFI->getOperand());
  if (!FRI)
    return nullptr;

  // Grab our relevant callee types...
  CanSILFunctionType SubstCalleeTy = AI->getSubstCalleeType();
  auto ConvertCalleeTy =
      CFI->getOperand().getType().castTo<SILFunctionType>();

  // ... and make sure they have no unsubstituted generics. If they do, bail.
  if (SubstCalleeTy->hasArchetype() || ConvertCalleeTy->hasArchetype())
    return nullptr;

  // Ok, we can now perform our transformation. Grab AI's operands and the
  // relevant types from the ConvertFunction function type and AI.
  OperandValueArrayRef Ops = AI->getArgumentsWithoutIndirectResult();
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
      auto UAC = Builder->createUncheckedAddrCast(AI->getLoc(), Op, NewOpType);
      UAC->setDebugScope(AI->getDebugScope());
      Args.push_back(UAC);
    } else if (OldOpType.isHeapObjectReferenceType()) {
      assert(NewOpType.isHeapObjectReferenceType() &&
             "refs should map to refs.");
      auto URC = Builder->createUncheckedRefCast(AI->getLoc(), Op, NewOpType);
      URC->setDebugScope(AI->getDebugScope());
      Args.push_back(URC);
    } else {
      Args.push_back(Op);
    }
  }

  SILType CCSILTy = SILType::getPrimitiveObjectType(ConvertCalleeTy);
  // Create the new apply inst.
  auto NAI = ApplyInst::create(AI->getLoc(), FRI, CCSILTy,
                               ConvertCalleeTy->getSILResult(),
                               ArrayRef<Substitution>(), Args,
                               *FRI->getReferencedFunction());
  NAI->setDebugScope(AI->getDebugScope());
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
    if (auto SI = dyn_cast<StructExtractInst>(Inst->getUser()))
      if (recursivelyCollectARCUsers(Uses, SI))
        continue;

    return false;
  }

  return true;
}



SILInstruction *
SILCombiner::optimizeConcatenationOfStringLiterals(ApplyInst *AI) {
  // String literals concatenation optimizer.
  return tryToConcatenateStrings(AI, *Builder);
}

/// \brief Returns a list of instructions that only write into the uninitialized
/// array \p Inst.
static bool recursivelyCollectArrayWritesInstr(UserListTy &Uses,
                                               SILInstruction *Inst) {
  Uses.push_back(Inst);
  for (auto Op : Inst->getUses()) {
    if (isa<RefCountingInst>(Op->getUser()) ||
        // The store must not store the array but only to the array.
        (isa<StoreInst>(Op->getUser()) &&
         dyn_cast<StoreInst>(Op->getUser())->getSrc().getDef() != Inst) ||
        isa<DebugValueInst>(Op->getUser())) {
      Uses.push_back(Op->getUser());
      continue;
    }

    SILInstruction *Proj;
    if ((Proj = dyn_cast<TupleExtractInst>(Op->getUser())) ||
        (Proj = dyn_cast<StructExtractInst>(Op->getUser())) ||
        (Proj = dyn_cast<IndexAddrInst>(Op->getUser())) ||
        (Proj = dyn_cast<PointerToAddressInst>(Op->getUser())))
      if (recursivelyCollectArrayWritesInstr(Uses, Proj))
        continue;

    return false;
  }

  return true;
}

/// Optimize builtins which receive the same value in their first and second
/// operand.
static SILInstruction *optimizeBuiltinWithSameOperands(BuiltinInst *I,
                                                       SILCombiner *C) {
  SILFunction &F = *I->getFunction();

  // Handle all builtins which can be optimized.
  // We have to take special care about floating point operations because of
  // potential NaN values. E.g. ordered equal FCMP_OEQ(Nan, Nan) is not true.
  switch (I->getBuiltinInfo().ID) {
      
  // Replace the uses with one of the (identical) operands.
  case BuiltinValueKind::And:
  case BuiltinValueKind::Or: {
    // We cannot just _return_ the operand because it is not necessarily an
    // instruction. It can be an argument.
    SILValue Op = I->getOperand(0);
    C->replaceInstUsesWith(*I, Op.getDef(), 0, Op.getResultNumber());
    break;
  }

  // Return 0 or false.
  case BuiltinValueKind::Sub:
  case BuiltinValueKind::SRem:
  case BuiltinValueKind::URem:
  case BuiltinValueKind::Xor:
  case BuiltinValueKind::ICMP_NE:
  case BuiltinValueKind::ICMP_SLT:
  case BuiltinValueKind::ICMP_SGT:
  case BuiltinValueKind::ICMP_ULT:
  case BuiltinValueKind::ICMP_UGT:
  case BuiltinValueKind::FCMP_ONE:
    if (auto Ty = I->getType().getAs<BuiltinIntegerType>()) {
      return IntegerLiteralInst::create(I->getLoc(), I->getType(),
                                        APInt(Ty->getGreatestWidth(), 0), F);
    }
    break;
      
  // Return 1 or true.
  case BuiltinValueKind::ICMP_EQ:
  case BuiltinValueKind::ICMP_SLE:
  case BuiltinValueKind::ICMP_SGE:
  case BuiltinValueKind::ICMP_ULE:
  case BuiltinValueKind::ICMP_UGE:
  case BuiltinValueKind::FCMP_UEQ:
  case BuiltinValueKind::FCMP_UGE:
  case BuiltinValueKind::FCMP_ULE:
  case BuiltinValueKind::SDiv:
  case BuiltinValueKind::UDiv:
    if (auto Ty = I->getType().getAs<BuiltinIntegerType>()) {
      return IntegerLiteralInst::create(I->getLoc(), I->getType(),
                                        APInt(Ty->getGreatestWidth(), 1), F);
    }
    break;

  // Return 0 in a tuple.
  case BuiltinValueKind::SSubOver:
  case BuiltinValueKind::USubOver: {
    SILType Ty = I->getType();
    SILType IntTy = Ty.getTupleElementType(0);
    SILType BoolTy = Ty.getTupleElementType(1);
    SILBuilderWithScope<4> B(I);
    SILValue Elements[] = {
      B.createIntegerLiteral(I->getLoc(), IntTy, /* Result */ 0),
      B.createIntegerLiteral(I->getLoc(), BoolTy, /* Overflow */ 0)
    };
    return TupleInst::create(I->getLoc(), Ty, Elements, F);
  }
      
  default:
    break;
  }
  return nullptr;
}

/// Match an index pointer that is fed by a sizeof(T)*Distance offset.
static IndexRawPointerInst *
matchSizeOfMultiplication(SILValue I, MetatypeInst *RequiredType,
                          BuiltinInst *&TruncOrBitCast, SILValue &Ptr,
                          SILValue &Distance) {
  IndexRawPointerInst *Res = dyn_cast<IndexRawPointerInst>(I);
  if (!Res)
    return nullptr;

  SILValue Dist;
  MetatypeInst *StrideType;
  if (match(
          Res->getOperand(1),
          m_ApplyInst(
              BuiltinValueKind::TruncOrBitCast,
              m_TupleExtractInst(
                  m_ApplyInst(
                      BuiltinValueKind::SMulOver, m_SILValue(Dist),
                      m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                                  m_ApplyInst(BuiltinValueKind::StrideofNonZero,
                                              m_MetatypeInst(StrideType)))),
                  0))) ||
      match(
          Res->getOperand(1),
          m_ApplyInst(
              BuiltinValueKind::TruncOrBitCast,
              m_TupleExtractInst(
                  m_ApplyInst(
                      BuiltinValueKind::SMulOver,
                      m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                                  m_ApplyInst(BuiltinValueKind::StrideofNonZero,
                                              m_MetatypeInst(StrideType))),
                      m_SILValue(Dist)),
                  0)))) {
    if (StrideType != RequiredType)
      return nullptr;
    TruncOrBitCast = cast<BuiltinInst>(Res->getOperand(1));
    Distance = Dist;
    Ptr = Res->getOperand(0);
    return Res;
  }
  return nullptr;
}

/// Given an index_raw_pointer Ptr, size_of(Metatype) * Distance create an
/// address_to_pointer (index_addr ptr, Distance : $*Metatype) : $RawPointer
/// instruction.
static SILInstruction *createIndexAddrFrom(IndexRawPointerInst *I,
                                           MetatypeInst *Metatype,
                                           BuiltinInst *TruncOrBitCast,
                                           SILValue Ptr, SILValue Distance,
                                           SILType RawPointerTy,
                                           SILBuilder *Builder) {
  SILType InstanceType =
    Metatype->getType().getMetatypeInstanceType(I->getModule());

  auto *NewPTAI = Builder->createPointerToAddress(
    I->getLoc(), Ptr, InstanceType.getAddressType());
  NewPTAI->setDebugScope(I->getDebugScope());

  auto *DistanceAsWord =
      Builder->createBuiltin(I->getLoc(), TruncOrBitCast->getName(),
                             TruncOrBitCast->getType(), {}, Distance);
  DistanceAsWord->setDebugScope(I->getDebugScope());

  auto *NewIAI = Builder->createIndexAddr(I->getLoc(), NewPTAI, DistanceAsWord);
  NewIAI->setDebugScope(I->getDebugScope());

  auto *NewATPI =
    Builder->createAddressToPointer(I->getLoc(), NewIAI, RawPointerTy);
  NewATPI->setDebugScope(I->getDebugScope());
  return NewATPI;
}

/// Optimize an array operation that has (index_raw_pointer b, sizeof(T) * Dist)
/// operands into one that use index_addr as operands.
SILInstruction *optimizeBuiltinArrayOperation(BuiltinInst *I,
                                              SILBuilder *Builder) {
  if (I->getNumOperands() < 3)
    return nullptr;

  // Take something like this:
  //   %stride = Builtin.strideof(T) * %distance
  //   %ptr' = index_raw_pointer %ptr, %stride
  //     = builtin "takeArrayFrontToBack"<Int>(%metatype, %ptr', ...

  // And convert it to this:
  //   %addr = pointer_to_address %ptr, $T
  //   %result = index_addr %addr, %distance
  //   %ptr' = address_to_pointer result : $RawPointer
  //     = builtin "takeArrayFrontToBack"<Int>(%metatype, %ptr', ...

  auto *Metatype = dyn_cast<MetatypeInst>(I->getOperand(0));
  if (!Metatype)
    return nullptr;

  SILValue Ptr;
  SILValue Distance;
  BuiltinInst *TruncOrBitCast;
  SILValue NewOp1 = I->getOperand(1), NewOp2 = I->getOperand(2);

  // Try to replace the first pointer operand.
  auto *IdxRawPtr1 = matchSizeOfMultiplication(I->getOperand(1), Metatype,
                                               TruncOrBitCast, Ptr, Distance);
  if (IdxRawPtr1)
    NewOp1 = createIndexAddrFrom(IdxRawPtr1, Metatype, TruncOrBitCast, Ptr,
                                 Distance, NewOp1.getType(), Builder);

  // Try to replace the second pointer operand.
  auto *IdxRawPtr2 = matchSizeOfMultiplication(I->getOperand(2), Metatype,
                                               TruncOrBitCast, Ptr, Distance);
  if (IdxRawPtr2)
    NewOp2 = createIndexAddrFrom(IdxRawPtr2, Metatype, TruncOrBitCast, Ptr,
                                 Distance, NewOp2.getType(), Builder);

  if (NewOp1 != I->getOperand(1) || NewOp2 != I->getOperand(2)) {
    SmallVector<SILValue, 5> NewOpds;
    for (auto OldOpd : I->getArguments())
      NewOpds.push_back(OldOpd);
    NewOpds[1] = NewOp1;
    NewOpds[2] = NewOp2;
    return BuiltinInst::create(I->getLoc(), I->getName(), I->getType(),
                               I->getSubstitutions(), NewOpds,
                               *I->getFunction());
  }
  return nullptr;
}

/// Get operands of a binary bitop builtin where one operand is an integer
/// literal.
static bool getBitOpArgs(BuiltinInst *BI, SILValue &op, APInt &bits) {
  OperandValueArrayRef Args = BI->getArguments();
  if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[0])) {
    op = Args[1];
    bits = IL->getValue();
    return true;
  }
  if (auto *IL = dyn_cast<IntegerLiteralInst>(Args[1])) {
    op = Args[0];
    bits = IL->getValue();
    return true;
  }
  return false;
}

/// Optimizes binary bit operations. Optimizations for "and":
///   x & 0 -> 0
///   x & ~0 -> x
///   (x & c1) & c2 -> x & (c1 & c2)
/// The same optimizations are done for "or" and "xor".
template <typename CombineFunc, typename NeutralFunc, typename ZeroFunc>
SILInstruction *optimizeBitOp(BuiltinInst *BI,
                              CombineFunc combine,
                              NeutralFunc isNeutral,
                              ZeroFunc isZero,
                              SILBuilder *Builder,
                              SILCombiner *C) {
  SILValue firstOp;
  APInt bits;
  if (!getBitOpArgs(BI, firstOp, bits))
    return nullptr;

  // Combine all bits of consecutive bit operations, e.g. ((op & c1) & c2) & c3
  SILValue op = firstOp;
  BuiltinInst *Prev;
  APInt prevBits;
  while ((Prev = dyn_cast<BuiltinInst>(op)) &&
         Prev->getBuiltinInfo().ID == BI->getBuiltinInfo().ID &&
         getBitOpArgs(Prev, op, prevBits)) {
    combine(bits, prevBits);
  }
  if (isNeutral(bits))
    // The bit operation has no effect, e.g. x | 0 -> x
    return C->replaceInstUsesWith(*BI, op.getDef());

  if (isZero(bits))
    // The bit operation yields to a constant, e.g. x & 0 -> 0
    return IntegerLiteralInst::create(BI->getLoc(), BI->getType(), bits,
                                      *BI->getFunction());
  
  if (op != firstOp) {
    // We combined multiple bit operations to a single one,
    // e.g. (x & c1) & c2 -> x & (c1 & c2)
    auto *newLI = Builder->createIntegerLiteral(BI->getLoc(), BI->getType(),
                                                bits);
    return BuiltinInst::create(BI->getLoc(), BI->getName(), BI->getType(),
                               BI->getSubstitutions(),
                               { op, newLI },
                               *BI->getFunction());
  }
  return nullptr;
}

SILInstruction *SILCombiner::visitBuiltinInst(BuiltinInst *I) {
  if (I->getBuiltinInfo().ID == BuiltinValueKind::CanBeObjCClass)
    return optimizeBuiltinCanBeObjCClass(I);
  if (I->getBuiltinInfo().ID == BuiltinValueKind::TakeArrayFrontToBack ||
      I->getBuiltinInfo().ID == BuiltinValueKind::TakeArrayBackToFront ||
      I->getBuiltinInfo().ID == BuiltinValueKind::CopyArray)
    return optimizeBuiltinArrayOperation(I, Builder);

  if (I->getNumOperands() >= 2 && I->getOperand(0) == I->getOperand(1)) {
    // It's a builtin which has the same value in its first and second operand.
    SILInstruction *Replacement = optimizeBuiltinWithSameOperands(I, this);
    if (Replacement)
      return Replacement;
  }

  // Optimize this case for unsigned and equality comparisons:
  //   cmp_*_T . (zext U->T x, zext U->T y)
  //      => cmp_*_T (x, y)
  switch (I->getBuiltinInfo().ID) {
  case BuiltinValueKind::ICMP_EQ:
  case BuiltinValueKind::ICMP_NE:
  case BuiltinValueKind::ICMP_ULE:
  case BuiltinValueKind::ICMP_ULT:
  case BuiltinValueKind::ICMP_UGE:
  case BuiltinValueKind::ICMP_UGT: {
    SILValue LCast, RCast;
    if (match(I->getArguments()[0],
              m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                          m_SILValue(LCast))) &&
        match(I->getArguments()[1],
              m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                          m_SILValue(RCast))) &&
        LCast->getType(0) == RCast->getType(0)) {

      auto *NewCmp = Builder->createBuiltinBinaryFunction(
          I->getLoc(), getBuiltinName(I->getBuiltinInfo().ID),
          LCast->getType(0), I->getType(), {LCast, RCast});

      I->replaceAllUsesWith(NewCmp);
      replaceInstUsesWith(*I, NewCmp, 0);
      return eraseInstFromFunction(*I);
    }
    break;
  }
  case BuiltinValueKind::And:
    return optimizeBitOp(I,
      [](APInt &left, const APInt &right) { left &= right; }    /* combine */,
      [](const APInt &i) -> bool { return i.isAllOnesValue(); } /* isNeutral */,
      [](const APInt &i) -> bool { return i.isMinValue(); }     /* isZero */,
      Builder, this);
  case BuiltinValueKind::Or:
    return optimizeBitOp(I,
      [](APInt &left, const APInt &right) { left |= right; }    /* combine */,
      [](const APInt &i) -> bool { return i.isMinValue(); }     /* isNeutral */,
      [](const APInt &i) -> bool { return i.isAllOnesValue(); } /* isZero */,
      Builder, this);
  case BuiltinValueKind::Xor:
    return optimizeBitOp(I,
      [](APInt &left, const APInt &right) { left ^= right; } /* combine */,
      [](const APInt &i) -> bool { return i.isMinValue(); }  /* isNeutral */,
      [](const APInt &i) -> bool { return false; }           /* isZero */,
      Builder, this);
  default:
    break;
  }
  
  if (I->getBuiltinInfo().ID == BuiltinValueKind::ICMP_EQ)
    return optimizeBuiltinCompareEq(I, /*Negate Eq result*/ false);

  if (I->getBuiltinInfo().ID == BuiltinValueKind::ICMP_NE)
    return optimizeBuiltinCompareEq(I, /*Negate Eq result*/ true);

  // Optimize sub(ptrtoint(index_raw_pointer(v, x)), ptrtoint(v)) -> x.
  BuiltinInst *Bytes2;
  IndexRawPointerInst *Indexraw;
  if (I->getNumOperands() == 2 &&
      match(I, m_BuiltinInst(BuiltinValueKind::Sub,
                             m_BuiltinInst(BuiltinValueKind::PtrToInt,
                                           m_IndexRawPointerInst(Indexraw)),
                             m_BuiltinInst(Bytes2)))) {
    if (match(Bytes2,
              m_BuiltinInst(BuiltinValueKind::PtrToInt, m_ValueBase()))) {
      if (Indexraw->getOperand(0) == Bytes2->getOperand(0) &&
          Indexraw->getOperand(1).getType() == I->getType()) {
        replaceInstUsesWith(*I, Indexraw->getOperand(1).getDef());
        return eraseInstFromFunction(*I);
      }
    }
  }

  // Canonicalize multiplication by a stride to be such that the stride is
  // always the second argument.
  if (I->getNumOperands() != 3)
    return nullptr;

  if (match(I, m_ApplyInst(BuiltinValueKind::SMulOver,
                            m_ApplyInst(BuiltinValueKind::Strideof),
                            m_ValueBase(), m_IntegerLiteralInst())) ||
      match(I, m_ApplyInst(BuiltinValueKind::SMulOver,
                            m_ApplyInst(BuiltinValueKind::StrideofNonZero),
                            m_ValueBase(), m_IntegerLiteralInst()))) {
    I->swapOperands(0, 1);
    return I;
  }

  return nullptr;
}

/// Propagate information about a concrete type from init_existential_addr
/// or init_existential_ref into witness_method conformances and into
/// apply instructions.
/// This helps the devirtualizer to replace witness_method by
/// class_method instructions and then devirtualize.
SILInstruction *
SILCombiner::propagateConcreteTypeOfInitExistential(ApplyInst *AI,
                                                    WitnessMethodInst *WMI,
                                                    SILValue InitExistential,
                                                    SILType InstanceType) {
  // Replace this witness_method by a more concrete one
  ArrayRef<ProtocolConformance*> Conformances;
  CanType ConcreteType;
  SILValue LastArg;

  if (auto IE = dyn_cast<InitExistentialAddrInst>(InitExistential)) {
    Conformances = IE->getConformances();
    ConcreteType = IE->getFormalConcreteType();
    LastArg = IE;
  } else if (auto IER = dyn_cast<InitExistentialRefInst>(InitExistential)) {
    Conformances = IER->getConformances();
    ConcreteType = IER->getFormalConcreteType();
    LastArg = IER->getOperand();
  }
  // If ConcreteType depends on any archetypes, then propagating it does not
  // help resolve witness table lookups. Catch these cases before calling
  // gatherAllSubstitutions, which only works on nominal types.
  if (ConcreteType->hasArchetype())
    return nullptr;

  auto ConcreteTypeSubsts = ConcreteType->gatherAllSubstitutions(
      AI->getModule().getSwiftModule(), nullptr);
  if (!ConcreteTypeSubsts.empty()) {
    // Bail if any generic types parameters of the concrete type are unbound.
    if (hasUnboundGenericTypes(ConcreteTypeSubsts))
      return nullptr;
    // At this point we know that all replacements use concrete types
    // and therefore the whole Lookup type is concrete. So, we can
    // propagate it, because we know how to devirtualize it.
  }

  if (Conformances.empty())
    return nullptr;

  ProtocolConformance *Conformance = nullptr;

  // Find the conformance related to witness_method
  for (auto Con : Conformances) {
    if (Con->getProtocol() == WMI->getLookupProtocol()) {
      Conformance = Con;
      break;
    }
  }
  if (!Conformance)
    return nullptr;

  // Don't specialize Apply instructions that return the Self type. Notice that
  // it is sufficient to compare the return type to the substituted type because
  // types that depend on the Self type are not allowed (for example [Self] is
  // not allowed).
  if (AI->getType().getSwiftType().getLValueOrInOutObjectType() ==
      WMI->getLookupType())
    return nullptr;

  SmallVector<SILValue, 8> Args;
  for (auto Arg : AI->getArgumentsWithoutSelf()) {
    Args.push_back(Arg);

    // Below we have special handling for the 'LastArg', which is the self
    // parameter. However, we also need to handle the Self return type.
    // In here we find arguments that are not the 'self' argument and if
    // they are of the Self type then we abort the optimization.
    if (Arg.getType().getSwiftType().getLValueOrInOutObjectType() ==
        WMI->getLookupType())
      return nullptr;
  }

  Args.push_back(LastArg);

  SILValue OptionalExistential =
  WMI->hasOperand() ? WMI->getOperand() : SILValue();
  auto *NewWMI = Builder->createWitnessMethod(WMI->getLoc(),
                                              ConcreteType,
                                              Conformance, WMI->getMember(),
                                              WMI->getType(),
                                              OptionalExistential,
                                              WMI->isVolatile());
  replaceInstUsesWith(*WMI, NewWMI, 0);
  eraseInstFromFunction(*WMI);

  SmallVector<Substitution, 8> Substitutions;
  for (auto Subst : AI->getSubstitutions()) {
    if (Subst.getArchetype()->isSelfDerived()) {
      Substitution NewSubst(Subst.getArchetype(), ConcreteType,
                            Subst.getConformances());
      Substitutions.push_back(NewSubst);
    } else
      Substitutions.push_back(Subst);
  }

  SILType SubstCalleeType = AI->getSubstCalleeSILType();

  SILType NewSubstCalleeType;

  auto FnTy = AI->getCallee().getType().getAs<SILFunctionType>();
  if (FnTy && FnTy->isPolymorphic()) {
    // Handle polymorphic functions by properly substituting
    // their parameter types.
    CanSILFunctionType SFT = FnTy->substGenericArgs(
                                        AI->getModule(),
                                        AI->getModule().getSwiftModule(),
                                        Substitutions);
    NewSubstCalleeType = SILType::getPrimitiveObjectType(SFT);
  } else {
    TypeSubstitutionMap TypeSubstitutions;
    TypeSubstitutions[InstanceType.getSwiftType().getPointer()] = ConcreteType;
    NewSubstCalleeType = SubstCalleeType.subst(AI->getModule(),
                                               AI->getModule().getSwiftModule(),
                                               TypeSubstitutions);
  }

  auto NewAI = Builder->createApply(AI->getLoc(), AI->getCallee(),
                                    NewSubstCalleeType,
                                    AI->getType(), Substitutions, Args);

  NewAI->setDebugScope(AI->getDebugScope());

  replaceInstUsesWith(*AI, NewAI, 0);
  eraseInstFromFunction(*AI);

  if (CG)
    CG->addEdgesForApply(NewAI);

  return nullptr;
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
static ApplyInst *optimizeCastThroughThinFuntionPointer(
    SILBuilder *Builder, ApplyInst *AI, FunctionRefInst *OrigThinFun,
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
  assert(Subs.size() == 1);

  SmallVector<SILValue, 16> Args;
  for (auto Arg : AI->getArguments())
    Args.push_back(Arg);

  auto NewSubstCalleeType =
      SILType::getPrimitiveObjectType(ConvertCalleeTy->substGenericArgs(
          AI->getModule(), AI->getModule().getSwiftModule(), Subs));

  ApplyInst *NewApply = Builder->createApply(
      AI->getLoc(), OrigThinFun, NewSubstCalleeType, AI->getType(), Subs, Args);
  NewApply->setDebugScope(AI->getDebugScope());

  return NewApply;
}

/// \brief Check that all users of the apply are retain/release ignoring one
/// user.
static bool
hasOnlyRetainReleaseUsers(ApplyInst *AI, SILInstruction *IgnoreUser,
                          SmallVectorImpl<SILInstruction *> &Users) {
  for (auto *Use : AI->getUses()) {
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
      .getBridgedToObjC(M, /*inExpression*/ false, Type.getSwiftRValueType(),
                        nullptr)
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
        if (auto *NewAI = optimizeCastThroughThinFuntionPointer(
                Builder, AI, OrigThinFun, CastedThinFun)) {
          replaceInstUsesWith(*AI, NewAI, 0);
          eraseInstFromFunction(*AI);
          if (CG)
            CG->addEdgesForApply(NewAI);
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
          Builder->emitReleaseValueOperation(AI->getLoc(), Arg);
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
      if (recursivelyCollectArrayWritesInstr(Users, AI)) {
        // Erase all of the stores to the dead array, the reference counting
        // instructions on the array and the allocation-apply itself.
        for (auto rit = Users.rbegin(), re = Users.rend(); rit != re; ++rit) {
          SILInstruction *I = *rit;
          if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
            // We must release all stored values (because they are not stored
            // anymore).
            SILValue V = SI->getSrc();
            SILBuilder BuilderAtStore(SI);
            BuilderAtStore.emitReleaseValueOperation(SI->getLoc(), V);
          }
          eraseInstFromFunction(*I);
        }
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
    auto *NewAI = ApplyInst::create(AI->getLoc(), TTTFI->getOperand(),
                             substTy, AI->getType(),
                             AI->getSubstitutions(), Arguments,
                             *AI->getFunction());
    NewAI->setDebugScope(AI->getDebugScope());
    return NewAI;
  }

  // (apply (witness_method)) -> propagate information about
  // a concrete type from init_existential_addr or init_existential_ref.
  if (auto *WMI = dyn_cast<WitnessMethodInst>(AI->getCallee())) {
    if (WMI->getConformance())
      return nullptr;
    auto LastArg = AI->getArguments().back();
    // Try to derive conformances from the apply_inst
    if (auto *Instance = dyn_cast<OpenExistentialAddrInst>(LastArg)) {
      auto Op = Instance->getOperand();
      for (auto Use : Op.getUses()) {
        if (auto *IE = dyn_cast<InitExistentialAddrInst>(Use->getUser())) {
          // IE should dominate Instance.
          // Without a DomTree we want to be very defensive
          // and only allow this optimization when it is used
          // inside the same BB.
          if (IE->getParent() != AI->getParent())
            continue;
          return propagateConcreteTypeOfInitExistential(AI, WMI, IE,
                                                        Instance->getType());
        }
      }
    }

    if (auto *Instance = dyn_cast<OpenExistentialRefInst>(LastArg)) {
      if (auto *IE = dyn_cast<InitExistentialRefInst>(Instance->getOperand())) {
        // IE should dominate Instance.
        // Without a DomTree we want to be very defensive
        // and only allow this optimization when it is used
        // inside the same BB.
        if (IE->getParent() == AI->getParent())
          return propagateConcreteTypeOfInitExistential(AI, WMI, IE,
                                                        Instance->getType());
      }
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
  Builder->setInsertionPoint(CFI->getParent());
  Builder->createUnreachable(ArtificialUnreachableLocation());

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

SILInstruction *
SILCombiner::visitRefToRawPointerInst(RefToRawPointerInst *RRPI) {
  // Ref to raw pointer consumption of other ref casts.
  //
  // (ref_to_raw_pointer (unchecked_ref_cast x))
  //    -> (ref_to_raw_pointer x)
  if (auto *ROPI = dyn_cast<UncheckedRefCastInst>(RRPI->getOperand())) {
    RRPI->setOperand(ROPI->getOperand());
    return ROPI->use_empty() ? eraseInstFromFunction(*ROPI) : nullptr;
  }

  // (ref_to_raw_pointer (open_existential_ref (init_existential_ref x))) ->
  // (ref_to_raw_pointer x)
  if (auto *OER = dyn_cast<OpenExistentialRefInst>(RRPI->getOperand()))
    if (auto *IER = dyn_cast<InitExistentialRefInst>(OER->getOperand()))
      return new (RRPI->getModule()) RefToRawPointerInst(
          RRPI->getLoc(), IER->getOperand(), RRPI->getType());

  // (ref_to_raw_pointer (unchecked_ref_bit_cast x))
  //    -> (unchecked_trivial_bit_cast x)
  if (auto *URBCI = dyn_cast<UncheckedRefBitCastInst>(RRPI->getOperand())) {
    return new (RRPI->getModule()) UncheckedTrivialBitCastInst(
        RRPI->getLoc(), URBCI->getOperand(), RRPI->getType());
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
      Builder->createEnum(IEAI->getLoc(), SILValue(), IEAI->getElement(),
                          IEAI->getOperand().getType().getObjectType());
    E->setDebugScope(IEAI->getDebugScope());
    Builder->createStore(IEAI->getLoc(), E, IEAI->getOperand())
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
      Builder->createEnum(DataAddrInst->getLoc(), SI->getSrc(),
                          DataAddrInst->getElement(),
                          DataAddrInst->getOperand().getType().getObjectType());
  E->setDebugScope(DataAddrInst->getDebugScope());
  Builder->createStore(DataAddrInst->getLoc(), E, DataAddrInst->getOperand())
    ->setDebugScope(DataAddrInst->getDebugScope());
  // Cleanup.
  eraseInstFromFunction(*SI);
  eraseInstFromFunction(*DataAddrInst);
  return eraseInstFromFunction(*IEAI);
}

SILInstruction *SILCombiner::visitUpcastInst(UpcastInst *UCI) {
  // Ref to raw pointer consumption of other ref casts.
  //
  // (upcast (upcast x)) -> (upcast x)
  if (auto *Op = dyn_cast<UpcastInst>(UCI->getOperand())) {
    UCI->setOperand(Op->getOperand());
    return Op->use_empty() ? eraseInstFromFunction(*Op) : nullptr;
  }

  return nullptr;
}

SILInstruction *
SILCombiner::
visitPointerToAddressInst(PointerToAddressInst *PTAI) {
  // If we reach this point, we know that the types must be different since
  // otherwise simplifyInstruction would have handled the identity case. This is
  // always legal to do since address-to-pointer pointer-to-address implies
  // layout compatibility.
  //
  // (pointer-to-address (address-to-pointer %x)) -> unchecked_
  if (auto *ATPI = dyn_cast<AddressToPointerInst>(PTAI->getOperand())) {
    return new (PTAI->getModule()) UncheckedAddrCastInst(PTAI->getLoc(),
                                                         ATPI->getOperand(),
                                                         PTAI->getType());
  }

  // Turn this also into a index_addr. We generate this pattern after switching
  // the Word type to an explicit Int32 or Int64 in the stdlib.
  //
  // %101 = builtin "strideof_nonzero"<Int>(%84 : $@thick Int.Type) :
  //         $Builtin.Word
  // %102 = builtin "zextOrBitCast_Word_Int64"(%101 : $Builtin.Word) :
  //         $Builtin.Int64
  // %111 = builtin "smul_with_overflow_Int64"(%108 : $Builtin.Int64,
  //                               %102 : $Builtin.Int64, %20 : $Builtin.Int1) :
  //         $(Builtin.Int64, Builtin.Int1)
  // %112 = tuple_extract %111 : $(Builtin.Int64, Builtin.Int1), 0
  // %113 = builtin "truncOrBitCast_Int64_Word"(%112 : $Builtin.Int64) :
  //         $Builtin.Word
  // %114 = index_raw_pointer %100 : $Builtin.RawPointer, %113 : $Builtin.Word
  // %115 = pointer_to_address %114 : $Builtin.RawPointer to $*Int
  SILValue Distance;
  SILValue TruncOrBitCast;
  MetatypeInst *Metatype;
  IndexRawPointerInst *IndexRawPtr;
  BuiltinInst *StrideMul;
  if (match(
          PTAI->getOperand(),
          m_IndexRawPointerInst(IndexRawPtr))) {
    SILValue Ptr = IndexRawPtr->getOperand(0);
    SILValue TruncOrBitCast = IndexRawPtr->getOperand(1);
    if (match(TruncOrBitCast,
              m_ApplyInst(BuiltinValueKind::TruncOrBitCast,
                          m_TupleExtractInst(m_BuiltinInst(StrideMul), 0)))) {
      if (match(StrideMul,
                m_ApplyInst(
                    BuiltinValueKind::SMulOver, m_SILValue(Distance),
                    m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                                m_ApplyInst(BuiltinValueKind::StrideofNonZero,
                                            m_MetatypeInst(Metatype))))) ||
          match(StrideMul,
                m_ApplyInst(
                    BuiltinValueKind::SMulOver,
                    m_ApplyInst(BuiltinValueKind::ZExtOrBitCast,
                                m_ApplyInst(BuiltinValueKind::StrideofNonZero,
                                            m_MetatypeInst(Metatype))),
                    m_SILValue(Distance)))) {
        SILType InstanceType =
            Metatype->getType().getMetatypeInstanceType(PTAI->getModule());
        auto *Trunc = cast<BuiltinInst>(TruncOrBitCast);

        // Make sure that the type of the metatype matches the type that we are
        // casting to so we stride by the correct amount.
        if (InstanceType.getAddressType() != PTAI->getType()) {
          return nullptr;
        }

        auto *NewPTAI = Builder->createPointerToAddress(PTAI->getLoc(), Ptr,
                                                        PTAI->getType());
        auto DistanceAsWord = Builder->createBuiltin(
            PTAI->getLoc(), Trunc->getName(), Trunc->getType(), {}, Distance);

        NewPTAI->setDebugScope(PTAI->getDebugScope());
        return new (PTAI->getModule())
            IndexAddrInst(PTAI->getLoc(), NewPTAI, DistanceAsWord);
      }
    }
  }
  // Turn:
  //
  //   %stride = Builtin.strideof(T) * %distance
  //   %ptr' = index_raw_pointer %ptr, %stride
  //   %result = pointer_to_address %ptr, $T'
  //
  // To:
  //
  //   %addr = pointer_to_address %ptr, $T
  //   %result = index_addr %addr, %distance
  //
  BuiltinInst *Bytes;
  if (match(PTAI->getOperand(),
            m_IndexRawPointerInst(m_ValueBase(),
                                  m_TupleExtractInst(m_BuiltinInst(Bytes),
                                                     0)))) {
    if (match(Bytes, m_ApplyInst(BuiltinValueKind::SMulOver, m_ValueBase(),
                                 m_ApplyInst(BuiltinValueKind::Strideof,
                                             m_MetatypeInst(Metatype)),
                                 m_ValueBase())) ||
        match(Bytes, m_ApplyInst(BuiltinValueKind::SMulOver, m_ValueBase(),
                                 m_ApplyInst(BuiltinValueKind::StrideofNonZero,
                                             m_MetatypeInst(Metatype)),
                                 m_ValueBase()))) {
      SILType InstanceType =
        Metatype->getType().getMetatypeInstanceType(PTAI->getModule());

      // Make sure that the type of the metatype matches the type that we are
      // casting to so we stride by the correct amount.
      if (InstanceType.getAddressType() != PTAI->getType())
        return nullptr;

      auto IRPI = cast<IndexRawPointerInst>(PTAI->getOperand().getDef());
      SILValue Ptr = IRPI->getOperand(0);
      SILValue Distance = Bytes->getArguments()[0];
      auto *NewPTAI =
          Builder->createPointerToAddress(PTAI->getLoc(), Ptr, PTAI->getType());
      NewPTAI->setDebugScope(PTAI->getDebugScope());
      return new (PTAI->getModule())
          IndexAddrInst(PTAI->getLoc(), NewPTAI, Distance);
    }
  }

  return nullptr;
}

SILInstruction *
SILCombiner::visitUncheckedAddrCastInst(UncheckedAddrCastInst *UADCI) {
  SILModule &Mod = UADCI->getModule();

  // (unchecked-addr-cast (unchecked-addr-cast x X->Y) Y->Z)
  //   ->
  // (unchecked-addr-cast x X->Z)
  if (auto *OtherUADCI = dyn_cast<UncheckedAddrCastInst>(UADCI->getOperand()))
    return new (Mod) UncheckedAddrCastInst(
        UADCI->getLoc(), OtherUADCI->getOperand(), UADCI->getType());

  // (unchecked-addr-cast cls->superclass) -> (upcast cls->superclass)
  if (UADCI->getType() != UADCI->getOperand().getType() &&
      UADCI->getType().isSuperclassOf(UADCI->getOperand().getType()))
    return new (Mod) UpcastInst(UADCI->getLoc(), UADCI->getOperand(),
                                UADCI->getType());

  // See if we have all loads from this unchecked_addr_cast. If we do, load the
  // original type and create the appropriate bitcast.

  // First if our UADCI has not users, bail. This will be eliminated by DCE.
  if (UADCI->use_empty())
    return nullptr;

  SILType InputTy = UADCI->getOperand().getType();
  SILType OutputTy = UADCI->getType();

  // If either type is address only, do not do anything here.
  if (InputTy.isAddressOnly(Mod) || OutputTy.isAddressOnly(Mod))
    return nullptr;

  bool InputIsTrivial = InputTy.isTrivial(Mod);
  bool OutputIsTrivial = OutputTy.isTrivial(Mod);

  // If our input is trivial and our output type is not, do not do
  // anything. This is to ensure that we do not change any types reference
  // semantics from trivial -> reference counted.
  if (InputIsTrivial && !OutputIsTrivial)
    return nullptr;

  // The structs could have different size. We have code in the stdlib that
  // casts pointers to differently sized integer types. This code prevents that
  // we bitcast the values.
  if (InputTy.getStructOrBoundGenericStruct() &&
      OutputTy.getStructOrBoundGenericStruct())
    return nullptr;

  // For each user U of the unchecked_addr_cast...
  for (auto U : UADCI->getUses())
    // Check if it is load. If it is not a load, bail...
    if (!isa<LoadInst>(U->getUser()))
      return nullptr;

  SILValue Op = UADCI->getOperand();
  SILLocation Loc = UADCI->getLoc();
  SILDebugScope *Scope = UADCI->getDebugScope();

  // Ok, we have all loads. Lets simplify this. Go back through the loads a
  // second time, rewriting them into a load + bitcast from our source.
  for (auto U : UADCI->getUses()) {
    // Grab the original load.
    LoadInst *L = cast<LoadInst>(U->getUser());

    // Insert a new load from our source and bitcast that as appropriate.
    LoadInst *NewLoad = Builder->createLoad(Loc, Op);
    NewLoad->setDebugScope(Scope);
    SILInstruction *BitCast = nullptr;
    if (OutputIsTrivial)
      BitCast = Builder->createUncheckedTrivialBitCast(Loc, NewLoad,
                                                       OutputTy.getObjectType());
    else
      BitCast = Builder->createUncheckedRefBitCast(Loc, NewLoad,
                                                   OutputTy.getObjectType());
    BitCast->setDebugScope(Scope);

    // Replace all uses of the old load with the new bitcasted result and erase
    // the old load.
    replaceInstUsesWith(*L, BitCast, 0);
    eraseInstFromFunction(*L);
  }

  // Delete the old cast.
  return eraseInstFromFunction(*UADCI);
}

SILInstruction *
SILCombiner::visitUncheckedRefCastInst(UncheckedRefCastInst *URCI) {
  // (unchecked-ref-cast (unchecked-ref-cast x X->Y) Y->Z)
  //   ->
  // (unchecked-ref-cast x X->Z)
  if (auto *OtherURCI = dyn_cast<UncheckedRefCastInst>(URCI->getOperand()))
    return new (URCI->getModule()) UncheckedRefCastInst(
        URCI->getLoc(), OtherURCI->getOperand(), URCI->getType());

  // (unchecked_ref_cast (upcast x X->Y) Y->Z) -> (unchecked_ref_cast x X->Z)
  if (auto *UI = dyn_cast<UpcastInst>(URCI->getOperand()))
    return new (URCI->getModule())
        UncheckedRefCastInst(URCI->getLoc(), UI->getOperand(), URCI->getType());

  if (URCI->getType() != URCI->getOperand().getType() &&
      URCI->getType().isSuperclassOf(URCI->getOperand().getType()))
    return new (URCI->getModule())
        UpcastInst(URCI->getLoc(), URCI->getOperand(), URCI->getType());

  // (unchecked_ref_cast (open_existential_ref (init_existential_ref X))) ->
  // (unchecked_ref_cast X)
  if (auto *OER = dyn_cast<OpenExistentialRefInst>(URCI->getOperand()))
    if (auto *IER = dyn_cast<InitExistentialRefInst>(OER->getOperand()))
      return new (URCI->getModule()) UncheckedRefCastInst(
          URCI->getLoc(), IER->getOperand(), URCI->getType());

  return nullptr;
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

SILInstruction *
SILCombiner::
visitUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *UCCAI) {
  CastOpt.optimizeUnconditionalCheckedCastAddrInst(UCCAI);
  return nullptr;
}

SILInstruction *
SILCombiner::
visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *UCCI) {
  if (CastOpt.optimizeUnconditionalCheckedCastInst(UCCI))
    return nullptr;

  // FIXME: rename from RemoveCondFails to RemoveRuntimeAsserts.
  if (RemoveCondFails) {
    auto LoweredTargetType = UCCI->getType();
    auto &Mod = UCCI->getModule();
    auto Loc = UCCI->getLoc();
    auto Op = UCCI->getOperand();
    if (LoweredTargetType.isAddress()) {
      // unconditional_checked_cast -> unchecked_addr_cast
      return new (Mod) UncheckedAddrCastInst(Loc, Op, LoweredTargetType);
    } else if (LoweredTargetType.isHeapObjectReferenceType()) {
      // unconditional_checked_cast -> unchecked_ref_cast
      return new (Mod) UncheckedRefCastInst(Loc, Op, LoweredTargetType);
    }
  }

  return nullptr;
}

SILInstruction *
SILCombiner::
visitRawPointerToRefInst(RawPointerToRefInst *RawToRef) {
  // (raw_pointer_to_ref (ref_to_raw_pointer x X->Y) Y->Z)
  //   ->
  // (unchecked_ref_cast X->Z)
  if (auto *RefToRaw = dyn_cast<RefToRawPointerInst>(RawToRef->getOperand())) {
    return new (RawToRef->getModule()) UncheckedRefCastInst(
        RawToRef->getLoc(), RefToRaw->getOperand(), RawToRef->getType());
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
  for (auto U : TEDAI->getUses())
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
  for (auto U : TEDAI->getUses()) {
    // Grab the load.
    LoadInst *L = cast<LoadInst>(U->getUser());

    // Insert a new Load of the enum and extract the data from that.
    auto *Load = Builder->createLoad(Loc, EnumAddr);
    Load->setDebugScope(Scope);
    auto *D = Builder->createUncheckedEnumData(
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
    return CondBranchInst::create(CBI->getLoc(), X,
                                  CBI->getFalseBB(), OrigFalseArgs,
                                  CBI->getTrueBB(), OrigTrueArgs,
                                  *CBI->getFunction());
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

    return SwitchEnumInst::create(SEI->getLoc(), SEI->getEnumOperand(), Default,
                                  Cases, *SEI->getFunction());
  }

  return nullptr;
}

SILInstruction *
SILCombiner::
visitUncheckedRefBitCastInst(UncheckedRefBitCastInst *URBCI) {
  // (unchecked_ref_bit_cast Y->Z (unchecked_ref_bit_cast X->Y x))
  //   ->
  // (unchecked_ref_bit_cast X->Z x)
  if (auto *Op = dyn_cast<UncheckedRefBitCastInst>(URBCI->getOperand())) {
    return new (URBCI->getModule()) UncheckedRefBitCastInst(URBCI->getLoc(),
                                                            Op->getOperand(),
                                                            URBCI->getType());
  }

  return nullptr;
}

SILInstruction *
SILCombiner::
visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *UTBCI) {
  // (unchecked_trivial_bit_cast Y->Z
  //                                 (unchecked_trivial_bit_cast X->Y x))
  //   ->
  // (unchecked_trivial_bit_cast X->Z x)
  SILValue Op = UTBCI->getOperand();
  if (auto *OtherUTBCI = dyn_cast<UncheckedTrivialBitCastInst>(Op)) {
    SILModule &Mod = UTBCI->getModule();
    return new (Mod) UncheckedTrivialBitCastInst(UTBCI->getLoc(),
                                                 OtherUTBCI->getOperand(),
                                                 UTBCI->getType());
  }

  // (unchecked_trivial_bit_cast Y->Z
  //                                 (unchecked_ref_bit_cast X->Y x))
  //   ->
  // (unchecked_trivial_bit_cast X->Z x)
  if (auto *URBCI = dyn_cast<UncheckedRefBitCastInst>(Op)) {
    SILModule &Mod = UTBCI->getModule();
    return new (Mod) UncheckedTrivialBitCastInst(UTBCI->getLoc(),
                                                 URBCI->getOperand(),
                                                 UTBCI->getType());
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitSelectEnumInst(SelectEnumInst *EIT) {
  // TODO: We should be able to flat-out replace the select_enum instruction
  // with the selected value in another pass. For parity with the enum_is_tag
  // combiner pass, handle integer literals for now.
  auto *EI = dyn_cast<EnumInst>(EIT->getEnumOperand());
  if (!EI)
    return nullptr;

  SILValue selected;
  for (unsigned i = 0, e = EIT->getNumCases(); i < e; ++i) {
    auto casePair = EIT->getCase(i);
    if (casePair.first == EI->getElement()) {
      selected = casePair.second;
      break;
    }
  }
  if (!selected)
    selected = EIT->getDefaultResult();

  if (auto inst = dyn_cast<IntegerLiteralInst>(selected)) {
    return IntegerLiteralInst::create(inst->getLoc(), inst->getType(),
                                      inst->getValue(), *EIT->getFunction());
  }

  return nullptr;
}

/// Helper function for simplifying convertions between
/// thick and objc metatypes.
static SILInstruction *
visitMetatypeConversionInst(ConversionInst *MCI,
                            MetatypeRepresentation Representation) {
  SILValue Op = MCI->getOperand(0);
  SILModule &Mod = MCI->getModule();
  // Instruction has a proper target type already.
  SILType Ty = MCI->getType();
  auto MetatypeTy = Op.getType().getAs<AnyMetatypeType>();

  if (MetatypeTy->getRepresentation() != Representation)
    return nullptr;

  if (dyn_cast<MetatypeInst>(Op)) {
    return new (Mod) MetatypeInst(MCI->getLoc(), Ty);
  } else if (auto *VMI = dyn_cast<ValueMetatypeInst>(Op)) {
    return new (Mod) ValueMetatypeInst(MCI->getLoc(),
                                       Ty,
                                       VMI->getOperand());
  } else if (auto *EMI = dyn_cast<ExistentialMetatypeInst>(Op)) {
    return new (Mod) ExistentialMetatypeInst(MCI->getLoc(),
                                             Ty,
                                             EMI->getOperand());
  }
  return nullptr;
}

SILInstruction *
SILCombiner::visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *TTOCMI) {
  // Perform the following transformations:
  // (thick_to_objc_metatype (metatype @thick)) ->
  // (metatype @objc_metatype)
  //
  // (thick_to_objc_metatype (value_metatype @thick)) ->
  // (value_metatype @objc_metatype)
  //
  // (thick_to_objc_metatype (existential_metatype @thick)) ->
  // (existential_metatype @objc_metatype)
  return visitMetatypeConversionInst(TTOCMI, MetatypeRepresentation::Thick);
}

SILInstruction *
SILCombiner::visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *OCTTMI) {
  // Perform the following transformations:
  // (objc_to_thick_metatype (metatype @objc_metatype)) ->
  // (metatype @thick)
  //
  // (objc_to_thick_metatype (value_metatype @objc_metatype)) ->
  // (value_metatype @thick)
  //
  // (objc_to_thick_metatype (existential_metatype @objc_metatype)) ->
  // (existential_metatype @thick)
  return visitMetatypeConversionInst(OCTTMI, MetatypeRepresentation::ObjC);
}

SILInstruction *SILCombiner::visitTupleExtractInst(TupleExtractInst *TEI) {
  // tuple_extract(apply([add|sub|...]overflow(x, 0)), 1) -> 0
  // if it can be proven that no overflow can happen.
  if (TEI->getFieldNo() != 1)
    return nullptr;

  if (auto *BI = dyn_cast<BuiltinInst>(TEI->getOperand()))
    if (!canOverflow(BI))
      return IntegerLiteralInst::create(TEI->getLoc(), TEI->getType(),
                                        APInt(1, 0), *TEI->getFunction());
  return nullptr;
}

SILInstruction *SILCombiner::visitFixLifetimeInst(FixLifetimeInst *FLI) {
  // fix_lifetime(alloc_stack) -> fix_lifetime(load(alloc_stack))
  if (auto *AI = dyn_cast<AllocStackInst>(FLI->getOperand())) {
    if (FLI->getOperand().getType().isLoadable(FLI->getModule())) {
      auto Load = Builder->createLoad(FLI->getLoc(), SILValue(AI, 1));
      Load->setDebugScope(FLI->getDebugScope());
      return new (FLI->getModule())
          FixLifetimeInst(FLI->getLoc(), SILValue(Load, 0));
    }
  }
  return nullptr;
}

SILInstruction *
SILCombiner::visitCheckedCastBranchInst(CheckedCastBranchInst *CBI) {
  CastOpt.optimizeCheckedCastBranchInst(CBI);
  return nullptr;
}

SILInstruction *
SILCombiner::
visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *CCABI) {
  CastOpt.optimizeCheckedCastAddrBranchInst(CCABI);
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
    auto *ARI = new (Mod) AllocRefInst(
        ARDI->getLoc(), SILInstanceTy, *ARDI->getFunction(), ARDI->isObjC());
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
      auto *ARI = new (Mod) AllocRefInst(
          ARDI->getLoc(), SILInstanceTy, *ARDI->getFunction(), ARDI->isObjC());
      ARI->setDebugScope(ARDI->getDebugScope());
      return ARI;
    }
  }
  return nullptr;
}
