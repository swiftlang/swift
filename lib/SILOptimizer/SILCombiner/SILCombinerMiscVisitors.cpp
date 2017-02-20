//===--- SILCombinerMiscVisitors.cpp --------------------------------------===//
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

#define DEBUG_TYPE "sil-combine"
#include "SILCombiner.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/CFG.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;
using namespace swift::PatternMatch;

SILInstruction*
SILCombiner::visitAllocExistentialBoxInst(AllocExistentialBoxInst *AEBI) {

  // Optimize away the pattern below that happens when exceptions are created
  // and in some cases, due to inlining, are not needed.
  //
  //   %6 = alloc_existential_box $Error, $ColorError
  //   %7 = enum $VendingMachineError, #ColorError.Red
  //   store %7 to %6#1 : $*ColorError
  //   debug_value %6#0 : $Error
  //   strong_release %6#0 : $Error

  StoreInst *SingleStore = nullptr;
  StrongReleaseInst *SingleRelease = nullptr;
  ProjectExistentialBoxInst *SingleProjection = nullptr;

  // For each user U of the alloc_existential_box...
  for (auto U : getNonDebugUses(AEBI)) {

    if (auto *PEBI = dyn_cast<ProjectExistentialBoxInst>(U->getUser())) {
      if (SingleProjection) return nullptr;
      SingleProjection = PEBI;
      for (auto AddrUse : getNonDebugUses(PEBI)) {
        // Record stores into the box.
        if (auto *SI = dyn_cast<StoreInst>(AddrUse->getUser())) {
          // If this is not the only store into the box then bail out.
          if (SingleStore) return nullptr;
          SingleStore = SI;
          continue;
        }
        // If there are other users to the box value address then bail out.
        return nullptr;
      }
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
    assert(SingleProjection && "store without a projection");
    // Release the value that was stored into the existential box. The box
    // is going away so we need to release the stored value now.
    Builder.setInsertionPoint(SingleStore);
    Builder.createReleaseValue(AEBI->getLoc(), SingleStore->getSrc(),
                               Atomicity::Atomic);

    // Erase the instruction that stores into the box and the release that
    // releases the box, and finally, release the box.
    eraseInstFromFunction(*SingleRelease);
    eraseInstFromFunction(*SingleStore);
    eraseInstFromFunction(*SingleProjection);
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
  SILType Ty = SEAI->getOperand()->getType();
  if (!Ty.isLoadable(SEAI->getModule()))
    return nullptr;

  SmallVector<std::pair<EnumElementDecl*, SILBasicBlock*>, 8> Cases;
  for (int i = 0, e = SEAI->getNumCases(); i < e; ++i)
    Cases.push_back(SEAI->getCase(i));

  Builder.setCurrentDebugScope(SEAI->getDebugScope());
  SILBasicBlock *Default = SEAI->hasDefault() ? SEAI->getDefaultBB() : nullptr;
  LoadInst *EnumVal = Builder.createLoad(SEAI->getLoc(), SEAI->getOperand(),
                                         LoadOwnershipQualifier::Unqualified);
  Builder.createSwitchEnum(SEAI->getLoc(), EnumVal, Default, Cases);
  return eraseInstFromFunction(*SEAI);
}

SILInstruction *SILCombiner::visitSelectEnumAddrInst(SelectEnumAddrInst *SEAI) {
  // Canonicalize a select_enum_addr: if the default refers to exactly one case,
  // then replace the default with that case.
  Builder.setCurrentDebugScope(SEAI->getDebugScope());
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
  SILType Ty = SEAI->getEnumOperand()->getType();
  if (!Ty.isLoadable(SEAI->getModule()))
    return nullptr;

  SmallVector<std::pair<EnumElementDecl*, SILValue>, 8> Cases;
  for (int i = 0, e = SEAI->getNumCases(); i < e; ++i)
    Cases.push_back(SEAI->getCase(i));

  SILValue Default = SEAI->hasDefault() ? SEAI->getDefaultResult() : SILValue();
  LoadInst *EnumVal = Builder.createLoad(SEAI->getLoc(), SEAI->getEnumOperand(),
                                         LoadOwnershipQualifier::Unqualified);
  auto *I = Builder.createSelectEnum(SEAI->getLoc(), EnumVal, SEAI->getType(),
                                     Default, Cases);
  return I;
}

SILInstruction *SILCombiner::visitSelectValueInst(SelectValueInst *SVI) {
  return nullptr;
}

SILInstruction *SILCombiner::visitSwitchValueInst(SwitchValueInst *SVI) {
  return nullptr;
}

namespace {

/// A SILInstruction visitor that analyzes alloc stack values for dead live
/// range and promotion opportunities.
///
/// init_existential_addr instructions behave like memory allocation within the
/// allocated object. We can promote the init_existential_addr allocation into a
/// dedicated allocation.
///
/// We detect this pattern
/// %0 = alloc_stack $LogicValue
/// %1 = init_existential_addr %0 : $*LogicValue, $*Bool
/// ...
/// use of %1
/// ...
/// destroy_addr %0 : $*LogicValue
/// dealloc_stack %0 : $*LogicValue
///
/// At the same we time also look for dead alloc_stack live ranges that are only
/// copied into.
///
/// %0 = alloc_stack
/// copy_addr %src, %0
/// destroy_addr %0 : $*LogicValue
/// dealloc_stack %0 : $*LogicValue
struct AllocStackAnalyzer : SILInstructionVisitor<AllocStackAnalyzer> {
  /// The alloc_stack that we are analyzing.
  AllocStackInst *ASI;

  /// Do all of the users of the alloc stack allow us to perform optimizations.
  bool LegalUsers = true;

  /// If we saw an init_existential_addr in the use list of the alloc_stack,
  /// this is the init_existential_addr. We are conservative in the face of
  /// having multiple init_existential_addr. In such a case, we say that the use
  /// list of the alloc_stack does not allow for optimizations to occur.
  InitExistentialAddrInst *IEI = nullptr;

  /// If we saw an open_existential_addr in the use list of the alloc_stack,
  /// this is the open_existential_addr. We are conservative in the case of
  /// multiple open_existential_addr. In such a case, we say that the use list
  /// of the alloc_stack does not allow for optimizations to occur.
  OpenExistentialAddrInst *OEI = nullptr;

  /// Did we see any copies into the alloc stack.
  bool HaveSeenCopyInto = false;

public:
  AllocStackAnalyzer(AllocStackInst *ASI) : ASI(ASI) {}

  /// Analyze the alloc_stack instruction's uses.
  void analyze() {
    // Scan all of the uses of the AllocStack and check if it is not used for
    // anything other than the init_existential_addr/open_existential_addr
    // container.

    for (auto *Op : getNonDebugUses(ASI)) {
      visit(Op->getUser());

      // If we found a non-legal user, bail early.
      if (!LegalUsers)
        break;
    }
  }

  /// Given an unhandled case, we have an illegal use for our optimization
  /// purposes. Set LegalUsers to false and return.
  void visitSILInstruction(SILInstruction *I) { LegalUsers = false; }

  // Destroy and dealloc are both fine.
  void visitDestroyAddrInst(DestroyAddrInst *I) {}
  void visitDeinitExistentialAddrInst(DeinitExistentialAddrInst *I) {}
  void visitDeallocStackInst(DeallocStackInst *I) {}

  void visitInitExistentialAddrInst(InitExistentialAddrInst *I) {
    // If we have already seen an init_existential_addr, we cannot
    // optimize. This is because we only handle the single init_existential_addr
    // case.
    if (IEI || HaveSeenCopyInto) {
      LegalUsers = false;
      return;
    }
    IEI = I;
  }

  void visitOpenExistentialAddrInst(OpenExistentialAddrInst *I) {
    // If we have already seen an open_existential_addr, we cannot
    // optimize. This is because we only handle the single open_existential_addr
    // case.
    if (OEI) {
      LegalUsers = false;
      return;
    }

    // Make sure that the open_existential does not have any uses except
    // destroy_addr.
    for (auto *Use : getNonDebugUses(I)) {
      if (!isa<DestroyAddrInst>(Use->getUser())) {
        LegalUsers = false;
        return;
      }
    }

    OEI = I;
  }

  void visitCopyAddrInst(CopyAddrInst *I) {
    if (IEI) {
      LegalUsers = false;
      return;
    }

    // Copies into the alloc_stack live range are safe.
    if (I->getDest() == ASI) {
      HaveSeenCopyInto = true;
      return;
    }

    LegalUsers = false;
  }
};

} // end anonymous namespace

SILInstruction *SILCombiner::visitAllocStackInst(AllocStackInst *AS) {
  AllocStackAnalyzer Analyzer(AS);
  Analyzer.analyze();

  // If when analyzing, we found a user that makes our optimization, illegal,
  // bail early.
  if (!Analyzer.LegalUsers)
    return nullptr;

  InitExistentialAddrInst *IEI = Analyzer.IEI;
  OpenExistentialAddrInst *OEI = Analyzer.OEI;

  // If the only users of the alloc_stack are alloc, destroy and
  // init_existential_addr then we can promote the allocation of the init
  // existential.
  // Be careful with open archetypes, because they cannot be moved before
  // their definitions.
  if (IEI && !OEI &&
      !IEI->getLoweredConcreteType()
           .getSwiftRValueType()
           ->isOpenedExistential()) {
    auto *ConcAlloc = Builder.createAllocStack(
        AS->getLoc(), IEI->getLoweredConcreteType(), AS->getVarInfo());
    IEI->replaceAllUsesWith(ConcAlloc);
    eraseInstFromFunction(*IEI);

    for (auto UI = AS->use_begin(), UE = AS->use_end(); UI != UE;) {
      auto *Op = *UI;
      ++UI;
      if (auto *DA = dyn_cast<DestroyAddrInst>(Op->getUser())) {
        Builder.setInsertionPoint(DA);
        Builder.createDestroyAddr(DA->getLoc(), ConcAlloc);
        eraseInstFromFunction(*DA);
        continue;
      }

      if (!isa<DeallocStackInst>(Op->getUser()))
        continue;

      auto *DS = cast<DeallocStackInst>(Op->getUser());
      Builder.setInsertionPoint(DS);
      Builder.createDeallocStack(DS->getLoc(), ConcAlloc);
      eraseInstFromFunction(*DS);
    }

    return eraseInstFromFunction(*AS);
  }

  // If we have a live 'live range' or a live range that we have not sen a copy
  // into, bail.
  if (!Analyzer.HaveSeenCopyInto || IEI)
    return nullptr;

  // Otherwise remove the dead live range that is only copied into.
  //
  // TODO: Do we not remove purely dead live ranges here? Seems like we should.
  SmallPtrSet<SILInstruction *, 16> ToDelete;

  for (auto *Op : AS->getUses()) {
    // Replace a copy_addr [take] %src ... by a destroy_addr %src if %src is
    // no the alloc_stack.
    // Otherwise, just delete the copy_addr.
    if (auto *CopyAddr = dyn_cast<CopyAddrInst>(Op->getUser())) {
      if (CopyAddr->isTakeOfSrc() && CopyAddr->getSrc() != AS) {
        Builder.setInsertionPoint(CopyAddr);
        Builder.createDestroyAddr(CopyAddr->getLoc(), CopyAddr->getSrc());
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
  return eraseInstFromFunction(*AS);
}

SILInstruction *SILCombiner::visitLoadInst(LoadInst *LI) {
  // (load (upcast-ptr %x)) -> (upcast-ref (load %x))
  Builder.setCurrentDebugScope(LI->getDebugScope());
  if (auto *UI = dyn_cast<UpcastInst>(LI->getOperand())) {
    auto NewLI = Builder.createLoad(LI->getLoc(), UI->getOperand(),
                                    LoadOwnershipQualifier::Unqualified);
    return Builder.createUpcast(LI->getLoc(), NewLI, LI->getType());
  }

  // Given a load with multiple struct_extracts/tuple_extracts and no other
  // uses, canonicalize the load into several (struct_element_addr (load))
  // pairs.
  using ProjInstPairTy = std::pair<Projection, SILInstruction *>;

  // Go through the loads uses and add any users that are projections to the
  // projection list.
  llvm::SmallVector<ProjInstPairTy, 8> Projections;
  for (auto *UI : getNonDebugUses(LI)) {
    auto *User = UI->getUser();

    // If we have any non SEI, TEI instruction, don't do anything here.
    if (!isa<StructExtractInst>(User) && !isa<TupleExtractInst>(User))
      return nullptr;

    Projections.push_back({Projection(User), User});
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
      replaceInstUsesWith(*Inst, LastNewLoad);
      eraseInstFromFunction(*Inst);
      continue;
    }

    // Ok, we have started to visit the range of instructions associated with
    // a new projection. Create the new address projection.
    auto I = Proj.createAddressProjection(Builder, LI->getLoc(), LI->getOperand());
    LastProj = &Proj;
    LastNewLoad = Builder.createLoad(LI->getLoc(), I.get(),
                                     LoadOwnershipQualifier::Unqualified);
    replaceInstUsesWith(*Inst, LastNewLoad);
    eraseInstFromFunction(*Inst);
  }

  // Erase the old load.
  return eraseInstFromFunction(*LI);
}

SILInstruction *SILCombiner::visitReleaseValueInst(ReleaseValueInst *RVI) {
  SILValue Operand = RVI->getOperand();
  SILType OperandTy = Operand->getType();

  // Destroy value of an enum with a trivial payload or no-payload is a no-op.
  if (auto *EI = dyn_cast<EnumInst>(Operand)) {
    if (!EI->hasOperand() ||
        EI->getOperand()->getType().isTrivial(EI->getModule()))
      return eraseInstFromFunction(*RVI);

    // retain_value of an enum_inst where we know that it has a payload can be
    // reduced to a retain_value on the payload.
    if (EI->hasOperand()) {
      return Builder.createReleaseValue(RVI->getLoc(), EI->getOperand(),
                                        Atomicity::Atomic);
    }
  }

  // ReleaseValueInst of an unowned type is an unowned_release.
  if (OperandTy.is<UnownedStorageType>())
    return Builder.createUnownedRelease(RVI->getLoc(), Operand,
                                        Atomicity::Atomic);

  // ReleaseValueInst of a reference type is a strong_release.
  if (OperandTy.isReferenceCounted(RVI->getModule()))
    return Builder.createStrongRelease(RVI->getLoc(), Operand,
                                       Atomicity::Atomic);

  // ReleaseValueInst of a trivial type is a no-op.
  if (OperandTy.isTrivial(RVI->getModule()))
    return eraseInstFromFunction(*RVI);

  // Do nothing for non-trivial non-reference types.
  return nullptr;
}

SILInstruction *SILCombiner::visitRetainValueInst(RetainValueInst *RVI) {
  SILValue Operand = RVI->getOperand();
  SILType OperandTy = Operand->getType();

  // retain_value of an enum with a trivial payload or no-payload is a no-op +
  // RAUW.
  if (auto *EI = dyn_cast<EnumInst>(Operand)) {
    if (!EI->hasOperand() ||
        EI->getOperand()->getType().isTrivial(RVI->getModule())) {
      return eraseInstFromFunction(*RVI);
    }

    // retain_value of an enum_inst where we know that it has a payload can be
    // reduced to a retain_value on the payload.
    if (EI->hasOperand()) {
      return Builder.createRetainValue(RVI->getLoc(), EI->getOperand(),
                                       Atomicity::Atomic);
    }
  }

  // RetainValueInst of an unowned type is an unowned_retain.
  if (OperandTy.is<UnownedStorageType>())
    return Builder.createUnownedRetain(RVI->getLoc(), Operand,
                                       Atomicity::Atomic);

  // RetainValueInst of a reference type is a strong_release.
  if (OperandTy.isReferenceCounted(RVI->getModule())) {
    return Builder.createStrongRetain(RVI->getLoc(), Operand,
                                      Atomicity::Atomic);
  }

  // RetainValueInst of a trivial type is a no-op + use propagation.
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
  // it). But that does not mean that we cannot eliminate this pair with a
  // peephole.

  // If we are not the first instruction in this basic block...
  if (RVI != &*RVI->getParent()->begin()) {
    SILBasicBlock::iterator Pred = std::prev(RVI->getIterator());

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
  // it). But that does not mean that we cannot eliminate this pair with a
  // peephole.

  // If we are not the first instruction in this basic block...
  if (SRI != &*SRI->getParent()->begin()) {
    auto Pred = std::prev(SRI->getIterator());

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

  assert(IEAI->getOperand()->getType().isAddress() && "Must be an address");
  Builder.setCurrentDebugScope(IEAI->getDebugScope());

  if (IEAI->getOperand()->getType().isAddressOnly(IEAI->getModule())) {
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
      auto BeforeTerm = std::prev(std::prev(IEAI->getParent()->end()));
      auto *SEAI = dyn_cast<SelectEnumAddrInst>(BeforeTerm);
      if (!SEAI)
        return nullptr;

      if (SEAI->getOperand() != IEAI->getOperand())
        return nullptr;

      SILBasicBlock::iterator II = IEAI->getIterator();
      StoreInst *SI = nullptr;
      for (;;) {
        SILInstruction *CI = &*II;
        if (CI == SEAI)
          break;
        ++II;
        SI = dyn_cast<StoreInst>(CI);
        if (SI) {
          if (SI->getDest() == IEAI->getOperand())
            return nullptr;
        }
        // Allow all instructions in between, which don't have any dependency to
        // the store.
        if (AA->mayWriteToMemory(&*II, IEAI->getOperand()))
          return nullptr;
      }

      auto *InjectedEnumElement = IEAI->getElement();
      auto Result = SEAI->getCaseResult(InjectedEnumElement);

      // Replace select_enum_addr by the result
      replaceInstUsesWith(*SEAI, Result);
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

      SILBasicBlock::iterator II = IEAI->getIterator();
      StoreInst *SI = nullptr;
      for (;;) {
        SILInstruction *CI = &*II;
        if (CI == SEI)
          break;
        ++II;
        SI = dyn_cast<StoreInst>(CI);
        if (SI) {
          if (SI->getDest() == IEAI->getOperand())
            return nullptr;
        }
        // Allow all instructions in between, which don't have any dependency to
        // the store.
        if (AA->mayWriteToMemory(&*II, IEAI->getOperand()))
          return nullptr;
      }

      // Replace switch_enum_addr by a branch instruction.
      SILBuilderWithScope B(SEI);
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
        auto *IL = B.createIntegerLiteral(
          SEI->getLoc(), IntTy,
          APInt(32, static_cast<uint64_t>(SEI->getNumCases()), false));
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
  if (!IEAI->getElement()->getArgumentInterfaceType()) {
    EnumInst *E =
      Builder.createEnum(IEAI->getLoc(), SILValue(), IEAI->getElement(),
                          IEAI->getOperand()->getType().getObjectType());
    Builder.createStore(IEAI->getLoc(), E, IEAI->getOperand(),
                        StoreOwnershipQualifier::Unqualified);
    return eraseInstFromFunction(*IEAI);
  }

  // Ok, we have a payload enum, make sure that we have a store previous to
  // us...
  SILValue ASO = IEAI->getOperand();
  if (!isa<AllocStackInst>(ASO)) {
    return nullptr;
  }
  InitEnumDataAddrInst *DataAddrInst = nullptr;
  InjectEnumAddrInst *EnumAddrIns = nullptr;
  llvm::SmallPtrSet<SILInstruction *, 32> WriteSet;
  for (auto UsersIt : ASO->getUses()) {
    SILInstruction *CurrUser = UsersIt->getUser();
    if (CurrUser->isDeallocatingStack()) {
      // we don't care about the dealloc stack instructions
      continue;
    }
    if (isDebugInst(CurrUser) || isa<LoadInst>(CurrUser)) {
      // These Instructions are a non-risky use we can ignore
      continue;
    }
    if (auto *CurrInst = dyn_cast<InitEnumDataAddrInst>(CurrUser)) {
      if (DataAddrInst) {
        return nullptr;
      }
      DataAddrInst = CurrInst;
      continue;
    }
    if (auto *CurrInst = dyn_cast<InjectEnumAddrInst>(CurrUser)) {
      if (EnumAddrIns) {
        return nullptr;
      }
      EnumAddrIns = CurrInst;
      continue;
    }
    if (isa<StoreInst>(CurrUser)) {
      // The only MayWrite Instruction we can safely handle
      WriteSet.insert(CurrUser);
      continue;
    }
    // It is too risky to continue if it is any other instruction.
    return nullptr;
  }

  if (!DataAddrInst || !EnumAddrIns) {
    return nullptr;
  }
  assert((EnumAddrIns == IEAI) &&
         "Found InitEnumDataAddrInst differs from IEAI");
  // Found the DataAddrInst to this enum payload. Check if it has only use.
  if (!hasOneNonDebugUse(DataAddrInst))
    return nullptr;

  StoreInst *SI = dyn_cast<StoreInst>(getSingleNonDebugUser(DataAddrInst));
  ApplyInst *AI = dyn_cast<ApplyInst>(getSingleNonDebugUser(DataAddrInst));
  if (!SI && !AI) {
    return nullptr;
  }

  // Make sure the enum pattern instructions are the only ones which write to
  // this location
  if (!WriteSet.empty()) {
    // Analyze the instructions (implicit dominator analysis)
    // If we find any of MayWriteSet, return nullptr
    SILBasicBlock *InitEnumBB = DataAddrInst->getParent();
    assert(InitEnumBB && "DataAddrInst is not in a valid Basic Block");
    llvm::SmallVector<SILInstruction *, 64> Worklist;
    Worklist.push_back(IEAI);
    llvm::SmallPtrSet<SILBasicBlock *, 16> Preds;
    Preds.insert(IEAI->getParent());
    while (!Worklist.empty()) {
      SILInstruction *CurrIns = Worklist.pop_back_val();
      SILBasicBlock *CurrBB = CurrIns->getParent();

      if (CurrBB->isEntry() && CurrBB != InitEnumBB) {
        // reached prologue without encountering the init bb
        return nullptr;
      }

      for (auto InsIt = ++CurrIns->getIterator().getReverse();
           InsIt != CurrBB->rend(); ++InsIt) {
        SILInstruction *Ins = &*InsIt;
        if (Ins == DataAddrInst) {
          // don't care about what comes before init enum in the basic block
          break;
        }
        if (WriteSet.count(Ins) != 0) {
          return nullptr;
        }
      }

      if (CurrBB == InitEnumBB) {
        continue;
      }

      // Go to predecessors and do all that again
      for (SILBasicBlock *Pred : CurrBB->getPredecessorBlocks()) {
        // If it's already in the set, then we've already queued and/or
        // processed the predecessors.
        if (Preds.insert(Pred).second) {
          Worklist.push_back(&*Pred->rbegin());
        }
      }
    }
  }

  if (SI) {
    assert((SI->getDest() == DataAddrInst) &&
           "Can't find StoreInst with DataAddrInst as its destination");
    // In that case, create the payload enum/store.
    EnumInst *E = Builder.createEnum(
        DataAddrInst->getLoc(), SI->getSrc(), DataAddrInst->getElement(),
        DataAddrInst->getOperand()->getType().getObjectType());
    Builder.createStore(DataAddrInst->getLoc(), E, DataAddrInst->getOperand(),
                        StoreOwnershipQualifier::Unqualified);
    // Cleanup.
    eraseInstFromFunction(*SI);
    eraseInstFromFunction(*DataAddrInst);
    return eraseInstFromFunction(*IEAI);
  }

  // Check whether we have an apply initializing the enum.
  //  %iedai = init_enum_data_addr %enum_addr
  //         = apply(%iedai,...)
  //  inject_enum_addr %enum_addr
  //
  // We can localize the store to an alloc_stack.
  // Allowing us to perform the same optimization as for the store.
  //
  //  %alloca = alloc_stack
  //            apply(%alloca,...)
  //  %load = load %alloca
  //  %1 = enum $EnumType, $EnumType.case, %load
  //  store %1 to %nopayload_addr
  //
  assert(AI && "Must have an apply");
  unsigned ArgIdx = 0;
  Operand *EnumInitOperand = nullptr;
  for (auto &Opd : AI->getArgumentOperands()) {
    // Found an apply that initializes the enum. We can optimize this by
    // localizing the initialization to an alloc_stack and loading from it.
    DataAddrInst = dyn_cast<InitEnumDataAddrInst>(Opd.get());
    if (DataAddrInst && DataAddrInst->getOperand() == IEAI->getOperand()
        && ArgIdx < AI->getSubstCalleeConv().getNumIndirectSILResults()) {
      EnumInitOperand = &Opd;
      break;
    }
    ++ArgIdx;
  }

  if (!EnumInitOperand) {
    return nullptr;
  }

  // Localize the address access.
  Builder.setInsertionPoint(AI);
  auto *AllocStack = Builder.createAllocStack(DataAddrInst->getLoc(),
                                              EnumInitOperand->get()->getType());
  EnumInitOperand->set(AllocStack);
  Builder.setInsertionPoint(std::next(SILBasicBlock::iterator(AI)));
  SILValue Load(Builder.createLoad(DataAddrInst->getLoc(), AllocStack,
                                   LoadOwnershipQualifier::Unqualified));
  EnumInst *E = Builder.createEnum(
      DataAddrInst->getLoc(), Load, DataAddrInst->getElement(),
      DataAddrInst->getOperand()->getType().getObjectType());
  Builder.createStore(DataAddrInst->getLoc(), E, DataAddrInst->getOperand(),
                      StoreOwnershipQualifier::Unqualified);
  Builder.createDeallocStack(DataAddrInst->getLoc(), AllocStack);
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

  // If our enum type is address only, we cannot do anything here. The key
  // thing to remember is that an enum is address only if any of its cases are
  // address only. So we *could* have a loadable payload resulting from the
  // TEDAI without the TEDAI being loadable itself.
  if (TEDAI->getOperand()->getType().isAddressOnly(TEDAI->getModule()))
    return nullptr;

  // For each user U of the take_enum_data_addr...
  for (auto U : getNonDebugUses(TEDAI))
    // Check if it is load. If it is not a load, bail...
    if (!isa<LoadInst>(U->getUser()))
      return nullptr;

  // Grab the EnumAddr.
  SILLocation Loc = TEDAI->getLoc();
  Builder.setCurrentDebugScope(TEDAI->getDebugScope());
  SILValue EnumAddr = TEDAI->getOperand();
  EnumElementDecl *EnumElt = TEDAI->getElement();
  SILType PayloadType = TEDAI->getType().getObjectType();

  // Go back through a second time now that we know all of our users are
  // loads. Perform the transformation on each load.
  SmallVector<LoadInst*, 4> ToRemove;
  for (auto U : getNonDebugUses(TEDAI)) {
    // Grab the load.
    LoadInst *L = cast<LoadInst>(U->getUser());

    // Insert a new Load of the enum and extract the data from that.
    auto *Ld =
        Builder.createLoad(Loc, EnumAddr, LoadOwnershipQualifier::Unqualified);
    auto *D = Builder.createUncheckedEnumData(Loc, Ld, EnumElt, PayloadType);

    // Replace all uses of the old load with the data and erase the old load.
    replaceInstUsesWith(*L, D);
    ToRemove.push_back(L);
  }

  for (auto *LD : ToRemove) {
    eraseInstFromFunction(*LD);
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
    auto EnumOperandTy = SEI->getEnumOperand()->getType();
    // Type should be loadable
    if (!EnumOperandTy.isLoadable(SEI->getModule()))
      return nullptr;

    // Result of the select_enum should be a boolean.
    if (SEI->getType() != CBI->getCondition()->getType())
      return nullptr;

    // If any of cond_br edges are critical edges, do not perform
    // the transformation, as SIL in canonical form may
    // only have critical edges that are originating from cond_br
    // instructions.
    if (!CBI->getTrueBB()->getSinglePredecessorBlock())
      return nullptr;

    if (!CBI->getFalseBB()->getSinglePredecessorBlock())
      return nullptr;

    SILBasicBlock *DefaultBB = nullptr;
    match_integer<0> Zero;

    if (SEI->hasDefault()) {
      // Default result should be an integer constant.
      if (!isa<IntegerLiteralInst>(SEI->getDefaultResult()))
        return nullptr;
      bool isFalse = match(SEI->getDefaultResult(), Zero);
      // Pick the default BB.
      DefaultBB = isFalse ? CBI->getFalseBB() : CBI->getTrueBB();
    }

    if (!DefaultBB) {
      // Find the targets for the majority of cases and pick it
      // as a default BB.
      unsigned TrueBBCases = 0;
      unsigned FalseBBCases = 0;
      for (int i = 0, e = SEI->getNumCases(); i < e; ++i) {
        auto Pair = SEI->getCase(i);
        if (isa<IntegerLiteralInst>(Pair.second)) {
          bool isFalse = match(Pair.second, Zero);
          if (!isFalse) {
            TrueBBCases++;
          } else {
            FalseBBCases++;
          }
          continue;
        }
        return nullptr;
      }

      if (FalseBBCases > TrueBBCases)
        DefaultBB = CBI->getFalseBB();
      else
        DefaultBB = CBI->getTrueBB();
    }

    assert(DefaultBB && "Default should be defined at this point");

    unsigned NumTrueBBCases = 0;
    unsigned NumFalseBBCases = 0;

    if (DefaultBB == CBI->getFalseBB())
      NumFalseBBCases++;
    else
      NumTrueBBCases++;

    // We can now convert cond_br(select_enum) into switch_enum.
    SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 8> Cases;
    for (int i = 0, e = SEI->getNumCases(); i < e; ++i) {
      auto Pair = SEI->getCase(i);

      // Bail if one of the results is not an integer constant.
      if (!isa<IntegerLiteralInst>(Pair.second))
        return nullptr;

      // Add a switch case.
      bool isFalse = match(Pair.second, Zero);
      if (!isFalse && DefaultBB != CBI->getTrueBB()) {
        Cases.push_back(std::make_pair(Pair.first, CBI->getTrueBB()));
        NumTrueBBCases++;
      }
      if (isFalse && DefaultBB != CBI->getFalseBB()) {
        Cases.push_back(std::make_pair(Pair.first, CBI->getFalseBB()));
        NumFalseBBCases++;
      }
    }

    // Bail if a switch_enum would introduce a critical edge.
    if (NumTrueBBCases > 1 || NumFalseBBCases > 1)
      return nullptr;

    return Builder.createSwitchEnum(SEI->getLoc(), SEI->getEnumOperand(),
                                    DefaultBB, Cases);
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

  Builder.setCurrentDebugScope(TEI->getDebugScope());
  if (auto *BI = dyn_cast<BuiltinInst>(TEI->getOperand()))
    if (!canOverflow(BI))
      return Builder.createIntegerLiteral(TEI->getLoc(), TEI->getType(),
                                          APInt(1, 0));
  return nullptr;
}

SILInstruction *SILCombiner::visitFixLifetimeInst(FixLifetimeInst *FLI) {
  // fix_lifetime(alloc_stack) -> fix_lifetime(load(alloc_stack))
  Builder.setCurrentDebugScope(FLI->getDebugScope());
  if (auto *AI = dyn_cast<AllocStackInst>(FLI->getOperand())) {
    if (FLI->getOperand()->getType().isLoadable(FLI->getModule())) {
      auto Load = Builder.createLoad(FLI->getLoc(), AI,
                                     LoadOwnershipQualifier::Unqualified);
      return Builder.createFixLifetime(FLI->getLoc(), Load);
    }
  }
  return nullptr;
}

SILInstruction *
SILCombiner::
visitAllocRefDynamicInst(AllocRefDynamicInst *ARDI) {
  SmallVector<SILValue, 4> Counts;
  auto getCounts = [&] (AllocRefDynamicInst *AI) -> ArrayRef<SILValue> {
    for (Operand &Op : AI->getTailAllocatedCounts()) {
      Counts.push_back(Op.get());
    }
    return Counts;
  };

  // %1 = metatype $X.Type
  // %2 = alloc_ref_dynamic %1 : $X.Type, Y
  // ->
  // alloc_ref X
  Builder.setCurrentDebugScope(ARDI->getDebugScope());

  SILValue MDVal = ARDI->getMetatypeOperand();
  if (auto *UC = dyn_cast<UpcastInst>(MDVal))
    MDVal = UC->getOperand();

  SILInstruction *NewInst = nullptr;
  if (MetatypeInst *MI = dyn_cast<MetatypeInst>(MDVal)) {
    auto &Mod = ARDI->getModule();
    auto SILInstanceTy = MI->getType().getMetatypeInstanceType(Mod);

    NewInst = Builder.createAllocRef(ARDI->getLoc(), SILInstanceTy,
                                     ARDI->isObjC(), false,
                                     ARDI->getTailAllocatedTypes(),
                                     getCounts(ARDI));

  } else if (isa<SILArgument>(MDVal)) {

    // checked_cast_br [exact] $Y.Type to $X.Type, bbSuccess, bbFailure
    // ...
    // bbSuccess(%T: $X.Type)
    // alloc_ref_dynamic %T : $X.Type, $X
    // ->
    // alloc_ref $X
    auto *PredBB = ARDI->getParent()->getSinglePredecessorBlock();
    if (!PredBB)
      return nullptr;
    auto *CCBI = dyn_cast<CheckedCastBranchInst>(PredBB->getTerminator());
    if (CCBI && CCBI->isExact() && ARDI->getParent() == CCBI->getSuccessBB()) {
      auto &Mod = ARDI->getModule();
      auto SILInstanceTy = CCBI->getCastType().getMetatypeInstanceType(Mod);
      NewInst = Builder.createAllocRef(ARDI->getLoc(), SILInstanceTy,
                                       ARDI->isObjC(), false,
                                       ARDI->getTailAllocatedTypes(),
                                       getCounts(ARDI));
    }
  }
  if (NewInst && NewInst->getType() != ARDI->getType()) {
    // In case the argument was an upcast of the metatype, we have to upcast the
    // resulting reference.
    NewInst = Builder.createUpcast(ARDI->getLoc(), NewInst, ARDI->getType());
  }
  return NewInst;
}

SILInstruction *SILCombiner::visitEnumInst(EnumInst *EI) {
  return nullptr;
}

