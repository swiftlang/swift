//===-------------------------- SILCombiner.h -----------------*- C++ -*---===//
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
//
// A port of LLVM's InstCombiner to SIL. Its main purpose is for performing
// small combining operations/peepholes at the SIL level. It additionally
// performs dead code elimination when it initially adds instructions to the
// work queue in order to reduce compile time by not visiting trivially dead
// instructions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILPASSES_SILCOMBINER_H
#define SWIFT_SILPASSES_SILCOMBINER_H

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class AliasAnalysis;

/// This is the worklist management logic for SILCombine.
class SILCombineWorklist {
  llvm::SmallVector<SILInstruction *, 256> Worklist;
  llvm::DenseMap<SILInstruction *, unsigned> WorklistMap;
  llvm::SmallVector<SILInstruction *, 8> TrackingList;

  void operator=(const SILCombineWorklist &RHS) = delete;
  SILCombineWorklist(const SILCombineWorklist &Worklist) = delete;
public:
  SILCombineWorklist() {}

  /// Returns true if the worklist is empty.
  bool isEmpty() const { return Worklist.empty(); }

  /// Add the specified instruction to the worklist if it isn't already in it.
  void add(SILInstruction *I);

  /// If the given ValueBase is a SILInstruction add it to the worklist.
  void addValue(ValueBase *V) {
    auto *I = dyn_cast<SILInstruction>(V);
    if (!I)
      return;
    add(I);
  }

  /// Add the given list of instructions in reverse order to the worklist. This
  /// routine assumes that the worklist is empty and the given list has no
  /// duplicates.
  void addInitialGroup(ArrayRef<SILInstruction *> List);

  // If I is in the worklist, remove it.
  void remove(SILInstruction *I) {
    auto It = WorklistMap.find(I);
    if (It == WorklistMap.end())
      return; // Not in worklist.

    // Don't bother moving everything down, just null out the slot. We will
    // check before we process any instruction if it is null.
    Worklist[It->second] = nullptr;
    WorklistMap.erase(It);
  }

  /// Remove the top element from the worklist.
  SILInstruction *removeOne() {
    SILInstruction *I = Worklist.pop_back_val();
    WorklistMap.erase(I);
    return I;
  }

  /// When an instruction has been simplified, add all of its users to the
  /// worklist since additional simplifications of its users may have been
  /// exposed.
  void addUsersToWorklist(ValueBase *I) {
    for (auto UI : I->getUses())
      add(UI->getUser());
  }

  /// If only one result of an instruction has been simplified, add all of the
  /// users of that result to the worklist since additional simplifications of
  /// its users may have been exposed.
  void addUsersToWorklist(ValueBase *I, unsigned Index) {
    for (auto UI : SILValue(I, Index).getUses())
      add(UI->getUser());
  }

  /// Check that the worklist is empty and nuke the backing store for the map if
  /// it is large.
  void zap() {
    assert(WorklistMap.empty() && "Worklist empty, but the map is not?");

    // Do an explicit clear, this shrinks the map if needed.
    WorklistMap.clear();
  }
};

/// This is a class which maintains the state of the combiner and simplifies
/// many operations such as removing/adding instructions and syncing them with
/// the worklist.
class SILCombiner :
    public SILInstructionVisitor<SILCombiner, SILInstruction *> {

  AliasAnalysis *AA;

  /// Worklist containing all of the instructions primed for simplification.
  SILCombineWorklist Worklist;

  /// Variable to track if the SILCombiner made any changes.
  bool MadeChange;

  /// If set to true then the optimizer is free to erase cond_fail instructions.
  bool RemoveCondFails;

  /// The current iteration of the SILCombine.
  unsigned Iteration;

  /// Builder used to insert instructions.
  SILBuilder *Builder;

  /// A list that the builder inserts newly created instructions into. Its
  /// contents are added to the worklist after every iteration and then the list
  /// is cleared.
  llvm::SmallVector<SILInstruction *, 64> TrackingList;

public:
  SILCombiner(AliasAnalysis *AA, bool removeCondFails)
    : AA(AA), Worklist(), MadeChange(false), RemoveCondFails(removeCondFails),
      Iteration(0), Builder(0) { }

  bool runOnFunction(SILFunction &F);

  void clear() {
    Iteration = 0;
    Worklist.zap();
    MadeChange = false;
  }

  // Insert the instruction New before instruction Old in Old's parent BB. Add
  // New to the worklist.
  SILInstruction *insertNewInstBefore(SILInstruction *New, SILInstruction &Old);

  // This method is to be used when an instruction is found to be dead,
  // replacable with another preexisting expression. Here we add all uses of I
  // to the worklist, replace all uses of I with the new value, then return I,
  // so that the combiner will know that I was modified.
  SILInstruction *replaceInstUsesWith(SILInstruction &I, ValueBase *V);

  /// This is meant to be used when one is attempting to replace only one of the
  /// results of I with a result of V.
  SILInstruction *replaceInstUsesWith(SILInstruction &I, ValueBase *V,
                                      unsigned IIndex, unsigned VIndex=0);

  // Some instructions can never be "trivially dead" due to side effects or
  // producing a void value. In those cases, since we can not rely on
  // SILCombines trivially dead instruction DCE in order to delete the
  // instruction, visit methods should use this method to delete the given
  // instruction and upon completion of their peephole return the value returned
  // by this method.
  SILInstruction *eraseInstFromFunction(SILInstruction &I);

  void addInitialGroup(ArrayRef<SILInstruction *> List) {
    Worklist.addInitialGroup(List);
  }

  /// Base visitor that does not do anything.
  SILInstruction *visitValueBase(ValueBase *V) { return nullptr; }

  /// Instruction visitors.
  SILInstruction *visitReleaseValueInst(ReleaseValueInst *DI);
  SILInstruction *visitRetainValueInst(RetainValueInst *CI);
  SILInstruction *visitPartialApplyInst(PartialApplyInst *AI);
  SILInstruction *visitApplyInst(ApplyInst *AI);
  SILInstruction *visitBuiltinInst(BuiltinInst *BI);
  SILInstruction *visitCondFailInst(CondFailInst *CFI);
  SILInstruction *visitStrongRetainInst(StrongRetainInst *SRI);
  SILInstruction *visitRefToRawPointerInst(RefToRawPointerInst *RRPI);
  SILInstruction *visitUpcastInst(UpcastInst *UCI);
  SILInstruction *visitLoadInst(LoadInst *LI);
  SILInstruction *visitAllocStackInst(AllocStackInst *AS);
  SILInstruction *visitSwitchEnumAddrInst(SwitchEnumAddrInst *SEAI);
  SILInstruction *visitInjectEnumAddrInst(InjectEnumAddrInst *IEAI);
  SILInstruction *visitPointerToAddressInst(PointerToAddressInst *PTAI);
  SILInstruction *visitUncheckedAddrCastInst(UncheckedAddrCastInst *UADCI);
  SILInstruction *visitUncheckedRefCastInst(UncheckedRefCastInst *URCI);
  SILInstruction *visitUnconditionalCheckedCastInst(
                    UnconditionalCheckedCastInst *UCCI);
  SILInstruction *visitRawPointerToRefInst(RawPointerToRefInst *RPTR);
  SILInstruction *
  visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *TEDAI);
  SILInstruction *visitStrongReleaseInst(StrongReleaseInst *SRI);
  SILInstruction *visitCondBranchInst(CondBranchInst *CBI);
  SILInstruction *
  visitUncheckedRefBitCastInst(UncheckedRefBitCastInst *URBCI);
  SILInstruction *
  visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *UTBCI);
  SILInstruction *visitSelectEnumInst(SelectEnumInst *EIT);
  SILInstruction *visitSelectEnumAddrInst(SelectEnumAddrInst *EIT);
  SILInstruction *visitStructExtractInst(StructExtractInst *SEI);
  SILInstruction *visitUncheckedEnumDataInst(UncheckedEnumDataInst *UEDI);
  SILInstruction *visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *TTOCMI);
  SILInstruction *visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *OCTTMI);
  SILInstruction *visitTupleExtractInst(TupleExtractInst *TEI);
  SILInstruction *visitFixLifetimeInst(FixLifetimeInst *FLI);
  SILInstruction *visitSwitchValueInst(SwitchValueInst *SVI);
  SILInstruction *visitSelectValueInst(SelectValueInst *SVI);
  SILInstruction *
  visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *CCABI);
  SILInstruction *
  visitCheckedCastBranchInst(CheckedCastBranchInst *CBI);
  SILInstruction * visitUnreachableInst(UnreachableInst *UI);


  /// Instruction visitor helpers.
  SILInstruction *optimizeBuiltinCanBeObjCClass(BuiltinInst *AI);

  // Optimize the "cmp_eq_XXX" builtin. If \p NegateResult is true then negate
  // the result bit.
  SILInstruction *optimizeBuiltinCompareEq(BuiltinInst *AI, bool NegateResult);

  SILInstruction *optimizeApplyOfPartialApply(ApplyInst *AI,
                                              PartialApplyInst *PAI);
  SILInstruction *optimizeApplyOfConvertFunctionInst(ApplyInst *AI,
                                                     ConvertFunctionInst *CFI);
  // Optimize concatenation of string literals.
  // Constant-fold concatenation of string literals known at compile-time.
  SILInstruction *optimizeConcatenationOfStringLiterals(ApplyInst *AI);
  SILInstruction *propagateConcreteTypeOfInitExistential(ApplyInst *AI,
                                                         WitnessMethodInst *WMI,
                                                         SILValue IE,
                                                         SILType InstanceType);

private:
  /// Perform one SILCombine iteration.
  bool doOneIteration(SILFunction &F, unsigned Iteration);
};

} // end namespace swift

#endif
