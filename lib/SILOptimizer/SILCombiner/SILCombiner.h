//===--- SILCombiner.h ------------------------------------------*- C++ -*-===//
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
// A port of LLVM's InstCombiner to SIL. Its main purpose is for performing
// small combining operations/peepholes at the SIL level. It additionally
// performs dead code elimination when it initially adds instructions to the
// work queue in order to reduce compile time by not visiting trivially dead
// instructions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_SILCOMBINER_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_SILCOMBINER_H

#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Analysis/ProtocolConformanceAnalysis.h"
#include "swift/SILOptimizer/Utils/CastOptimizer.h"
#include "swift/SILOptimizer/Utils/Existential.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class AliasAnalysis;

/// This is the worklist management logic for SILCombine.
class SILCombineWorklist {
  llvm::SmallVector<SILInstruction *, 256> Worklist;
  llvm::DenseMap<SILInstruction *, unsigned> WorklistMap;

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
    if (auto *I = V->getDefiningInstruction())
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
  /// worklist, since additional simplifications of its users may have been
  /// exposed.
  void addUsersToWorklist(ValueBase *I) {
    for (auto UI : I->getUses())
      add(UI->getUser());
  }

  /// When an instruction has been simplified, add all of its users to the
  /// worklist, since additional simplifications of its users may have been
  /// exposed.
  void addUsersOfAllResultsToWorklist(SILInstruction *I) {
    for (auto result : I->getResults()) {
      addUsersToWorklist(result);
    }
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

  DominanceAnalysis *DA;

  // Determine the set of types a protocol conforms to in whole-module
  // compilation mode.
  ProtocolConformanceAnalysis *PCA;

  // Class hierarchy analysis needed to confirm no derived classes of a sole
  // conforming class.
  ClassHierarchyAnalysis *CHA;

  /// Worklist containing all of the instructions primed for simplification.
  SILCombineWorklist Worklist;

  /// Variable to track if the SILCombiner made any changes.
  bool MadeChange;

  /// If set to true then the optimizer is free to erase cond_fail instructions.
  bool RemoveCondFails;

  /// The current iteration of the SILCombine.
  unsigned Iteration;

  /// Builder used to insert instructions.
  SILBuilder &Builder;

  /// Cast optimizer
  CastOptimizer CastOpt;

public:
  SILCombiner(SILOptFunctionBuilder &FuncBuilder,
              SILBuilder &B, AliasAnalysis *AA, DominanceAnalysis *DA,
              ProtocolConformanceAnalysis *PCA, ClassHierarchyAnalysis *CHA,
              bool removeCondFails)
      : AA(AA), DA(DA), PCA(PCA), CHA(CHA), Worklist(), MadeChange(false),
        RemoveCondFails(removeCondFails), Iteration(0), Builder(B),
        CastOpt(FuncBuilder,
                /* ReplaceInstUsesAction */
                [&](SingleValueInstruction *I, ValueBase *V) {
                  replaceInstUsesWith(*I, V);
                },
                /* EraseAction */
                [&](SILInstruction *I) { eraseInstFromFunction(*I); }) {}

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
  // replaceable with another preexisting expression. Here we add all uses of I
  // to the worklist, replace all uses of I with the new value, then return I,
  // so that the combiner will know that I was modified.
  void replaceInstUsesWith(SingleValueInstruction &I, ValueBase *V);

  void replaceInstUsesPairwiseWith(SILInstruction *oldI, SILInstruction *newI);

  // Some instructions can never be "trivially dead" due to side effects or
  // producing a void value. In those cases, since we cannot rely on
  // SILCombines trivially dead instruction DCE in order to delete the
  // instruction, visit methods should use this method to delete the given
  // instruction and upon completion of their peephole return the value returned
  // by this method.
  SILInstruction *eraseInstFromFunction(SILInstruction &I,
                                        SILBasicBlock::iterator &InstIter,
                                        bool AddOperandsToWorklist = true);

  SILInstruction *eraseInstFromFunction(SILInstruction &I,
                                        bool AddOperandsToWorklist = true) {
    SILBasicBlock::iterator nullIter;
    return eraseInstFromFunction(I, nullIter, AddOperandsToWorklist);
  }

  void addInitialGroup(ArrayRef<SILInstruction *> List) {
    Worklist.addInitialGroup(List);
  }

  /// Base visitor that does not do anything.
  SILInstruction *visitSILInstruction(SILInstruction *I) { return nullptr; }

  /// Instruction visitors.
  SILInstruction *visitReleaseValueInst(ReleaseValueInst *DI);
  SILInstruction *visitRetainValueInst(RetainValueInst *CI);
  SILInstruction *visitReleaseValueAddrInst(ReleaseValueAddrInst *DI);
  SILInstruction *visitRetainValueAddrInst(RetainValueAddrInst *CI);
  SILInstruction *visitPartialApplyInst(PartialApplyInst *AI);
  SILInstruction *visitApplyInst(ApplyInst *AI);
  SILInstruction *visitTryApplyInst(TryApplyInst *AI);
  SILInstruction *optimizeStringObject(BuiltinInst *BI);
  SILInstruction *visitBuiltinInst(BuiltinInst *BI);
  SILInstruction *visitCondFailInst(CondFailInst *CFI);
  SILInstruction *visitStrongRetainInst(StrongRetainInst *SRI);
  SILInstruction *visitRefToRawPointerInst(RefToRawPointerInst *RRPI);
  SILInstruction *visitUpcastInst(UpcastInst *UCI);
  SILInstruction *optimizeLoadFromStringLiteral(LoadInst *LI);
  SILInstruction *visitLoadInst(LoadInst *LI);
  SILInstruction *visitIndexAddrInst(IndexAddrInst *IA);
  SILInstruction *visitAllocStackInst(AllocStackInst *AS);
  SILInstruction *visitAllocRefInst(AllocRefInst *AR);
  SILInstruction *visitSwitchEnumAddrInst(SwitchEnumAddrInst *SEAI);
  SILInstruction *visitInjectEnumAddrInst(InjectEnumAddrInst *IEAI);
  SILInstruction *visitPointerToAddressInst(PointerToAddressInst *PTAI);
  SILInstruction *visitUncheckedAddrCastInst(UncheckedAddrCastInst *UADCI);
  SILInstruction *visitUncheckedRefCastInst(UncheckedRefCastInst *URCI);
  SILInstruction *visitUncheckedRefCastAddrInst(UncheckedRefCastAddrInst *URCI);
  SILInstruction *visitBridgeObjectToRefInst(BridgeObjectToRefInst *BORI);
  SILInstruction *visitUnconditionalCheckedCastInst(
                    UnconditionalCheckedCastInst *UCCI);
  SILInstruction *
  visitUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *UCCAI);
  SILInstruction *visitRawPointerToRefInst(RawPointerToRefInst *RPTR);
  SILInstruction *
  visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *TEDAI);
  SILInstruction *visitStrongReleaseInst(StrongReleaseInst *SRI);
  SILInstruction *visitCondBranchInst(CondBranchInst *CBI);
  SILInstruction *
  visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *UTBCI);
  SILInstruction *
  visitUncheckedBitwiseCastInst(UncheckedBitwiseCastInst *UBCI);
  SILInstruction *visitSelectEnumInst(SelectEnumInst *EIT);
  SILInstruction *visitSelectEnumAddrInst(SelectEnumAddrInst *EIT);
  SILInstruction *visitAllocExistentialBoxInst(AllocExistentialBoxInst *S);
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
  SILInstruction *visitUnreachableInst(UnreachableInst *UI);
  SILInstruction *visitAllocRefDynamicInst(AllocRefDynamicInst *ARDI);
  SILInstruction *visitEnumInst(EnumInst *EI);
      
  SILInstruction *visitMarkDependenceInst(MarkDependenceInst *MDI);
  SILInstruction *visitClassifyBridgeObjectInst(ClassifyBridgeObjectInst *CBOI);
  SILInstruction *visitConvertFunctionInst(ConvertFunctionInst *CFI);
  SILInstruction *
  visitConvertEscapeToNoEscapeInst(ConvertEscapeToNoEscapeInst *Cvt);

  /// Instruction visitor helpers.
  SILInstruction *optimizeBuiltinCanBeObjCClass(BuiltinInst *AI);

  // Optimize the "trunc_N1_M2" builtin. if N1 is a result of "zext_M1_*" and
  // the following holds true: N1 > M1 and M2>= M1
  SILInstruction *optimizeBuiltinTruncOrBitCast(BuiltinInst *I);

  // Optimize the "zext_M2_M3" builtin. if M2 is a result of "zext_M1_M2"
  SILInstruction *optimizeBuiltinZextOrBitCast(BuiltinInst *I);

  // Optimize the "cmp_eq_XXX" builtin. If \p NegateResult is true then negate
  // the result bit.
  SILInstruction *optimizeBuiltinCompareEq(BuiltinInst *AI, bool NegateResult);

  SILInstruction *tryOptimizeApplyOfPartialApply(PartialApplyInst *PAI);

  SILInstruction *optimizeApplyOfConvertFunctionInst(FullApplySite AI,
                                                     ConvertFunctionInst *CFI);
  // Optimize concatenation of string literals.
  // Constant-fold concatenation of string literals known at compile-time.
  SILInstruction *optimizeConcatenationOfStringLiterals(ApplyInst *AI);

  // Optimize an application of f_inverse(f(x)) -> x.
  bool optimizeIdentityCastComposition(ApplyInst *FInverse,
                                       StringRef FInverseName, StringRef FName);

private:
  FullApplySite rewriteApplyCallee(FullApplySite apply, SILValue callee);

  bool canReplaceArg(FullApplySite Apply, const ConcreteExistentialInfo &CEI,
                     unsigned ArgIdx);
  SILInstruction *createApplyWithConcreteType(
      FullApplySite Apply,
      const llvm::SmallDenseMap<unsigned, ConcreteExistentialInfo> &CEIs,
      SILBuilderContext &BuilderCtx);

  // Common utility function to replace the WitnessMethodInst using a
  // BuilderCtx.
  void replaceWitnessMethodInst(WitnessMethodInst *WMI,
                                SILBuilderContext &BuilderCtx,
                                CanType ConcreteType,
                                const ProtocolConformanceRef ConformanceRef);

  SILInstruction *
  propagateConcreteTypeOfInitExistential(FullApplySite Apply,
                                         WitnessMethodInst *WMI);
  SILInstruction *propagateConcreteTypeOfInitExistential(FullApplySite Apply);

  /// Propagate concrete types from ProtocolConformanceAnalysis.
  SILInstruction *propagateSoleConformingType(FullApplySite Apply,
                                              WitnessMethodInst *WMI);

  /// Perform one SILCombine iteration.
  bool doOneIteration(SILFunction &F, unsigned Iteration);

  /// Add reachable code to the worklist. Meant to be used when starting to
  /// process a new function.
  void addReachableCodeToWorklist(SILBasicBlock *BB);

  typedef SmallVector<SILInstruction*, 4> UserListTy;

  /// \brief Returns a list of instructions that project or perform reference
  /// counting operations on \p Value or on its uses.
  /// \return return false if \p Value has other than ARC uses.
  static bool recursivelyCollectARCUsers(UserListTy &Uses, ValueBase *Value);

  /// Erases an apply instruction including all it's uses \p.
  /// Inserts release/destroy instructions for all owner and in-parameters.
  /// \return Returns true if successful.
  bool eraseApply(FullApplySite FAS, const UserListTy &Users);

  /// Returns true if the results of a try_apply are not used.
  static bool isTryApplyResultNotUsed(UserListTy &AcceptedUses,
                                      TryApplyInst *TAI);
};

} // end namespace swift

#endif
