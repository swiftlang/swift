//===--- EpilogueARCAnalysis.h ----------------------------------*- C++ -*-===//
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
//  This is an analysis that determines the ref count identity (i.e. gc root) of
//  a pointer. Any values with the same ref count identity are able to be
//  retained and released interchangeably.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_EPILOGUEARCANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_EPILOGUEARCANALYSIS_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"

namespace swift {

/// EpilogueARCBlockState - Keep track of whether an epilogue ARC instruction
/// has been found.
struct EpilogueARCBlockState {
  /// Keep track of whether an epilogue release has been found before and after
  /// this basic block.
  bool BBSetIn;
  /// The basic block local SILValue we are interested to find epilogue ARC in.
  SILValue LocalArg;
  /// Constructor, we only compute epilogue ARC instruction for 1 argument at
  /// a time.
  /// Optimistic data flow.
  EpilogueARCBlockState() { BBSetIn = true; LocalArg = SILValue(); }
};

/// EpilogueARCContext - This class implements a data flow with which epilogue
/// retains or releases for a SILValue are found.
///
/// NOTE:
/// In case of release finder, this function assumes the SILArgument has
/// @owned semantic.
/// In case of retain finder, this class assumes Arg is one of the return value
/// of the function.
class EpilogueARCContext {
public:
  enum EpilogueARCKind { Retain = 0, Release = 1 };

private:
  // Are we finding retains or releases.
  EpilogueARCKind Kind;

  // The argument we are looking for epilogue ARC instruction for.
  SILValue Arg;

  /// The allocator we are currently using.
  llvm::SpecificBumpPtrAllocator<EpilogueARCBlockState> BPA;

  /// Current function we are analyzing.
  SILFunction *F;

  /// Current post-order we are using.
  PostOrderFunctionInfo *PO;

  /// Current alias analysis we are using.
  AliasAnalysis *AA;

  /// Current rc-identity we are using.
  RCIdentityFunctionInfo *RCFI;

  /// The epilogue retains or releases.
  llvm::SmallSetVector<SILInstruction *, 1> EpilogueARCInsts; 

  /// All the retain/release block state for all the basic blocks in the function. 
  llvm::DenseMap<SILBasicBlock *, EpilogueARCBlockState *> EpilogueARCBlockStates;

  /// The exit blocks of the function.
  llvm::SmallPtrSet<SILBasicBlock *, 2> ExitBlocks;

  /// Return true if this is a function exiting block this epilogue ARC
  /// matcher is interested in. 
  bool isInterestedFunctionExitingBlock(SILBasicBlock *BB) {
    if (EpilogueARCKind::Release == Kind)  
      return BB->getTerminator()->isFunctionExiting();

    return BB->getTerminator()->isFunctionExiting() &&
           BB->getTerminator()->getTermKind() != TermKind::ThrowInst;
  }

  /// Return true if this is a function exit block.
  bool isExitBlock(SILBasicBlock *BB) {
    return ExitBlocks.count(BB);
  }

  /// Return true if this is a retain instruction.
  bool isRetainInstruction(SILInstruction *II) {
    return isa<RetainValueInst>(II) || isa<StrongRetainInst>(II);
  }

  /// Return true if this is a release instruction.
  bool isReleaseInstruction(SILInstruction *II) {
    return isa<ReleaseValueInst>(II) || isa<StrongReleaseInst>(II);
  }

  SILValue getArg(SILBasicBlock *B) {
    SILValue A = EpilogueARCBlockStates[B]->LocalArg;
    if (A)
      return A;
    return Arg;
  }

public:
  /// Constructor.
  EpilogueARCContext(EpilogueARCKind Kind, SILValue Arg, SILFunction *F,
                     PostOrderFunctionInfo *PO, AliasAnalysis *AA,
                     RCIdentityFunctionInfo *RCFI)
    : Kind(Kind), Arg(Arg), F(F), PO(PO), AA(AA), RCFI(RCFI) {}

  /// Run the data flow to find the epilogue retains or releases.
  bool run() {
    // Initialize the epilogue arc data flow context.
    initializeDataflow();
    // Converge the data flow.
    if (!convergeDataflow())
      return false;
    // Lastly, find the epilogue ARC instructions.
    return computeEpilogueARC();
  }

  /// Reset the epilogue arc instructions. 
  void resetEpilogueARCInsts() { EpilogueARCInsts.clear(); }
  llvm::SmallSetVector<SILInstruction *, 1> getEpilogueARCInsts() {
    return EpilogueARCInsts;
  }

  /// Initialize the data flow.
  void initializeDataflow();

  /// Keep iterating until the data flow is converged.
  bool convergeDataflow();

  /// Find the epilogue ARC instructions.
  bool computeEpilogueARC();

  /// This instruction prevents looking further for epilogue retains on the
  /// current path.
  bool mayBlockEpilogueRetain(SILInstruction *II, SILValue Ptr) { 
    // reference decrementing instruction prevents any retain to be identified as
    // epilogue retains.
    if (mayDecrementRefCount(II, Ptr, AA))
      return true;
    // Handle self-recursion. A self-recursion can be considered a +1 on the
    // current argument.
    if (ApplyInst *AI = dyn_cast<ApplyInst>(II))
     if (AI->getCalleeFunction() == II->getParent()->getParent())
       return true;
    return false;
  } 

  /// This instruction prevents looking further for epilogue releases on the
  /// current path.
  bool mayBlockEpilogueRelease(SILInstruction *II, SILValue Ptr) { 
    // Check whether this instruction read reference count, i.e. uniqueness
    // check. Moving release past that may result in additional COW.
   if (II->mayReleaseOrReadRefCount())
      return true;
    return false;
  } 

  /// Does this instruction block the interested ARC instruction ?
  bool mayBlockEpilogueARC(SILInstruction *II, SILValue Ptr) { 
    if (Kind == EpilogueARCKind::Retain)
      return mayBlockEpilogueRetain(II, Ptr);
    return mayBlockEpilogueRelease(II, Ptr);
  }

  /// This is the type of instructions the data flow is interested in.
  bool isInterestedInstruction(SILInstruction *II) {
    // We are checking for release.
    if (Kind == EpilogueARCKind::Release)
      return isReleaseInstruction(II) &&
             RCFI->getRCIdentityRoot(II->getOperand(0)) ==
             RCFI->getRCIdentityRoot(getArg(II->getParent()));
    // We are checking for retain. If this is a self-recursion. call
    // to the function (which returns an owned value) can be treated as
    // the retain instruction.
    if (ApplyInst *AI = dyn_cast<ApplyInst>(II))
     if (AI->getCalleeFunction() == II->getParent()->getParent())
       return true;
    // Check whether this is a retain instruction and the argument it
    // retains.
    return isRetainInstruction(II) &&
           RCFI->getRCIdentityRoot(II->getOperand(0)) ==
           RCFI->getRCIdentityRoot(getArg(II->getParent()));
  }
};

/// This class is a simple wrapper around an identity cache.
class EpilogueARCFunctionInfo {
  /// Current function we are analyzing.
  SILFunction *F;

  /// Current post-order we are using.
  PostOrderAnalysis *PO;

  /// Current alias analysis we are using.
  AliasAnalysis *AA;

  /// Current rc-identity we are using.
  RCIdentityAnalysis *RC;

  using ARCInstructions = llvm::SmallSetVector<SILInstruction *, 1>;
  /// The epilogue retain cache.
  llvm::DenseMap<SILValue, ARCInstructions> EpilogueRetainInstCache;
  /// The epilogue release cache.
  llvm::DenseMap<SILValue, ARCInstructions> EpilogueReleaseInstCache;

public:
  void handleDeleteNotification(ValueBase *V) {
    // Being conservative and clear everything for now.
    EpilogueRetainInstCache.clear();
    EpilogueReleaseInstCache.clear();
  }

  /// Constructor.
  EpilogueARCFunctionInfo(SILFunction *F, PostOrderAnalysis *PO,
                          AliasAnalysis *AA, RCIdentityAnalysis *RC)
    : F(F), PO(PO), AA(AA), RC(RC) {}

  /// Find the epilogue ARC instruction based on the given \p Kind and given
  /// \p Arg.
  llvm::SmallSetVector<SILInstruction *, 1>
  computeEpilogueARCInstructions(EpilogueARCContext::EpilogueARCKind Kind,
                                 SILValue Arg) {
    auto ARCCache = Kind == EpilogueARCContext::EpilogueARCKind::Retain ?
                 EpilogueRetainInstCache :
                 EpilogueReleaseInstCache;
    auto Iter = ARCCache.find(Arg);
    if (Iter != ARCCache.end())
      return Iter->second;

    EpilogueARCContext CM(Kind, Arg, F, PO->get(F), AA, RC->get(F));
    // Initialize and run the data flow. Clear the epilogue arc instructions if the
    // data flow is aborted in middle.
   if (!CM.run()) { 
     CM.resetEpilogueARCInsts();
     return CM.getEpilogueARCInsts();
    }
    return ARCCache[Arg] = CM.getEpilogueARCInsts();
  }
};

class EpilogueARCAnalysis : public FunctionAnalysisBase<EpilogueARCFunctionInfo> {
  /// Current post order analysis we are using.
  PostOrderAnalysis *PO;
  /// Current alias analysis we are using.
  AliasAnalysis *AA;
  /// Current RC Identity analysis we are using.
  RCIdentityAnalysis *RC;

public:
  EpilogueARCAnalysis(SILModule *)
    : FunctionAnalysisBase<EpilogueARCFunctionInfo>(AnalysisKind::EpilogueARC),
      PO(nullptr), AA(nullptr), RC(nullptr) {}

  EpilogueARCAnalysis(const EpilogueARCAnalysis &) = delete;
  EpilogueARCAnalysis &operator=(const EpilogueARCAnalysis &) = delete;

  virtual void handleDeleteNotification(ValueBase *V) override {
    // If the parent function of this instruction was just turned into an
    // external declaration, bail. This happens during SILFunction destruction.
    SILFunction *F = V->getFunction();
    if (F->isExternalDeclaration()) {
      return;
    }
    get(F)->handleDeleteNotification(V);
  }

  virtual bool needsNotifications() override { return true; }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::EpilogueARC;
  }

  virtual void initialize(SILPassManager *PM) override;
  
  virtual EpilogueARCFunctionInfo *newFunctionAnalysis(SILFunction *F) override {
    return new EpilogueARCFunctionInfo(F, PO, AA, RC);
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return true;
  }

 };

} // end swift namespace

#endif
