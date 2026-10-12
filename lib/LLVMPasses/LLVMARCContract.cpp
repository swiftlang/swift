//===--- LLVMARCContract.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "swift-arc-contract"
#include "swift/LLVMPasses/Passes.h"
#include "ARCEntryPointBuilder.h"
#include "LLVMARCOpts.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Utils/SSAUpdater.h"

using namespace llvm;
using namespace swift;
using swift::SwiftARCContract;

STATISTIC(NumNoopDeleted,
          "Number of no-op swift calls eliminated");
STATISTIC(NumRetainReleasesEliminatedByMergingIntoRetainReleaseN,
          "Number of retain/release eliminated by merging into "
          "retain_n/release_n");
STATISTIC(NumUnknownObjectRetainReleasesEliminatedByMergingIntoRetainReleaseN,
          "Number of retain/release eliminated by merging into "
          "unknownObjectRetain_n/unknownObjectRelease_n");
STATISTIC(NumBridgeRetainReleasesEliminatedByMergingIntoRetainReleaseN,
          "Number of bridge retain/release eliminated by merging into "
          "bridgeRetain_n/bridgeRelease_n");

/// The largest count passed to a single retain_n or release_n call. The
/// Embedded runtime requires this to be no more than maxRefcountDelta in
/// EmbeddedRuntime.swift.
static constexpr size_t MaxRetainReleaseN = 256;

/// Pimpl implementation of SwiftARCContractPass.
namespace {

struct LocalState {
  TinyPtrVector<CallInst *> RetainList;
  TinyPtrVector<CallInst *> ReleaseList;
  TinyPtrVector<CallInst *> UnknownObjectRetainList;
  TinyPtrVector<CallInst *> UnknownObjectReleaseList;
  TinyPtrVector<CallInst *> BridgeRetainList;
  TinyPtrVector<CallInst *> BridgeReleaseList;
};

/// This implements the very late (just before code generation) lowering
/// processes that we do to expose low level performance optimizations and take
/// advantage of special features of the ABI.  These expansion steps can foil
/// the general mid-level optimizer, so they are done very, very, late.
///
/// Optimizations include:
///
///   - Merging together retain and release calls into retain_n, release_n
///   - calls.
///
/// Coming into this function, we assume that the code is in canonical form:
/// none of these calls have any uses of their return values.
class SwiftARCContractImpl {
  /// Was a change made while running the optimization.
  bool Changed;

  /// Swift RC Identity.
  SwiftRCIdentity RC;

  /// The function that we are processing.
  Function &F;

  /// The entry point builder that is used to construct ARC entry points.
  ARCEntryPointBuilder B;
public:
  SwiftARCContractImpl(Function &InF) : Changed(false), F(InF), B(F) {}

  // The top level run routine of the pass.
  bool run();

private:
  /// Perform the RRN Optimization given the current state that we are
  /// tracking. This is called at the end of BBs and if we run into an unknown
  /// call.
  void
  performRRNOptimization(DenseMap<Value *, LocalState> &PtrToLocalStateMap);
};

} // end anonymous namespace

/// Replace the calls in \p List with calls made by \p CreateN, each covering at
/// most MaxRetainReleaseN of them. Retains are merged at the first retain of each
/// group and releases at the last release. Returns the number of calls removed.
template <typename CreateNFn>
static unsigned mergeIntoN(TinyPtrVector<CallInst *> &List, bool AtLast,
                           ARCEntryPointBuilder &B, SwiftRCIdentity &RC,
                           CreateNFn CreateN) {
  unsigned NumRemoved = 0;
  ArrayRef<CallInst *> Remaining = List;
  while (Remaining.size() > 1) {
    ArrayRef<CallInst *> Group = Remaining.take_front(MaxRetainReleaseN);
    Remaining = Remaining.drop_front(Group.size());

    CallInst *OldCI = AtLast ? Group.back() : Group.front();
    B.setInsertPoint(OldCI);
    Value *O = RC.getSwiftRCIdentityRoot(OldCI->getArgOperand(0));
    CallInst *RI = OldCI;
    for (auto *R : Group) {
      if (B.isAtomic(R)) {
        RI = R;
        break;
      }
    }
    CallInst *NewCI = CreateN(O, Group.size(), RI);

    for (auto *Inst : Group) {
      // Bridge retains may modify the input reference before forwarding it, so
      // their results are used. A pointer cast may be needed when types have
      // been obfuscated in some way.
      if (!Inst->use_empty()) {
        B.setInsertPoint(Inst);
        Inst->replaceAllUsesWith(B.maybeCast(NewCI, Inst->getType()));
      }
      Inst->eraseFromParent();
    }
    NumRemoved += Group.size() - 1;
  }
  List.clear();
  return NumRemoved;
}

void SwiftARCContractImpl::
performRRNOptimization(DenseMap<Value *, LocalState> &PtrToLocalStateMap) {
  for (auto &P : PtrToLocalStateMap) {
    LocalState &S = P.second;
    NumRetainReleasesEliminatedByMergingIntoRetainReleaseN +=
        mergeIntoN(S.RetainList, /*AtLast=*/false, B, RC,
                   [&](Value *O, unsigned N, CallInst *RI) {
                     return B.createRetainN(O, N, RI);
                   });
    NumRetainReleasesEliminatedByMergingIntoRetainReleaseN +=
        mergeIntoN(S.ReleaseList, /*AtLast=*/true, B, RC,
                   [&](Value *O, unsigned N, CallInst *RI) {
                     return B.createReleaseN(O, N, RI);
                   });
    NumUnknownObjectRetainReleasesEliminatedByMergingIntoRetainReleaseN +=
        mergeIntoN(S.UnknownObjectRetainList, /*AtLast=*/false, B, RC,
                   [&](Value *O, unsigned N, CallInst *RI) {
                     return B.createUnknownObjectRetainN(O, N, RI);
                   });
    NumUnknownObjectRetainReleasesEliminatedByMergingIntoRetainReleaseN +=
        mergeIntoN(S.UnknownObjectReleaseList, /*AtLast=*/true, B, RC,
                   [&](Value *O, unsigned N, CallInst *RI) {
                     return B.createUnknownObjectReleaseN(O, N, RI);
                   });
    NumBridgeRetainReleasesEliminatedByMergingIntoRetainReleaseN +=
        mergeIntoN(S.BridgeRetainList, /*AtLast=*/false, B, RC,
                   [&](Value *O, unsigned N, CallInst *RI) {
                     return B.createBridgeRetainN(O, N, RI);
                   });
    NumBridgeRetainReleasesEliminatedByMergingIntoRetainReleaseN +=
        mergeIntoN(S.BridgeReleaseList, /*AtLast=*/true, B, RC,
                   [&](Value *O, unsigned N, CallInst *RI) {
                     return B.createBridgeReleaseN(O, N, RI);
                   });
  }
}


bool SwiftARCContractImpl::run() {
  // intra-BB retain/release merging.
  DenseMap<Value *, LocalState> PtrToLocalStateMap;
  for (BasicBlock &BB : F) {
    for (auto II = BB.begin(), IE = BB.end(); II != IE; ) {
      // Preincrement iterator to avoid iteration issues in the loop.
      Instruction &Inst = *II++;

      auto Kind = classifyInstruction(Inst);
      switch (Kind) {
      // The *_n forms are normally created by this pass, so we shouldn't
      // encounter them on input. But IR that defines the runtime entry
      // points themselves (e.g. EmbeddedRuntime.swift) can contain calls
      // to them directly, so just leave them alone.
      case RT_RetainN:
      case RT_UnknownObjectRetainN:
      case RT_BridgeRetainN:
      case RT_ReleaseN:
      case RT_UnknownObjectReleaseN:
      case RT_BridgeReleaseN:
        break;
      // Delete all fix lifetime and end borrow instructions. After llvm-ir they
      // have no use and show up as calls in the final binary.
      case RT_FixLifetime:
      case RT_EndBorrow:
        Inst.eraseFromParent();
        ++NumNoopDeleted;
        continue;
      case RT_Retain: {
        auto *CI = cast<CallInst>(&Inst);
        auto *ArgVal = RC.getSwiftRCIdentityRoot(CI->getArgOperand(0));

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];
        LocalEntry.RetainList.push_back(CI);
        continue;
      }
      case RT_UnknownObjectRetain: {
        auto *CI = cast<CallInst>(&Inst);
        auto *ArgVal = RC.getSwiftRCIdentityRoot(CI->getArgOperand(0));

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];
        LocalEntry.UnknownObjectRetainList.push_back(CI);
        continue;
      }
      case RT_Release: {
        // Stash any releases that we see.
        auto *CI = cast<CallInst>(&Inst);
        auto *ArgVal = RC.getSwiftRCIdentityRoot(CI->getArgOperand(0));

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];
        LocalEntry.ReleaseList.push_back(CI);
        continue;
      }
      case RT_UnknownObjectRelease: {
        // Stash any releases that we see.
        auto *CI = cast<CallInst>(&Inst);
        auto *ArgVal = RC.getSwiftRCIdentityRoot(CI->getArgOperand(0));

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];
        LocalEntry.UnknownObjectReleaseList.push_back(CI);
        continue;
      }
      case RT_BridgeRetain: {
        auto *CI = cast<CallInst>(&Inst);
        auto *ArgVal = RC.getSwiftRCIdentityRoot(CI->getArgOperand(0));

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];
        LocalEntry.BridgeRetainList.push_back(CI);
        continue;
      }
      case RT_BridgeRelease: {
        auto *CI = cast<CallInst>(&Inst);
        auto *ArgVal = RC.getSwiftRCIdentityRoot(CI->getArgOperand(0));

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];
        LocalEntry.BridgeReleaseList.push_back(CI);
        continue;
      }
      case RT_Unknown:
      case RT_AllocObject:
      case RT_NoMemoryAccessed:
      case RT_RetainUnowned:
      case RT_CheckUnowned:
      case RT_ObjCRelease:
      case RT_ObjCRetain:
        break;
      }

      if (Kind != RT_Unknown)
        continue;
      
      // If we have an unknown call, we need to create any retainN calls we
      // have seen. The reason why is that we do not want to move retains,
      // releases over isUniquelyReferenced calls. Specifically imagine this:
      //
      // retain(x); unknown(x); release(x); isUniquelyReferenced(x); retain(x);
      //
      // In this case we would with this optimization merge the last retain
      // with the first. This would then create an additional copy. The
      // release side of this is:
      //
      // retain(x); unknown(x); release(x); isUniquelyReferenced(x); release(x);
      //
      // Again in such a case by merging the first release with the second
      // release, we would be introducing an additional copy.
      //
      // Thus if we see an unknown call we merge together all retains and
      // releases before. This could be made more aggressive through
      // appropriate alias analysis and usage of LLVM's function attributes to
      // determine that a function does not touch globals.
      performRRNOptimization(PtrToLocalStateMap);
    }

    // Perform the RRNOptimization.
    performRRNOptimization(PtrToLocalStateMap);
    PtrToLocalStateMap.clear();
  }

  return Changed;
}

bool SwiftARCContract::runOnFunction(Function &F) {
  return SwiftARCContractImpl(F).run();
}

char SwiftARCContract::ID = 0;
INITIALIZE_PASS_BEGIN(SwiftARCContract, "swift-arc-contract",
                      "Swift ARC contraction", false, false)
INITIALIZE_PASS_END(SwiftARCContract,
                    "swift-arc-contract", "Swift ARC contraction",
                    false, false)

llvm::FunctionPass *swift::createSwiftARCContractPass() {
  initializeSwiftARCContractPass(*llvm::PassRegistry::getPassRegistry());
  return new SwiftARCContract();
}

void SwiftARCContract::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
  AU.setPreservesCFG();
}

llvm::PreservedAnalyses
SwiftARCContractPass::run(llvm::Function &F,
                          llvm::FunctionAnalysisManager &AM) {
  // Don't touch those functions that implement reference counting in the
  // runtime.
  if (!allowArcOptimizations(F.getName()))
    return PreservedAnalyses::all();

  bool changed = SwiftARCContractImpl(F).run();
  if (!changed)
    return PreservedAnalyses::all();

  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}
