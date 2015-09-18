//===--- LLVMARCContract.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "swift-arc-contract"
#include "swift/LLVMPasses/Passes.h"
#include "ARCEntryPointBuilder.h"
#include "LLVMARCOpts.h"
#include "swift/Basic/Fallthrough.h"
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

/// Pimpl implementation of SwiftARCContractPass.
namespace {

struct LocalState {
  Value *CurrentLocalUpdate;
  TinyPtrVector<CallInst *> RetainList;
  TinyPtrVector<CallInst *> ReleaseList;
};

/// This implements the very late (just before code generation) lowering
/// processes that we do to expose low level performance optimizations and take
/// advantage of special features of the ABI.  These expansion steps can foil
/// the general mid-level optimizer, so they are done very, very, late.
///
/// Optimizations include:
///
///   - Lowering "retain no return" calls to swift_retain (which return the
///     retained argument) to lower register pressure.
///
///   - Merging together retain and release calls into retain_n, release_n
///   - calls.
///
/// Coming into this function, we assume that the code is in canonical form:
/// none of these calls have any uses of their return values.
class SwiftARCContractImpl {
  /// Was a change made while running the optimization.
  bool Changed;

  /// The function that we are processing.
  Function &F;

  /// The entry point builder that is used to construct ARC entry points.
  ARCEntryPointBuilder B;

  /// Since all of the calls are canonicalized, we know that we can just walk
  /// through the function and collect the interesting heap object definitions
  /// by getting the argument to these functions.
  DenseMap<Value *, TinyPtrVector<Instruction *>> DefsOfValue;

  /// Keep track of which order we see values in since iteration over a densemap
  /// isn't in a deterministic order, and isn't efficient anyway.
  ///
  /// TODO: Maybe this should be merged into DefsOfValue in a MapVector?
  SmallVector<Value *, 16> DefOrder;

public:
  SwiftARCContractImpl(Function &InF) : Changed(false), F(InF), B(F) {}

  // The top level run routine of the pass.
  bool run();

private:
  /// Perform single basic block optimizations.
  ///
  /// This means changing retain_no_return into retains, finding return values,
  /// and merging retains, releases.
  void performSingleBBOpts();

  /// Perform the RRN Optimization given the current state that we are
  /// tracking. This is called at the end of BBs and if we run into an unknown
  /// call.
  void
  performRRNOptimization(DenseMap<Value *, LocalState> &PtrToLocalStateMap);
};

} // end anonymous namespace

void SwiftARCContractImpl::
performRRNOptimization(DenseMap<Value *, LocalState> &PtrToLocalStateMap) {
  // Go through all of our pointers and merge all of the retains with the
  // first retain we saw and all of the releases with the last release we saw.
  for (auto &P : PtrToLocalStateMap) {
    Value *ArgVal = P.first;
    auto &RetainList = P.second.RetainList;
    if (RetainList.size() > 1) {
      // Create the retainN call right by the first retain.
      B.setInsertPoint(RetainList[0]);
      auto &CI = *B.createRetainN(RetainList[0]->getArgOperand(0),
                                  RetainList.size());

      // Change the Local Entry to be the new retainN call.
      P.second.CurrentLocalUpdate = &CI;

      // Change GlobalEntry to track the new retainN instruction instead of
      // the last retain that was seen.
      TinyPtrVector<Instruction *> &GlobalEntry = DefsOfValue[ArgVal];
      GlobalEntry.pop_back();
      GlobalEntry.push_back(&CI);

      // Replace all uses of the retain instructions with our new retainN and
      // then delete them.
      for (auto *Inst : RetainList) {
        Inst->replaceAllUsesWith(&CI);
        Inst->eraseFromParent();
        NumRetainReleasesEliminatedByMergingIntoRetainReleaseN++;
      }

      NumRetainReleasesEliminatedByMergingIntoRetainReleaseN--;
    }
    RetainList.clear();

    auto &ReleaseList = P.second.ReleaseList;
    if (ReleaseList.size() > 1) {
      // Create the releaseN call right by the last release.
      auto *OldCI = ReleaseList[ReleaseList.size() - 1];
      B.setInsertPoint(OldCI);
      B.createReleaseN(OldCI->getArgOperand(0), ReleaseList.size());

      // Remove all old release instructions.
      for (auto *Inst : ReleaseList) {
        Inst->eraseFromParent();
        NumRetainReleasesEliminatedByMergingIntoRetainReleaseN++;
      }

      NumRetainReleasesEliminatedByMergingIntoRetainReleaseN--;
    }
    ReleaseList.clear();
  }
}

void SwiftARCContractImpl::performSingleBBOpts() {
  // Do a first pass over the function, collecting all interesting definitions.

  // In this pass, we rewrite any intra-block uses that we can, since the
  // SSAUpdater doesn't handle them.
  DenseMap<Value *, LocalState> PtrToLocalStateMap;
  for (BasicBlock &BB : F) {
    for (auto II = BB.begin(), IE = BB.end(); II != IE; ) {
      // Preincrement iterator to avoid iteration issues in the loop.
      Instruction &Inst = *II++;

      auto Kind = classifyInstruction(Inst);
      switch (Kind) {
      // Delete all fix lifetime instructions. After llvm-ir they have no use
      // and show up as calls in the final binary.
      case RT_FixLifetime:
        Inst.eraseFromParent();
        ++NumNoopDeleted;
        continue;
      case RT_Retain:
        llvm_unreachable("This should be canonicalized away!");
      case RT_RetainNoResult: {
        auto *ArgVal = cast<CallInst>(Inst).getArgOperand(0);

        B.setInsertPoint(&Inst);
        CallInst &CI = *B.createRetain(ArgVal);
        Inst.eraseFromParent();

        if (!isa<Instruction>(ArgVal) && !isa<Argument>(ArgVal))
          continue;

        TinyPtrVector<Instruction *> &GlobalEntry = DefsOfValue[ArgVal];

        // If this is the first definition of a value for the argument that
        // we've seen, keep track of it in DefOrder.
        if (GlobalEntry.empty())
          DefOrder.push_back(ArgVal);

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];

        // Check to see if there is already an entry for this basic block.  If
        // there is another local entry, switch to using the local value and
        // remove the previous value from the GlobalEntry.
        if (LocalEntry.CurrentLocalUpdate) {
          Changed = true;
          CI.setArgOperand(0, LocalEntry.CurrentLocalUpdate);
          assert(GlobalEntry.back() == LocalEntry.CurrentLocalUpdate &&
                 "Local/Global mismatch?");
          GlobalEntry.pop_back();
        }

        LocalEntry.CurrentLocalUpdate = &CI;
        LocalEntry.RetainList.push_back(&CI);
        GlobalEntry.push_back(&CI);
        continue;
      }
      case RT_Release: {
        // Stash any releases that we see.
        auto *CI = cast<CallInst>(&Inst);
        auto *ArgVal = CI->getArgOperand(0);

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];
        LocalEntry.ReleaseList.push_back(CI);
        SWIFT_FALLTHROUGH;
      }
      case RT_Unknown:
      case RT_AllocObject:
      case RT_NoMemoryAccessed:
      case RT_UnknownRelease:
      case RT_UnknownRetain:
      case RT_BridgeRelease:
      case RT_BridgeRetain:
      case RT_RetainUnowned:
      case RT_CheckUnowned:
      case RT_ObjCRelease:
      case RT_ObjCRetain:
        // Just remap any uses in the value.
        break;
      }

      // Check to see if there are any uses of a value in the LocalUpdates
      // map.  If so, remap it now to the locally defined version.
      for (unsigned i = 0, e = Inst.getNumOperands(); i != e; ++i) {
        auto Iter = PtrToLocalStateMap.find(Inst.getOperand(i));
        if (Iter != PtrToLocalStateMap.end()) {
          if (Value *V = Iter->second.CurrentLocalUpdate) {
            Changed = true;
            Inst.setOperand(i, V);
          }
        }
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
}

bool SwiftARCContractImpl::run() {
  // Perform single BB optimizations and gather information in prepration for
  // the multiple BB optimizations.
  performSingleBBOpts();

  // Now that we've collected all of the interesting heap object values that are
  // passed into argument-returning functions, rewrite uses of these pointers
  // with optimized lifetime-shorted versions of it.
  for (Value *Ptr : DefOrder) {
    // If Ptr is an instruction, remember its block.  If not, use the entry
    // block as its block (it must be an argument, constant, etc).
    BasicBlock *PtrBlock;
    if (auto *PI = dyn_cast<Instruction>(Ptr))
      PtrBlock = PI->getParent();
    else
      PtrBlock = &F.getEntryBlock();

    TinyPtrVector<Instruction *> &Defs = DefsOfValue[Ptr];
    // This is the same problem as SSA construction, so we just use LLVM's
    // SSAUpdater, with each retain as a definition of the virtual value.
    SSAUpdater Updater;
    Updater.Initialize(Ptr->getType(), Ptr->getName());

    // Set the return value of each of these calls as a definition of the
    // virtual value.
    for (auto *D : Defs)
      Updater.AddAvailableValue(D->getParent(), D);

    // If we didn't add a definition for Ptr's block, then Ptr itself is
    // available in its block.
    if (!Updater.HasValueForBlock(PtrBlock))
      Updater.AddAvailableValue(PtrBlock, Ptr);

    // Rewrite uses of Ptr to their optimized forms.
    //
    // NOTE: We are assuming that our Ptrs are not constants meaning that we
    // know that users can not be constant expressions.
    for (auto UI = Ptr->user_begin(), E = Ptr->user_end(); UI != E; ) {
      // Make sure to increment the use iterator before potentially rewriting
      // it.
      Use &U = UI.getUse();
      ++UI;

      // If the use is in the same block that defines it and the User is not a
      // PHI node, then this is a local use that shouldn't be rewritten.
      auto *User = cast<Instruction>(U.getUser());
      if (User->getParent() == PtrBlock && !isa<PHINode>(User))
        continue;

      // Otherwise, change it if profitable!
      Updater.RewriteUse(U);

      if (U.get() != Ptr)
        Changed = true;
    }
  }

  return Changed;
}

bool SwiftARCContract::runOnFunction(Function &F) {
  return SwiftARCContractImpl(F).run();
}

namespace llvm {
  void initializeSwiftARCContractPass(PassRegistry&);
}

char SwiftARCContract::ID = 0;
INITIALIZE_PASS(SwiftARCContract,
                "swift-arc-contract", "Swift ARC contraction", false, false)

llvm::FunctionPass *swift::createSwiftARCContractPass() {
  initializeSwiftARCContractPass(*llvm::PassRegistry::getPassRegistry());
  return new SwiftARCContract();
}
