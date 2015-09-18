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
      B.createRetainN(RetainList[0]->getArgOperand(0), RetainList.size());

      // Replace all uses of the retain instructions with our new retainN and
      // then delete them.
      for (auto *Inst : RetainList) {
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


bool SwiftARCContractImpl::run() {
  // Perform single BB optimizations and gather information in prepration for
  // intra-BB retain/release merging.
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
      case RT_Retain: {
        auto *CI = cast<CallInst>(&Inst);
        auto *ArgVal = CI->getArgOperand(0);

        LocalState &LocalEntry = PtrToLocalStateMap[ArgVal];
        LocalEntry.RetainList.push_back(CI);
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
