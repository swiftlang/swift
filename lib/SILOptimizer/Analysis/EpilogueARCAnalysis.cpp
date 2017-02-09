//===--- EpilogueARCAnalysis.cpp ------------------------------------------===//
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

#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                          Epilogue ARC Utilities 
//===----------------------------------------------------------------------===//

void EpilogueARCContext::initializeDataflow() {
  for (auto &B : *F) {
    // Find the exit blocks.
    if (isInterestedFunctionExitingBlock(&B)) {
      ExitBlocks.insert(&B);
    }
    // Allocate the storage.
    EpilogueARCBlockStates[&B] =
              new (BPA.Allocate()) EpilogueARCBlockState();
  }

  // Split the SILargument into local arguments to each specific basic block.
  llvm::SmallVector<SILValue, 4> ToProcess;
  llvm::DenseSet<SILValue> Processed;
  ToProcess.push_back(Arg);
  while (!ToProcess.empty()) {
    SILValue CArg = ToProcess.pop_back_val();
    if (!CArg)
      continue;
    if (Processed.find(CArg) != Processed.end())
       continue;
    Processed.insert(CArg);
    if (auto *A = dyn_cast<SILPHIArgument>(CArg)) {
      // Find predecessor and break the SILArgument to predecessors.
      for (auto X : A->getParent()->getPredecessorBlocks()) {
        // Try to find the predecessor edge-value.
        SILValue IA = A->getIncomingValue(X);
        EpilogueARCBlockStates[X]->LocalArg = IA;
        // Maybe the edge value is another SILArgument.
        ToProcess.push_back(IA);
      }
    }
  }
}

bool EpilogueARCContext::convergeDataflow() {
  // Keep iterating until Changed is false.
  bool Changed = false;
  do {
    Changed = false;
    // Iterate until the data flow converges.
    for (SILBasicBlock *B : PO->getPostOrder()) {
      auto BS = EpilogueARCBlockStates[B];
      // Merge in all the successors.
      bool BBSetOut = false;
      if (!B->succ_empty()) {
        auto Iter = B->succ_begin();
        BBSetOut = EpilogueARCBlockStates[*Iter]->BBSetIn;
        Iter = std::next(Iter);
        for (auto E = B->succ_end(); Iter != E; ++Iter) {
          BBSetOut &= EpilogueARCBlockStates[*Iter]->BBSetIn;
        }
      } else if (isExitBlock(B)) {
        // We set the BBSetOut for exit blocks.
        BBSetOut = true;
      }

      // If an epilogue ARC instruction or blocking operating has been identified
      // then there is no point visiting every instruction in this block.
      if (BBSetOut) {
        // Iterate over all instructions in the basic block and find the
        // interested ARC instruction in the block.
        for (auto I = B->rbegin(), E = B->rend(); I != E; ++I) {
          // This is a transition from 1 to 0 due to an interested instruction.
          if (isInterestedInstruction(&*I)) {
            BBSetOut = false;
            break;
          }
          // This is a transition from 1 to 0 due to a blocking instruction.
          // at this point, its OK to abort the data flow as we have one path
          // which we did not find an epilogue retain before getting blocked.
          if (mayBlockEpilogueARC(&*I, RCFI->getRCIdentityRoot(Arg))) {
            return false;
          }
        }
      }

      // Update BBSetIn.
      Changed |= (BS->BBSetIn != BBSetOut);
      BS->BBSetIn = BBSetOut;
    }
  } while (Changed);
  return true;
}

bool EpilogueARCContext::computeEpilogueARC() {
  // At this point the data flow should have converged. Find the epilogue
  // releases.
  for (SILBasicBlock *B : PO->getPostOrder()) {
    bool BBSetOut = false;
    // Merge in all the successors.
    if (!B->succ_empty()) {
      // Make sure we've either found no ARC instructions in all the successors
      // or we've found ARC instructions in all successors.
      //
      // In case we've found ARC instructions in some and not all successors,
      // that means from this point to the end of the function, some paths will
      // not have an epilogue ARC instruction, which means the data flow has 
      // failed.
      auto Iter = B->succ_begin();
      auto Base = EpilogueARCBlockStates[*Iter]->BBSetIn;
      Iter = std::next(Iter);
      for (auto E = B->succ_end(); Iter != E; ++Iter) {
        if (EpilogueARCBlockStates[*Iter]->BBSetIn != Base)
          return false;
      }
      BBSetOut = Base;
    } else if (isExitBlock(B)) {
      // We set the BBSetOut for exit blocks.
      BBSetOut = true;
    }

    // If an epilogue ARC instruction or blocking operating has been identified
    // then there is no point visiting every instruction in this block.
    if (!BBSetOut) {
      continue;
    }

    // An epilogue ARC instruction has not been identified, maybe its in this block.
    //
    // Iterate over all instructions in the basic block and find the interested ARC
    // instruction in the block.
    for (auto I = B->rbegin(), E = B->rend(); I != E; ++I) {
      // This is a transition from 1 to 0 due to an interested instruction.
      if (isInterestedInstruction(&*I)) {
        EpilogueARCInsts.insert(&*I);
        break;
      }
      // This is a transition from 1 to 0 due to a blocking instruction.
      if (mayBlockEpilogueARC(&*I, RCFI->getRCIdentityRoot(Arg))) {
        break;
      }
    }
  }
  return true;
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

void EpilogueARCAnalysis::initialize(SILPassManager *PM) {
  AA = PM->getAnalysis<AliasAnalysis>();
  PO = PM->getAnalysis<PostOrderAnalysis>();
  RC = PM->getAnalysis<RCIdentityAnalysis>();
}

SILAnalysis *swift::createEpilogueARCAnalysis(SILModule *M) {
  return new EpilogueARCAnalysis(M);
}
