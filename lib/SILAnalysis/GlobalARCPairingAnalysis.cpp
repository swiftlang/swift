//===-- GlobalARCPairingAnalysis.cpp - Global ARC Retain Release Pairing --===//
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

#define DEBUG_TYPE "sil-global-arc-analysis"
#include "swift/SILAnalysis/ARCAnalysis.h"
#include "BlotMapVector.h"
#include "ReferenceCountState.h"
#include "GlobalARCSequenceDataflow.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::arc;

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

bool swift::arc::
computeARCMatchingSet(SILFunction &F, AliasAnalysis *AA,
                      std::function<void (ARCMatchingSet&)> Fun) {
  BlotMapVector<SILInstruction *, TopDownRefCountState> DecToIncStateMap;
  BlotMapVector<SILInstruction *, BottomUpRefCountState> IncToDecStateMap;

  bool NestingDetected = performARCSequenceDataflow(F, AA, DecToIncStateMap,
                                                    IncToDecStateMap);
  bool MatchedPair = false;

  ARCMatchingSet MatchSet;

  for (auto &Pair : DecToIncStateMap) {
    // If we were blotted, skip this pair.
    if (!Pair.first)
      continue;

    // If we do not have an instruction, this is a state with an invalidated
    // reference count. Skip it...
    if (!Pair.second.getInstruction())
      continue;

    if (!MatchSet.Ptr)
      MatchSet.Ptr = Pair.first->getOperand(0);
    assert(MatchSet.Ptr == Pair.first->getOperand(0) &&
           "If Ptr is already set, make sure it matches the ptr on the "
           "increment.");

    auto *InsertPt = Pair.second.getInsertPoint();

    // If we reached this point and do not have an insertion point (in the case
    // where we do not complete the sequence) or are known safe, remove the
    // retain release pair.
    if (Pair.second.isKnownSafe() || !InsertPt) {
      SILInstruction *Inst = Pair.second.getInstruction();

      DEBUG(llvm::dbgs() << "Removing Pair:\n    KnownSafe: "
                         << (Pair.second.isKnownSafe() ? "yes" : "no")
                         << "\n    " << *Inst << "    " << *Pair.first);

      MatchedPair = true;
      MatchSet.Increments.insert(Inst);
      MatchSet.Decrements.insert(Pair.first);
      DecToIncStateMap.blot(Pair.first);
      Fun(MatchSet);
      MatchSet.clear();
      continue;
    }

    if (InsertPt) {
      MatchSet.Increments.insert(Pair.second.getInstruction());
      MatchSet.IncrementInsertPts.insert(InsertPt);
      Fun(MatchSet);
      MatchSet.clear();
    }
  }

  // If we did not find a matching pair or detected nesting during the dataflow,
  // there are no more increment, decrements that we can optimize.
  return MatchedPair && NestingDetected;
}
