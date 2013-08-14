//===--- MandatoryInlining.cpp - Perform inlining of "force_inline" sites -===//
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
#define DEBUG_TYPE "mandatory-inlining"
#include "swift/Subsystems.h"
#include "swift/SIL/SILInliner.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumSitesInlined, "Number of function application sites inlined");

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILMandatoryInlining(SILModule *M) {
  SmallVector<ApplyInst*, 4> ApplySites;
  for (auto &Fn : *M) {
    ApplySites.clear();

    for (auto &BB : Fn) {
      // FIXME: Recursively inline functions, and diagnose attempts to force
      // circular inlining
      for (auto &I : BB) {
        ApplyInst *AI;
        if ((AI = dyn_cast<ApplyInst>(&I)) && AI->isForceInline())
          ApplySites.push_back(AI);
      }

      SILInliner Inliner(Fn);
      for (auto *AI : ApplySites) {
        Inliner.inlineFunction(AI);
        ++NumSitesInlined;
      }
    }
  }
}
