//===--- NonLocalAccessBlockAnalysis.cpp  - Nonlocal end_access -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Analysis/NonLocalAccessBlockAnalysis.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;

// Populate this->accessBlocks with all blocks containing a non-local
// end_access.
void NonLocalAccessBlocks::compute() {
  for (SILBasicBlock &block : *this->function) {
    for (SILInstruction &inst : block) {
      if (auto *endAccess = dyn_cast<EndAccessInst>(&inst)) {
        if (endAccess->getBeginAccess()->getParent() != endAccess->getParent())
          this->accessBlocks.insert(&block);
      } else if (isa<EndUnpairedAccessInst>(inst)) {
        this->accessBlocks.insert(&block);
      }
    }
  }
}
