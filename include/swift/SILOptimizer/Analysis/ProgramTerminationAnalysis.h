//===--- ProgramTerminationAnalysis.h ---------------------------*- C++ -*-===//
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
///
/// \file
///
/// This is an analysis which determines if a block is a "program terminating
/// block". Define a program terminating block is defined as follows:
///
/// 1. A block at whose end point according to the SIL model, the program must
/// end. An example of such a block is one that includes a call to fatalError.
/// 2. Any block that is joint post-dominated by program terminating blocks.
///
/// For now we only identify instances of 1. But the analysis could be extended
/// appropriately via simple dataflow or through the use of post-dominator
/// trees.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_PROGRAMTERMINATIONANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_PROGRAMTERMINATIONANALYSIS_H

#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {

class ProgramTerminationFunctionInfo {
  llvm::SmallPtrSet<const SILBasicBlock *, 4> ProgramTerminatingBlocks;

public:
  ProgramTerminationFunctionInfo(const SILFunction *F) {
    for (const auto &BB : *F) {
      if (!isARCInertTrapBB(&BB))
        continue;
      ProgramTerminatingBlocks.insert(&BB);
    }
  }

  bool isProgramTerminatingBlock(const SILBasicBlock *BB) const {
    return ProgramTerminatingBlocks.count(BB);
  }
};

} // end swift namespace

#endif
