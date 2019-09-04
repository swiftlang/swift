//===--- SILInstructionWorklist.cpp ---------------------------------------===//
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

#define DEBUG_TYPE "sil-instruction-worklist"
#include "swift/SIL/SILInstructionWorklist.h"

using namespace swift;

void SILInstructionWorklist::add(SILInstruction *instruction) {
  if (!worklistMap.insert(std::make_pair(instruction, worklist.size())).second)
    return;

  LLVM_DEBUG(llvm::dbgs() << loggingName << ": ADD: " << *instruction << '\n');
  worklist.push_back(instruction);
}

void SILInstructionWorklist::addInitialGroup(ArrayRef<SILInstruction *> list) {
  assert(worklist.empty() && "worklist must be empty to add initial group");
  worklist.reserve(list.size() + 16);
  worklistMap.reserve(list.size());
  LLVM_DEBUG(llvm::dbgs() << loggingName << ": ADDING: " << list.size()
                          << " instrs to worklist\n");
  while (!list.empty()) {
    SILInstruction *instruction = list.back();
    list = list.slice(0, list.size() - 1);
    worklistMap.insert(std::make_pair(instruction, worklist.size()));
    worklist.push_back(instruction);
  }
}
