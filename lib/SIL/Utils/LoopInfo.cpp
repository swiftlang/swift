//===--- LoopInfo.cpp - SIL Loop Analysis ---------------------------------===//
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

#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/Dominance.h"
#include "llvm/Support/GenericLoopInfoImpl.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// Instantiate template members.
template class llvm::LoopBase<SILBasicBlock, SILLoop>;
template class llvm::LoopInfoBase<SILBasicBlock, SILLoop>;


void SILLoop::dump() const {
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  print(llvm::dbgs());
#endif
}

SILLoopInfo::SILLoopInfo(SILFunction *F, DominanceInfo *DT) : Dominance(DT) {
  LI.analyze(*Dominance);
}

void SILLoopInfo::verify() const {
  LI.verify(*Dominance);
}
