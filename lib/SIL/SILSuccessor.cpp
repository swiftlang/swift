//===--- SILSuccessor.cpp - Implementation of SILSuccessor.h ---------------==//
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

#include "swift/SIL/SILSuccessor.h"
#include "swift/SIL/SILBasicBlock.h"
using namespace swift;

void SILSuccessor::operator=(BasicBlock *BB) {
  // If we're not changing anything, we're done.
  if (SuccessorBlock == BB) return;
  
  assert(ContainingInst &&"init method not called after default construction?");
  
  // If we were already pointing to a basic block, remove ourself from its
  // predecessor list.
  if (SuccessorBlock) {
    *Prev = Next;
    if (Next) Next->Prev = Prev;
  }
  
  // If we have a successor, add ourself to its prev list.
  if (BB) {
    Prev = &BB->PredList;
    Next = BB->PredList;
    if (Next) Next->Prev = &Next;
    BB->PredList = this;
  }
  
  SuccessorBlock = BB;
}

// Derferencing the SuccIterator returns the predecessors BasicBlock.
BasicBlock *SILSuccessorIterator::operator*() {
  assert(Cur && "Can't deference end (or default constructed) iterator");
  return Cur->ContainingInst->getParent();
}
