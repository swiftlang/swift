//===--- BasicBlock.cpp - Basic blocks for high-level CFGs -----------------==//
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
//
// This file defines the high-level BasicBlocks used for Swift CFGs.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/BasicBlock.h"
#include "swift/SIL/BBArgument.h"
#include "swift/SIL/SIL.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// BBArgument Implementation
//===----------------------------------------------------------------------===//

BBArgument::BBArgument(Type Ty, BasicBlock *ParentBB)
  : Value(ValueKind::BBArgument, Ty), ParentBB(ParentBB) {
  ParentBB->addArgument(this);
}

//===----------------------------------------------------------------------===//
// BasicBlock Implementation
//===----------------------------------------------------------------------===//

BasicBlock::BasicBlock(CFG *ParentCFG, const char *Name)
  : ParentCFG(ParentCFG), PredList(0) {
  ParentCFG->getBlocks().push_back(this);
    
  // FIXME: Drop the name on the floor for now.
}
BasicBlock::~BasicBlock() {}


/// eraseFromParent - This method unlinks 'this' from the containing CFG and
/// deletes it.
///
void BasicBlock::eraseFromParent() {
  getParent()->getBlocks().erase(this);
}
