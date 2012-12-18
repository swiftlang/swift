//===--- BasicBlock.cpp - Basic blocks for high-level SIL code -------------==//
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
// This file defines the high-level BasicBlocks used for Swift SIL code.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/BasicBlock.h"
#include "swift/SIL/BBArgument.h"
#include "swift/SIL/Function.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// BBArgument Implementation
//===----------------------------------------------------------------------===//

BBArgument::BBArgument(SILType Ty, BasicBlock *ParentBB)
  : ValueBase(ValueKind::BBArgument, Ty), ParentBB(ParentBB) {
  ParentBB->addArgument(this);
}

//===----------------------------------------------------------------------===//
// BasicBlock Implementation
//===----------------------------------------------------------------------===//

BasicBlock::BasicBlock(Function *Parent, const char *Name)
  : Parent(Parent), PredList(0) {
  Parent->getBlocks().push_back(this);
    
  // FIXME: Drop the name on the floor for now.
}
BasicBlock::~BasicBlock() {}


/// eraseFromParent - This method unlinks 'this' from the containing SIL and
/// deletes it.
///
void BasicBlock::eraseFromParent() {
  getParent()->getBlocks().erase(this);
}
