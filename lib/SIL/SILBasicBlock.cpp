//===--- SILBasicBlock.cpp - Basic blocks for high-level SIL code ----------==//
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

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// SILArgument Implementation
//===----------------------------------------------------------------------===//

SILArgument::SILArgument(SILType Ty, SILBasicBlock *ParentBB)
  : ValueBase(ValueKind::SILArgument, Ty), ParentBB(ParentBB) {
  ParentBB->addArgument(this);
}


SILFunction *SILArgument::getFunction() {
  return getParent()->getParent();
}
const SILFunction *SILArgument::getFunction() const {
  return getParent()->getParent();
}

SILModule *SILArgument::getModule() {
  return getFunction()->getParent();
}
const SILModule *SILArgument::getModule() const {
  return getFunction()->getParent();
}


//===----------------------------------------------------------------------===//
// SILBasicBlock Implementation
//===----------------------------------------------------------------------===//

SILBasicBlock::SILBasicBlock(SILFunction *Parent)
  : Parent(Parent), PredList(0) {
  Parent->getBlocks().push_back(this);
}
SILBasicBlock::~SILBasicBlock() {}

SILModule *SILBasicBlock::getModule() {
  return getParent()->getParent();
}
const SILModule *SILBasicBlock::getModule() const {
  return getParent()->getParent();
}

/// eraseFromParent - This method unlinks 'this' from the containing SIL and
/// deletes it.
///
void SILBasicBlock::eraseFromParent() {
  getParent()->getBlocks().erase(this);
}
