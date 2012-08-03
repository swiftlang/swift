//===--- BasicBlock.cpp - Basic blocks for high-level CFGs -------*- C++ -*-==//
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

#include "swift/CFG/BasicBlock.h"
#include "swift/CFG/CFG.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

BasicBlock::BasicBlock(CFG *C, unsigned &BlockID) :
  FrontI(0), BackI(0), cfg(C), blockID(BlockID++)
{
  cfg->Blks.push_back(this);
}

BasicBlock::~BasicBlock() {}

/// Pretty-print the BasicBlock.
void BasicBlock::dump() const {
  print(llvm::errs());
}

/// Pretty-print the BasicBlock with the designated stream.
void BasicBlock::print(llvm::raw_ostream &OS) const {
  OS << "[Block " << blockID << "]\n";
  for (const Instruction *I : *this)
    I->print(OS);
  if (TermInst *TI = terminator()) {
    OS << "Terminator: ";
    TI->print(OS);
  }
  OS << "Preds:";
  for (const BasicBlock *B : preds())
    OS << ' ' << B->blockID;
  OS << "\nSuccs:";
  for (const BasicBlock *B : succs())
    OS << ' ' << B->blockID;
  OS << '\n';
}


