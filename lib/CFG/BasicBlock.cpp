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

BasicBlock::BasicBlock(CFG *C) : cfg(C) { cfg->blocks.push_back(this); }
BasicBlock::~BasicBlock() {}

/// Pretty-print the BasicBlock.
void BasicBlock::dump() const { print(llvm::errs()); }

/// Pretty-print the BasicBlock with the designated stream.
void BasicBlock::print(llvm::raw_ostream &OS) const {
  OS << "[Block " << (void*) this << "]\n";
  for (const Instruction &I : instructions)
    I.print(OS);
  OS << "Preds:";
  for (const BasicBlock *B : preds())
    OS << ' ' << (void*) B;
  OS << "\nSuccs:";
  for (const BasicBlock *B : succs())
    OS << ' ' << (void*) B;
  OS << '\n';
}

namespace llvm {
raw_ostream &operator<<(raw_ostream &OS, const ::swift::BasicBlock &B) {
  OS << 'B' << (void*) &B;
  return OS;
}
} // end namespace llvm
