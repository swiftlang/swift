//===--- CFGBuilder.h - Class for creating CFG Constructs --------*- C++ -*-==//
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

#ifndef SWIFT_CFG_CFGBUILDER_H
#define SWIFT_CFG_CFGBUILDER_H

#include "swift/CFG/BasicBlock.h"
#include "swift/CFG/Instruction.h"

namespace swift {

class CFGBuilder {
  /// BB - If this is non-null, the instruction is inserted in the specified
  /// basic block, at the specified InsertPt.  If null, created instructions
  /// are not auto-inserted.
  BasicBlock *BB;
  BasicBlock::iterator InsertPt;
public:

  CFGBuilder() : BB(0) {}

  CFGBuilder(Instruction *I) {
    setInsertionPoint(I);
  }

  CFGBuilder(BasicBlock *BB) {
    setInsertionPoint(BB);
  }

  CFGBuilder(BasicBlock *BB, BasicBlock::iterator InsertPt) {
    setInsertionPoint(BB, InsertPt);
  }

  /// clearInsertionPoint - Clear the insertion point: created instructions will
  /// not be inserted into a block.
  void clearInsertionPoint() {
    BB = 0;
  }

  /// setInsertionPoint - Set the insertion point.
  void setInsertionPoint(BasicBlock *BB, BasicBlock::iterator InsertPt) {
    this->BB = BB;
    this->InsertPt = InsertPt;
  }

  /// setInsertionPoint - Set the insertion point to insert before the specified
  /// instruction.
  void setInsertionPoint(Instruction *I) {
    setInsertionPoint(I->getParent(), I);
  }

  /// setInsertionPoint - Set the insertion point to insert at the end of the
  /// specified block.
  void setInsertionPoint(BasicBlock *BB) {
    setInsertionPoint(BB, BB->end());
  }
    

};

} // end swift namespace

#endif
