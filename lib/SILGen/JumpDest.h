//===--- JumpDest.h - Jump Destination Representation -----------*- C++ -*-===//
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
//
// Types relating to branch destinations.
//
//===----------------------------------------------------------------------===//

#ifndef JUMPDEST_H
#define JUMPDEST_H

#include "swift/SIL/SILLocation.h"
#include "llvm/Support/Compiler.h"
#include "Cleanup.h"

namespace swift {
  class SILBasicBlock;
  class CaseStmt;
  
namespace Lowering {

/// The destination of a direct jump.  Swift currently does not
/// support indirect branches or goto, so the jump mechanism only
/// needs to worry about branches out of scopes, not into them.
class LLVM_LIBRARY_VISIBILITY JumpDest {
  SILBasicBlock *Block = nullptr;
  CleanupsDepth Depth = CleanupsDepth::invalid();
  CleanupLocation CleanupLoc;
public:
  JumpDest(CleanupLocation L) : CleanupLoc(L) {}
  
  JumpDest(SILBasicBlock *block, CleanupsDepth depth, CleanupLocation l)
    : Block(block), Depth(depth), CleanupLoc(l) {}

  SILBasicBlock *getBlock() const { return Block; }
  SILBasicBlock *takeBlock() {
    auto *BB = Block;
    Block = nullptr;
    return BB;
  }
  CleanupsDepth getDepth() const { return Depth; }
  CleanupLocation getCleanupLocation() const { return CleanupLoc; }

  bool isValid() const { return Block != nullptr; }
  static JumpDest invalid() {
    return JumpDest(CleanupLocation((Expr*) nullptr));
  }
};
  
} // end namespace Lowering
} // end namespace swift

#endif
