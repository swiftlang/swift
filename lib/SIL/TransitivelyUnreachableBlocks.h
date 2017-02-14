//===--- TransitivelyUnreachableBlocksInfo.h
//----------------------------------===//
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

#ifndef SWIFT_SIL_TRANSITIVELYUNREACHABLEBLOCKS_H
#define SWIFT_SIL_TRANSITIVELYUNREACHABLEBLOCKS_H

#include "swift/SIL/PostOrder.h"
#include "swift/SIL/SILBasicBlock.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {

/// This is a simple unreachable dataflow analysis that determines all blocks
/// that are post-dominated by no-return functions, no-return builtins and
/// unreachables. It is needed to determine if the a value definition can have a
/// lack of users ignored along a specific path.
class TransitivelyUnreachableBlocksInfo {
  llvm::SmallPtrSet<const SILBasicBlock *, 32> UnreachableBlocks;

public:
  TransitivelyUnreachableBlocksInfo(const PostOrderFunctionInfo &POFI) {
    for (SILBasicBlock *Block : POFI.getPostOrder()) {
      if (isa<UnreachableInst>(Block->getTerminator())) {
        UnreachableBlocks.insert(Block);
        continue;
      }

      if (Block->succ_empty()) {
        continue;
      }

      if (any_of(Block->getSuccessorBlocks(),
                 [this](SILBasicBlock *SuccBlock) -> bool {
                   return !UnreachableBlocks.count(SuccBlock);
                 })) {
        continue;
      }

      UnreachableBlocks.insert(Block);
    }
  }

  bool isUnreachable(const SILBasicBlock *BB) const {
    return UnreachableBlocks.count(BB);
  }
};

} // namespace swift

#endif
