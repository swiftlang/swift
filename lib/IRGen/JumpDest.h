//===--- JumpDest.h - Jump Destination Representation -----------*- C++ -*-===//
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
// Types relating to branch destinations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_JUMPDEST_H
#define SWIFT_IRGEN_JUMPDEST_H

#include "IRGenFunction.h"

namespace llvm {
  class BasicBlock;
}

namespace swift {
namespace irgen {

/// The destination of a direct jump.  Swift currently does not
/// support indirect branches or goto, so the jump mechanism only
/// needs to worry about branches into scopes, not out of them.
class JumpDest {
  llvm::BasicBlock *Block;
  IRGenFunction::CleanupsDepth Depth;
public:
  JumpDest(llvm::BasicBlock *block, IRGenFunction::CleanupsDepth depth)
    : Block(block), Depth(depth) {}

  llvm::BasicBlock *getBlock() const { return Block; }
  IRGenFunction::CleanupsDepth getDepth() const { return Depth; }
};

} // end namespace irgen
} // end namespace swift

#endif
