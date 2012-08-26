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

#ifndef SWIFT_CFG_LOWERING_JUMPDEST_H
#define SWIFT_CFG_LOWERING_JUMPDEST_H

#include "swift/Basic/DiverseStack.h"

namespace swift {
  class BasicBlock;
  
namespace CFGLowering {

/// FIXME: Temporary.
typedef int Cleanup;
  
//class Cleanup;
typedef DiverseStackImpl<Cleanup>::stable_iterator CleanupsDepth;

/// The destination of a direct jump.  Swift currently does not
/// support indirect branches or goto, so the jump mechanism only
/// needs to worry about branches into scopes, not out of them.
class JumpDest {
  BasicBlock *Block;
  CleanupsDepth Depth;
public:
  JumpDest(BasicBlock *block, CleanupsDepth depth)
    : Block(block), Depth(depth) {}

  BasicBlock *getBlock() const { return Block; }
  CleanupsDepth getDepth() const { return Depth; }
};

} // end namespace CFGLowering
} // end namespace swift

#endif
