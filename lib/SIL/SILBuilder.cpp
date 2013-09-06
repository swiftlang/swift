//===--- SILBuilder.h - Class for creating SIL Constructs --------*- C++ -*-==//
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

#include "swift/SIL/SILBuilder.h"
using namespace swift;

BranchInst *SILBuilder::createBranch(SILLocation Loc,
                                     SILBasicBlock *TargetBlock,
                                     OperandValueArrayRef Args) {
  SmallVector<SILValue, 6> ArgsCopy;
  ArgsCopy.reserve(Args.size());
  for (auto I = Args.begin(), E = Args.end(); I != E; ++I)
    ArgsCopy.push_back(*I);
  return createBranch(Loc, TargetBlock, ArgsCopy);
}

/// emitBlock - Move the specified block to the current insertion point (which
/// is the end of the function if there is no insertion point) and reset the
/// insertion point to point to the first instruction in the emitted block.
void SILBuilder::emitBlock(SILBasicBlock *BB, SILLocation BranchLoc) {
  SILFunction::iterator IP;

  // If this is a fall through into BB, emit the fall through branch.
  if (hasValidInsertionPoint()) {
    // Move the new block after the current one.
    IP = getInsertionBB();
    ++IP;

    // Emit the branch.
    assert(BB->bbarg_empty() && "cannot fall through to bb with args");
    assert(!BranchLoc.isNull());
    createBranch(BranchLoc, BB);
  } else {
    // If we don't have an insertion point, insert the block at the end of the
    // function
    IP = BB->getParent()->end();
  }

  // Start inserting into that block.
  setInsertionPoint(BB);

  // Move block to its new spot.
  moveBlockTo(BB, IP);
}

/// splitBlockForFallthrough - Prepare for the insertion of a terminator.  If
/// the builder's insertion point is at the end of the current block (as when
/// SILGen is creating the initial code for a function), just create and
/// return a new basic block that will be later used for the continue point.
///
/// If the insertion point is valid (i.e., pointing to an existing
/// instruction) then split the block at that instruction and return the
/// continuation block.
SILBasicBlock *SILBuilder::splitBlockForFallthrough() {
  // If we are concatenating, just create and return a new block.
  if (insertingAtEndOfBlock())
    return new (*F.getParent()) SILBasicBlock(&F);

  // Otherwise we need to split the current block at the insertion point.
  auto *NewBB = BB->splitBasicBlock(InsertPt);
  InsertPt = BB->end();
  return NewBB;
}

SILValue SILBuilder::emitGeneralizedValue(SILLocation loc, SILValue v) {
  // Thicken thin functions.
  if (v.getType().is<AnyFunctionType>() &&
      v.getType().castTo<AnyFunctionType>()->isThin()) {
    // Thunk functions to the standard "freestanding" calling convention.
    if (v.getType().getAbstractCC() != AbstractCC::Freestanding) {
      auto freestandingType =
        Lowering::getThinFunctionType(v.getType().getSwiftType(),
                                      AbstractCC::Freestanding);
      SILType freestandingSILType =
        F.getParent()->Types.getLoweredLoadableType(freestandingType, 0);
      v = createConvertCC(loc, v, freestandingSILType);
    }

    Type thickTy = Lowering::getThickFunctionType(v.getType().getSwiftType(),
                                                  AbstractCC::Freestanding);

    v = createThinToThickFunction(loc, v,
                       F.getParent()->Types.getLoweredLoadableType(thickTy));
  }

  return v;
}

