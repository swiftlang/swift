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

//===----------------------------------------------------------------------===//
// SILBuilder Implementation
//===----------------------------------------------------------------------===//

SILType SILBuilder::getPartialApplyResultType(SILType origTy, unsigned argCount,
                                              SILModule &M) {
  CanSILFunctionType FTI = origTy.castTo<SILFunctionType>();
  auto params = FTI->getParameters();
  auto newParams = params.slice(0, params.size() - argCount);

  auto extInfo = SILFunctionType::ExtInfo(AbstractCC::Freestanding,
                                          /*thin*/ false,
                                          /*noreturn*/ FTI->isNoReturn(),
                                          /*autoclosure*/ false,
                                          /*block*/ false);

  auto appliedFnType = SILFunctionType::get(nullptr, extInfo,
                                            ParameterConvention::Direct_Owned,
                                            newParams,
                                            FTI->getResult(),
                                            M.getASTContext());
  return SILType::getPrimitiveObjectType(appliedFnType);
}

BranchInst *SILBuilder::createBranch(SILLocation Loc,
                                     SILBasicBlock *TargetBlock,
                                     OperandValueArrayRef Args) {
  SmallVector<SILValue, 6> ArgsCopy;
  ArgsCopy.reserve(Args.size());
  for (auto I = Args.begin(), E = Args.end(); I != E; ++I)
    ArgsCopy.push_back(*I);
  return createBranch(Loc, TargetBlock, ArgsCopy);
}

/// \brief Move the specified block to the end of the function and reset the
/// insertion point to point to the first instruction in the emitted block.
///
/// Assumes that no insertion point is currently active.
void SILBuilder::emitBlock(SILBasicBlock *BB) {
  assert(!hasValidInsertionPoint());

  // We don't have an insertion point, insert the block at the end of the
  // function.
  SILFunction::iterator IP = BB->getParent()->end();

  // Start inserting into that block.
  setInsertionPoint(BB);

  // Move block to its new spot.
  moveBlockTo(BB, IP);
}

/// \brief Move the specified block to the current insertion point (which
/// is the end of the function if there is no insertion point) and reset the
/// insertion point to point to the first instruction in the emitted block.
void SILBuilder::emitBlock(SILBasicBlock *BB, SILLocation BranchLoc) {
  if (!hasValidInsertionPoint()) {
    return emitBlock(BB);
  }

  // Fall though from the currently active block into the given block.
  assert(BB->bbarg_empty() && "cannot fall through to bb with args");

  // Move the new block after the current one.
  SILFunction::iterator IP = getInsertionBB();
  ++IP;

  // This is a fall through into BB, emit the fall through branch.
  createBranch(BranchLoc, BB);

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
    return new (F.getModule()) SILBasicBlock(&F);

  // Otherwise we need to split the current block at the insertion point.
  auto *NewBB = BB->splitBasicBlock(InsertPt);
  InsertPt = BB->end();
  return NewBB;
}

/// emitDestroyAddr - Try to fold a destroy_addr operation into the previous
/// instructions, or generate an explicit one if that fails.  If this inserts a
/// new instruction, it returns it, otherwise it returns null.
DestroyAddrInst *SILBuilder::emitDestroyAddr(SILLocation Loc, SILValue Operand){
  // Check to see if the instruction immediately before the insertion point is a
  // copy_addr from the specified operand.  If so, we can fold this into the
  // copy_addr as a take.
  auto I = getInsertionPoint(), BBStart = getInsertionBB()->begin();
  while (I != BBStart) {
    auto *Inst = &*--I;

    if (auto CA = dyn_cast<CopyAddrInst>(Inst)) {
      if (CA->getSrc() == Operand && !CA->isTakeOfSrc()) {
        CA->setIsTakeOfSrc(IsTake);
        return nullptr;
      }
    }

    // destroy_addrs commonly exist in a block of dealloc_stack's, which don't
    // affect take-ability.
    if (isa<DeallocStackInst>(Inst))
      continue;

    // This code doesn't try to prove tricky validity constraints about whether
    // it is safe to push the destroy_addr past interesting instructions.
    if (Inst->mayHaveSideEffects())
      break;
  }

  // If we didn't find a copy_addr to fold this into, emit the destroy_addr.
  return createDestroyAddr(Loc, Operand);
}

/// Perform a strong_release instruction at the current location, attempting
/// to fold it locally into nearby retain instructions or emitting an explicit
/// strong release if necessary.  If this inserts a new instruction, it
/// returns it, otherwise it returns null.
StrongReleaseInst *SILBuilder::emitStrongRelease(SILLocation Loc,
                                                 SILValue Operand) {
  // Release on a functionref is a noop.
  if (isa<FunctionRefInst>(Operand))
    return nullptr;

  // Check to see if the instruction immediately before the insertion point is a
  // strong_retain of the specified operand.  If so, we can zap the pair.
  auto I = getInsertionPoint(), BBStart = getInsertionBB()->begin();
  while (I != BBStart) {
    auto *Inst = &*--I;

    if (auto SRA = dyn_cast<StrongRetainInst>(Inst)) {
      if (SRA->getOperand() == Operand) {
        SRA->eraseFromParent();
        return nullptr;
      }
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple memory accesses.
    if (isa<LoadInst>(Inst) || isa<StoreInst>(Inst) ||
        isa<CopyValueInst>(Inst))
      continue;

    // This code doesn't try to prove tricky validity constraints about whether
    // it is safe to push the release past interesting instructions.
    if (Inst->mayHaveSideEffects())
      break;
  }

  // If we didn't find a retain to fold this into, emit the release.
  return createStrongRelease(Loc, Operand);
}


