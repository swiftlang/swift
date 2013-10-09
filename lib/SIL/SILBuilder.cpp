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

SILType SILBuilder::getTupleElementType(SILType Ty, unsigned EltNo) {
  TupleType *TT = Ty.getAs<TupleType>();
  auto EltTy = TT->getFields()[EltNo].getType()->getCanonicalType();
  return SILType::getPrimitiveObjectType(EltTy);
}

SILType SILBuilder::getStructFieldType(VarDecl *Field) {
  auto FieldTy = Field->getType()->getCanonicalType();
  return SILType::getPrimitiveObjectType(FieldTy);
}

SILType SILBuilder::getPartialApplyResultType(SILType origTy, unsigned argCount,
                                              SILModule &M) {
  SILFunctionType *FTI = origTy.getFunctionTypeInfo(M);
  auto params = FTI->getNonReturnParameters();
  auto newParams = params.slice(0, params.size() - argCount);

  SmallVector<TupleTypeElt, 4> newArgTypes;
  newArgTypes.reserve(newParams.size());
  for (auto param : newParams) {
    Type type = param.getType();
    if (param.isIndirectInOut())
      type = param.getSILType().getSwiftType();
    newArgTypes.push_back(type);
  }

  Type argTy = TupleType::get(newArgTypes, M.getASTContext());
  Type resTy = FunctionType::get(argTy, origTy.getAs<AnyFunctionType>().getResult(),
                                 M.getASTContext());
  return M.Types.getLoweredType(resTy);
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
        F.getModule().Types.getLoweredLoadableType(freestandingType, 0);
      v = createConvertCC(loc, v, freestandingSILType);
    }

    Type thickTy = Lowering::getThickFunctionType(v.getType().getSwiftType(),
                                                  AbstractCC::Freestanding);

    v = createThinToThickFunction(loc, v,
                           F.getModule().Types.getLoweredLoadableType(thickTy));
  }

  return v;
}


/// emitDestroyAddr - Try to fold a destroy_addr operation into the previous
/// instructions, or generate an explicit one if that fails.
void SILBuilder::emitDestroyAddr(SILLocation Loc, SILValue Operand) {
  // Check to see if the instruction immediately before the insertion point is a
  // copy_addr from the specified operand.  If so, we can fold this into the
  // copy_addr as a take.
  auto I = getInsertionPoint(), BBStart = getInsertionBB()->begin();
  while (I != BBStart) {
    auto *Inst = &*--I;

    if (auto CA = dyn_cast<CopyAddrInst>(Inst)) {
      if (CA->getSrc() == Operand && !CA->isTakeOfSrc()) {
        CA->setIsTakeOfSrc(IsTake);
        return;
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
  createDestroyAddr(Loc, Operand);
}


