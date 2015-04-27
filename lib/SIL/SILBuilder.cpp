//===--- SILBuilder.cpp - Class for creating SIL Constructs ----------------==//
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
                                              SILModule &M,
                                              ArrayRef<Substitution> subs) {
  CanSILFunctionType FTI = origTy.castTo<SILFunctionType>();
  if (!subs.empty())
    FTI = FTI->substGenericArgs(M, M.getSwiftModule(), subs);
  
  assert(!FTI->isPolymorphic()
         && "must provide substitutions for generic partial_apply");
  auto params = FTI->getParameters();
  auto newParams = params.slice(0, params.size() - argCount);

  auto extInfo = SILFunctionType::ExtInfo(
                                        SILFunctionType::Representation::Thick,
                                        /*noreturn*/ FTI->isNoReturn());
  
  auto appliedFnType = SILFunctionType::get(nullptr, extInfo,
                                            ParameterConvention::Direct_Owned,
                                            newParams,
                                            FTI->getResult(),
                                            FTI->getOptionalErrorResult(),
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

/// \brief Branch to the given block if there's an active insertion point,
/// then move the insertion point to the end of that block.
void SILBuilder::emitBlock(SILBasicBlock *BB, SILLocation BranchLoc) {
  if (!hasValidInsertionPoint()) {
    return emitBlock(BB);
  }

  // Fall though from the currently active block into the given block.
  assert(BB->bbarg_empty() && "cannot fall through to bb with args");

  // This is a fall through into BB, emit the fall through branch.
  createBranch(BranchLoc, BB);

  // Start inserting into that block.
  setInsertionPoint(BB);
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
  if (insertingAtEndOfBlock()) {
    return new (F.getModule()) SILBasicBlock(&F, BB);
  }

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

static bool couldReduceStrongRefcount(SILInstruction *Inst) {
  // Simple memory accesses cannot reduce refcounts.
  if (isa<LoadInst>(Inst) || isa<StoreInst>(Inst) ||
      isa<RetainValueInst>(Inst) || isa<UnownedRetainInst>(Inst) ||
      isa<UnownedReleaseInst>(Inst) || isa<StrongRetainUnownedInst>(Inst) ||
      isa<StoreWeakInst>(Inst) || isa<StrongRetainInst>(Inst) ||
      isa<AllocStackInst>(Inst) || isa<DeallocStackInst>(Inst))
    return false;

  // Assign and copyaddr of trivial types cannot drop refcounts, and 'inits'
  // never can either.  Nontrivial ones can though, because the overwritten
  // value drops a retain.  We would have to do more alias analysis to be able
  // to safely ignore one of those.
  if (auto AI = dyn_cast<AssignInst>(Inst)) {
    auto StoredType = AI->getOperand(0).getType();
    if (StoredType.isTrivial(Inst->getModule()) ||
        StoredType.is<ReferenceStorageType>())
      return false;
  }

  if (auto *CAI = dyn_cast<CopyAddrInst>(Inst)) {
    // Initializations can only increase refcounts.
    if (CAI->isInitializationOfDest())
      return false;

    SILType StoredType = CAI->getOperand(0).getType().getObjectType();
    if (StoredType.isTrivial(Inst->getModule()) ||
        StoredType.is<ReferenceStorageType>())
      return false;
  }

  // This code doesn't try to prove tricky validity constraints about whether
  // it is safe to push the release past interesting instructions.
  return Inst->mayHaveSideEffects();
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
        DeleteInst(SRA);
        return nullptr;
      }
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple instructions that cannot reduce strong refcounts.
    if (couldReduceStrongRefcount(Inst))
      break;
  }

  // If we didn't find a retain to fold this into, emit the release.
  return createStrongRelease(Loc, Operand);
}

/// Emit a release_value instruction at the current location, attempting to
/// fold it locally into another nearby retain_value instruction.  This
/// returns the new instruction if it inserts one, otherwise it returns null.
ReleaseValueInst *
SILBuilder::emitReleaseValue(SILLocation Loc, SILValue Operand) {
  // Check to see if the instruction immediately before the insertion point is a
  // retain_value of the specified operand.  If so, we can zap the pair.
  auto I = getInsertionPoint(), BBStart = getInsertionBB()->begin();
  while (I != BBStart) {
    auto *Inst = &*--I;

    if (auto SRA = dyn_cast<RetainValueInst>(Inst)) {
      if (SRA->getOperand() == Operand) {
        DeleteInst(SRA);
        return nullptr;
      }
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple instructions that cannot reduce refcounts.
    if (couldReduceStrongRefcount(Inst))
      break;
  }

  // If we didn't find a retain to fold this into, emit the release.
  return createReleaseValue(Loc, Operand);
}


SILValue SILBuilder::emitThickToObjCMetatype(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
  // If the operand is an otherwise-unused 'metatype' instruction in the
  // same basic block, zap it and create a 'metatype' instruction that
  // directly produces an Objective-C metatype.
  if (auto metatypeInst = dyn_cast<MetatypeInst>(Op)) {
    if (metatypeInst->use_empty() &&
        metatypeInst->getParent() == getInsertionBB()) {
      auto origLoc = metatypeInst->getLoc();
      metatypeInst->removeFromParent();
      return createMetatype(origLoc, Ty);
    }
  }

  // Just create the thick_to_objc_metatype instruction.
  return createThickToObjCMetatype(Loc, Op, Ty);
}

SILValue SILBuilder::emitObjCToThickMetatype(SILLocation Loc, SILValue Op,
                                             SILType Ty) {
  // If the operand is an otherwise-unused 'metatype' instruction in the
  // same basic block, zap it and create a 'metatype' instruction that
  // directly produces a thick metatype.
  if (auto metatypeInst = dyn_cast<MetatypeInst>(Op)) {
    if (metatypeInst->use_empty() &&
        metatypeInst->getParent() == getInsertionBB()) {
      auto origLoc = metatypeInst->getLoc();
      metatypeInst->removeFromParent();
      return createMetatype(origLoc, Ty);
    }
  }

  // Just create the objc_to_thick_metatype instruction.
  return createObjCToThickMetatype(Loc, Op, Ty);
}
