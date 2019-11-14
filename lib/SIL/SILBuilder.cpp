//===--- SILBuilder.cpp - Class for creating SIL Constructs ---------------===//
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

#include "swift/SIL/SILBuilder.h"
#include "swift/AST/Expr.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILGlobalVariable.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// SILBuilder Implementation
//===----------------------------------------------------------------------===//

SILBuilder::SILBuilder(SILGlobalVariable *GlobVar,
                       SmallVectorImpl<SILInstruction *> *InsertedInstrs)
    : TempContext(GlobVar->getModule(), InsertedInstrs), C(TempContext),
      F(nullptr) {
  setInsertionPoint(&GlobVar->StaticInitializerBlock);
}

IntegerLiteralInst *SILBuilder::createIntegerLiteral(IntegerLiteralExpr *E) {
  return insert(IntegerLiteralInst::create(E, getSILDebugLocation(E),
                                           getModule()));
}

FloatLiteralInst *SILBuilder::createFloatLiteral(FloatLiteralExpr *E) {
  return insert(FloatLiteralInst::create(E, getSILDebugLocation(E),
                                         getModule()));
}

TupleInst *SILBuilder::createTuple(SILLocation loc, ArrayRef<SILValue> elts) {
  // Derive the tuple type from the elements.
  SmallVector<TupleTypeElt, 4> eltTypes;
  for (auto elt : elts)
    eltTypes.push_back(elt->getType().getASTType());
  auto tupleType = SILType::getPrimitiveObjectType(
      CanType(TupleType::get(eltTypes, getASTContext())));

  return createTuple(loc, tupleType, elts);
}

SILType SILBuilder::getPartialApplyResultType(
    TypeExpansionContext context, SILType origTy, unsigned argCount,
    SILModule &M, SubstitutionMap subs, ParameterConvention calleeConvention,
    PartialApplyInst::OnStackKind onStack) {
  CanSILFunctionType FTI = origTy.castTo<SILFunctionType>();
  if (!subs.empty())
    FTI = FTI->substGenericArgs(M, subs, context);

  assert(!FTI->isPolymorphic()
         && "must provide substitutions for generic partial_apply");
  auto params = FTI->getParameters();
  auto newParams = params.slice(0, params.size() - argCount);

  auto extInfo = FTI->getExtInfo()
    .withRepresentation(SILFunctionType::Representation::Thick)
    .withIsPseudogeneric(false);
  if (onStack)
    extInfo = extInfo.withNoEscape();

  // If the original method has an @unowned_inner_pointer return, the partial
  // application thunk will lifetime-extend 'self' for us, converting the
  // return value to @unowned.
  //
  // If the original method has an @autoreleased return, the partial application
  // thunk will retain it for us, converting the return value to @owned.
  SmallVector<SILResultInfo, 4> results;
  results.append(FTI->getResults().begin(), FTI->getResults().end());
  for (auto &result : results) {
    if (result.getConvention() == ResultConvention::UnownedInnerPointer)
      result = SILResultInfo(result.getReturnValueType(M, FTI),
                             ResultConvention::Unowned);
    else if (result.getConvention() == ResultConvention::Autoreleased)
      result = SILResultInfo(result.getReturnValueType(M, FTI),
                             ResultConvention::Owned);
  }

  auto appliedFnType = SILFunctionType::get(nullptr, extInfo,
                                            FTI->getCoroutineKind(),
                                            calleeConvention,
                                            newParams,
                                            FTI->getYields(),
                                            results,
                                            FTI->getOptionalErrorResult(),
                                            SubstitutionMap(),
                                            false,
                                            M.getASTContext());

  return SILType::getPrimitiveObjectType(appliedFnType);
}

ProjectBoxInst *SILBuilder::createProjectBox(SILLocation Loc,
                                             SILValue boxOperand,
                                             unsigned index) {
  auto boxTy = boxOperand->getType().castTo<SILBoxType>();
  auto fieldTy = getSILBoxFieldType(getTypeExpansionContext(), boxTy,
                                    getModule().Types, index);

  return insert(new (getModule()) ProjectBoxInst(
      getSILDebugLocation(Loc), boxOperand, index, fieldTy));
}

// If legal, create an unchecked_ref_cast from the given operand and result
// type, otherwise return null.
SingleValueInstruction *
SILBuilder::tryCreateUncheckedRefCast(SILLocation Loc, SILValue Op,
                                      SILType ResultTy) {
  if (!SILType::canRefCast(Op->getType(), ResultTy, getModule()))
    return nullptr;

  return insert(UncheckedRefCastInst::create(getSILDebugLocation(Loc), Op,
                                             ResultTy, getFunction(),
                                             C.OpenedArchetypes));
}

ClassifyBridgeObjectInst *
SILBuilder::createClassifyBridgeObject(SILLocation Loc, SILValue value) {
  auto &ctx = getASTContext();
  Type int1Ty = BuiltinIntegerType::get(1, ctx);
  Type resultTy = TupleType::get({ int1Ty, int1Ty }, ctx);
  auto ty = SILType::getPrimitiveObjectType(resultTy->getCanonicalType());
  return insert(new (getModule())
                ClassifyBridgeObjectInst(getSILDebugLocation(Loc), value, ty));
}


// Create the appropriate cast instruction based on result type.
SingleValueInstruction *
SILBuilder::createUncheckedBitCast(SILLocation Loc, SILValue Op, SILType Ty) {
  assert(isLoadableOrOpaque(Ty));
  if (Ty.isTrivial(getFunction()))
    return insert(UncheckedTrivialBitCastInst::create(
        getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes));

  if (auto refCast = tryCreateUncheckedRefCast(Loc, Op, Ty))
    return refCast;

  // The destination type is nontrivial, and may be smaller than the source
  // type, so RC identity cannot be assumed.
  return insert(UncheckedBitwiseCastInst::create(
      getSILDebugLocation(Loc), Op, Ty, getFunction(), C.OpenedArchetypes));
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

/// Branch to the given block if there's an active insertion point,
/// then move the insertion point to the end of that block.
void SILBuilder::emitBlock(SILBasicBlock *BB, SILLocation BranchLoc) {
  if (!hasValidInsertionPoint()) {
    return emitBlock(BB);
  }

  // Fall though from the currently active block into the given block.
  assert(BB->args_empty() && "cannot fall through to bb with args");

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
    return getFunction().createBasicBlockAfter(BB);
  }

  // Otherwise we need to split the current block at the insertion point.
  auto *NewBB = BB->split(InsertPt);
  InsertPt = BB->end();
  return NewBB;
}

static bool setAccessToDeinit(BeginAccessInst *beginAccess) {
  // It's possible that AllocBoxToStack could catch some cases that
  // AccessEnforcementSelection does not promote to [static]. Ultimately, this
  // should be an assert, but only after we the two passes can be fixed to share
  // a common analysis.
  if (beginAccess->getEnforcement() == SILAccessEnforcement::Dynamic)
    return false;

  beginAccess->setAccessKind(SILAccessKind::Deinit);
  return true;
}

PointerUnion<CopyAddrInst *, DestroyAddrInst *>
SILBuilder::emitDestroyAddr(SILLocation Loc, SILValue Operand) {
  // Check to see if the instruction immediately before the insertion point is a
  // copy_addr from the specified operand.  If so, we can fold this into the
  // copy_addr as a take.
  BeginAccessInst *beginAccess = nullptr;
  CopyAddrInst *copyAddrTake = nullptr;
  auto I = getInsertionPoint(), BBStart = getInsertionBB()->begin();
  while (I != BBStart) {
    auto *Inst = &*--I;

    if (auto CA = dyn_cast<CopyAddrInst>(Inst)) {
      if (!CA->isTakeOfSrc()) {
        if (CA->getSrc() == Operand && !CA->isTakeOfSrc()) {
          CA->setIsTakeOfSrc(IsTake);
          return CA;
        }
        // If this copy_addr is accessing the same source, continue searching
        // backward until we see the begin_access. If any side effects occur
        // between the `%adr = begin_access %src` and `copy_addr %adr` then we
        // cannot promote the access to a deinit. `[deinit]` requires exclusive
        // access, but an instruction with side effects may require shared
        // access.
        if (CA->getSrc() == beginAccess) {
          copyAddrTake = CA;
          continue;
        }
      }
    }

    // If we've already seen a copy_addr that can be convert to `take`, then
    // stop at the begin_access for the copy's source.
    if (copyAddrTake && beginAccess == Inst) {
      // If `setAccessToDeinit()` returns `true` it has modified the access
      // instruction, so we are committed to the transformation on that path.
      if (setAccessToDeinit(beginAccess)) {
        copyAddrTake->setIsTakeOfSrc(IsTake);
        return copyAddrTake;
      }
    }

    // destroy_addrs commonly exist in a block of dealloc_stack's, which don't
    // affect take-ability.
    if (isa<DeallocStackInst>(Inst))
      continue;

    // An end_access of the same address may be able to be rewritten as a
    // [deinit] access.
    if (auto endAccess = dyn_cast<EndAccessInst>(Inst)) {
      if (endAccess->getSource() == Operand) {
        beginAccess = endAccess->getBeginAccess();
        continue;
      }
    }

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
  switch (Inst->getKind()) {
#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Store##Name##Inst: \
    return false;
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  /* The next case must be first in this macro because */ \
  /* SOMETIMES_LOADABLE_CHECKED_REF_STORAGE will fall into it. */ \
  case SILInstructionKind::Name##ReleaseInst: \
    if (isLessStrongThan(ReferenceOwnership::Name, ReferenceOwnership::Strong))\
      return false; \
    break; \
  case SILInstructionKind::Name##RetainInst: \
  case SILInstructionKind::StrongRetain##Name##Inst: \
    return false;
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Store##Name##Inst: \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, "...")
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  case SILInstructionKind::StrongCopy##Name##ValueInst:                        \
    return false;
#include "swift/AST/ReferenceStorage.def"
  case SILInstructionKind::LoadInst:
  case SILInstructionKind::StoreInst:
  case SILInstructionKind::RetainValueInst:
  case SILInstructionKind::StrongRetainInst:
  case SILInstructionKind::AllocStackInst:
  case SILInstructionKind::DeallocStackInst:
    return false;
  default:
    break;
  }

  // Assign and copyaddr of trivial types cannot drop refcounts, and 'inits'
  // never can either.  Nontrivial ones can though, because the overwritten
  // value drops a retain.  We would have to do more alias analysis to be able
  // to safely ignore one of those.
  if (auto AI = dyn_cast<AssignInst>(Inst)) {
    auto StoredType = AI->getOperand(0)->getType();
    if (StoredType.isTrivial(*Inst->getFunction()) ||
        StoredType.is<ReferenceStorageType>())
      return false;
  }

  if (auto *CAI = dyn_cast<CopyAddrInst>(Inst)) {
    // Initializations can only increase refcounts.
    if (CAI->isInitializationOfDest())
      return false;

    SILType StoredType = CAI->getOperand(0)->getType().getObjectType();
    if (StoredType.isTrivial(*Inst->getFunction()) ||
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
PointerUnion<StrongRetainInst *, StrongReleaseInst *>
SILBuilder::emitStrongRelease(SILLocation Loc, SILValue Operand) {
  // Release on a functionref is a noop.
  if (isa<FunctionRefInst>(Operand)) {
    return static_cast<StrongReleaseInst *>(nullptr);
  }

  // Check to see if the instruction immediately before the insertion point is a
  // strong_retain of the specified operand.  If so, we can zap the pair.
  auto I = getInsertionPoint(), BBStart = getInsertionBB()->begin();
  while (I != BBStart) {
    auto *Inst = &*--I;

    if (auto *SRA = dyn_cast<StrongRetainInst>(Inst)) {
      if (SRA->getOperand() == Operand)
        return SRA;
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple instructions that cannot reduce strong refcounts.
    if (couldReduceStrongRefcount(Inst))
      break;
  }

  // If we didn't find a retain to fold this into, emit the release.
  return createStrongRelease(Loc, Operand, getDefaultAtomicity());
}

/// Emit a release_value instruction at the current location, attempting to
/// fold it locally into another nearby retain_value instruction.  This
/// returns the new instruction if it inserts one, otherwise it returns null.
PointerUnion<RetainValueInst *, ReleaseValueInst *>
SILBuilder::emitReleaseValue(SILLocation Loc, SILValue Operand) {
  // Check to see if the instruction immediately before the insertion point is a
  // retain_value of the specified operand.  If so, we can zap the pair.
  auto I = getInsertionPoint(), BBStart = getInsertionBB()->begin();
  while (I != BBStart) {
    auto *Inst = &*--I;

    if (auto *SRA = dyn_cast<RetainValueInst>(Inst)) {
      if (SRA->getOperand() == Operand)
        return SRA;
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple instructions that cannot reduce refcounts.
    if (couldReduceStrongRefcount(Inst))
      break;
  }

  // If we didn't find a retain to fold this into, emit the release.
  return createReleaseValue(Loc, Operand, getDefaultAtomicity());
}

PointerUnion<CopyValueInst *, DestroyValueInst *>
SILBuilder::emitDestroyValue(SILLocation Loc, SILValue Operand) {
  // Check to see if the instruction immediately before the insertion point is a
  // retain_value of the specified operand.  If so, we can zap the pair.
  auto I = getInsertionPoint(), BBStart = getInsertionBB()->begin();
  while (I != BBStart) {
    auto *Inst = &*--I;

    if (auto *CVI = dyn_cast<CopyValueInst>(Inst)) {
      if (SILValue(CVI) == Operand || CVI->getOperand() == Operand)
        return CVI;
      // Skip past unrelated retains.
      continue;
    }

    // Scan past simple instructions that cannot reduce refcounts.
    if (couldReduceStrongRefcount(Inst))
      break;
  }

  // If we didn't find a retain to fold this into, emit the release.
  return createDestroyValue(Loc, Operand);
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
      metatypeInst->eraseFromParent();
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
      metatypeInst->eraseFromParent();
      return createMetatype(origLoc, Ty);
    }
  }

  // Just create the objc_to_thick_metatype instruction.
  return createObjCToThickMetatype(Loc, Op, Ty);
}

/// Add opened archetypes defined or used by the current instruction.
/// If there are no such opened archetypes in the current instruction
/// and it is an instruction with just one operand, try to perform
/// the same action for the instruction defining an operand, because
/// it may have some opened archetypes used or defined.
void SILBuilder::addOpenedArchetypeOperands(SILInstruction *I) {
  // The list of archetypes from the previous instruction needs
  // to be replaced, because it may reference a removed instruction.
  C.OpenedArchetypes.addOpenedArchetypeOperands(I->getTypeDependentOperands());
  if (I && I->getNumTypeDependentOperands() > 0)
    return;

  // Keep track of already visited instructions to avoid infinite loops.
  SmallPtrSet<SILInstruction *, 8> Visited;

  while (I && I->getNumOperands() == 1 &&
         I->getNumTypeDependentOperands() == 0) {
    // All the open instructions are single-value instructions.  Operands may
    // be null when code is being transformed.
    auto SVI = dyn_cast_or_null<SingleValueInstruction>(I->getOperand(0));
    // Within SimplifyCFG this function may be called for an instruction
    // within unreachable code. And within an unreachable block it can happen
    // that defs do not dominate uses (because there is no dominance defined).
    // To avoid the infinite loop when following the chain of instructions via
    // their operands, bail if the operand is not an instruction or this
    // instruction was seen already.
    if (!SVI || !Visited.insert(SVI).second)
      return;
    // If it is a definition of an opened archetype,
    // register it and exit.
    auto Archetype = getOpenedArchetypeOf(SVI);
    if (!Archetype) {
      I = SVI;
      continue;
    }
    auto Def = C.OpenedArchetypes.getOpenedArchetypeDef(Archetype);
    // Return if it is a known open archetype.
    if (Def)
      return;
    // Otherwise register it and return.
    if (C.OpenedArchetypesTracker)
      C.OpenedArchetypesTracker->addOpenedArchetypeDef(Archetype, SVI);
    return;
  }

  if (I && I->getNumTypeDependentOperands() > 0) {
    C.OpenedArchetypes.addOpenedArchetypeOperands(
        I->getTypeDependentOperands());
  }
}

ValueMetatypeInst *SILBuilder::createValueMetatype(SILLocation Loc,
                                                   SILType MetatypeTy,
                                                   SILValue Base) {
  assert(Base->getType().isLoweringOf(
             getTypeExpansionContext(), getModule(),
             MetatypeTy.castTo<MetatypeType>().getInstanceType()) &&
         "value_metatype result must be formal metatype of the lowered operand "
         "type");
  return insert(new (getModule()) ValueMetatypeInst(getSILDebugLocation(Loc),
                                                      MetatypeTy, Base));
}

// TODO: This should really be an operation on type lowering.
void SILBuilder::emitDestructureValueOperation(
    SILLocation loc, SILValue v, SmallVectorImpl<SILValue> &results) {
  // Once destructure is allowed everywhere, remove the projection code.

  // If we do not have a tuple or a struct, add to our results list and return.
  SILType type = v->getType();
  if (!(type.is<TupleType>() || type.getStructOrBoundGenericStruct())) {
    results.emplace_back(v);
    return;
  }

  // Otherwise, we want to destructure add the destructure and return.
  if (getFunction().hasOwnership()) {
    auto *i = emitDestructureValueOperation(loc, v);
    llvm::copy(i->getResults(), std::back_inserter(results));
    return;
  }

  // In non qualified ownership SIL, drop back to using projection code.
  SmallVector<Projection, 16> projections;
  Projection::getFirstLevelProjections(v->getType(), getModule(),
                                       getTypeExpansionContext(), projections);
  llvm::transform(projections, std::back_inserter(results),
                  [&](const Projection &p) -> SILValue {
                    return p.createObjectProjection(*this, loc, v).get();
                  });
}

// TODO: Can we put this on type lowering? It would take a little bit of work
// since we would need to be able to handle aggregate trivial types which is not
// represented today in TypeLowering.
void SILBuilder::emitDestructureAddressOperation(
    SILLocation loc, SILValue v, SmallVectorImpl<SILValue> &results) {

  // If we do not have a tuple or a struct, add to our results list.
  SILType type = v->getType();
  if (!(type.is<TupleType>() || type.getStructOrBoundGenericStruct())) {
    results.emplace_back(v);
    return;
  }

  SmallVector<Projection, 16> projections;
  Projection::getFirstLevelProjections(v->getType(), getModule(),
                                       getTypeExpansionContext(), projections);
  llvm::transform(projections, std::back_inserter(results),
                  [&](const Projection &p) -> SILValue {
                    return p.createAddressProjection(*this, loc, v).get();
                  });
}

void SILBuilder::emitDestructureValueOperation(
    SILLocation loc, SILValue operand,
    function_ref<void(unsigned, SILValue)> func) {
  // Do a quick check to see if we have a tuple without elements. In that
  // case, bail early since we are not going to ever invoke Func.
  if (auto tupleType = operand->getType().getAs<TupleType>())
    if (0 == tupleType->getNumElements())
      return;

  SmallVector<SILValue, 8> results;
  emitDestructureValueOperation(loc, operand, results);

  for (auto p : llvm::enumerate(results)) {
    func(p.index(), p.value());
  }
}

DebugValueInst *SILBuilder::createDebugValue(SILLocation Loc, SILValue src,
                                             SILDebugVariable Var) {
  assert(isLoadableOrOpaque(src->getType()));
  // Debug location overrides cannot apply to debug value instructions.
  DebugLocOverrideRAII LocOverride{*this, None};
  return insert(
      DebugValueInst::create(getSILDebugLocation(Loc), src, getModule(), Var));
}

DebugValueAddrInst *SILBuilder::createDebugValueAddr(SILLocation Loc,
                                                     SILValue src,
                                                     SILDebugVariable Var) {
  // Debug location overrides cannot apply to debug addr instructions.
  DebugLocOverrideRAII LocOverride{*this, None};
  return insert(DebugValueAddrInst::create(getSILDebugLocation(Loc), src,
                                           getModule(), Var));
}

void SILBuilder::emitScopedBorrowOperation(SILLocation loc, SILValue original,
                                           function_ref<void(SILValue)> &&fun) {
  if (original->getType().isAddress()) {
    original = createLoadBorrow(loc, original);
  } else {
    original = createBeginBorrow(loc, original);
  }

  fun(original);

  createEndBorrow(loc, original);
}

CheckedCastBranchInst *SILBuilder::createCheckedCastBranch(
    SILLocation Loc, bool isExact, SILValue op, SILType destTy,
    SILBasicBlock *successBB, SILBasicBlock *failureBB,
    ProfileCounter target1Count, ProfileCounter target2Count) {
  assert((!hasOwnership() || !failureBB->getNumArguments() ||
          failureBB->getArgument(0)->getType() == op->getType()) &&
         "failureBB's argument doesn't match incoming argument type");
  return insertTerminator(CheckedCastBranchInst::create(
      getSILDebugLocation(Loc), isExact, op, destTy, successBB, failureBB,
      getFunction(), C.OpenedArchetypes, target1Count, target2Count));
}
