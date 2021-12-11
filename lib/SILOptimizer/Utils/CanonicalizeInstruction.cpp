//===--- CanonicalizeInstruction.cpp - canonical SIL peepholes ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// SSA-peephole transformations that yield a more canonical SIL representation.
///
/// A superset of simplifyInstruction.
///
//===----------------------------------------------------------------------===//

// CanonicalizeInstruction defines a default DEBUG_TYPE: "sil-canonicalize"

#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/AST/Type.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/ValueUtils.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "swift/SILOptimizer/Utils/DebugOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// Tracing within the implementation can also be activiated by the pass.
#define DEBUG_TYPE pass.debugType

// Vtable anchor.
CanonicalizeInstruction::~CanonicalizeInstruction() {}

// Helper to delete an instruction, or mark it for deletion.
//
// Return an iterator to the next non-deleted instruction. The incoming iterator
// may already have advanced beyond 'inst'.
static SILBasicBlock::iterator killInstruction(SILInstruction *inst,
                                               SILBasicBlock::iterator nextII,
                                               CanonicalizeInstruction &pass) {
  if (nextII == inst->getIterator())
    ++nextII;
  pass.killInstruction(inst);
  return nextII;
}

// Helper to delete, or mark for deletion, an instruction with potential debug
// or end of scope uses. All "real" uses must already be removed.
//
// fix_lifetime uses are not currently handled here. They are generally
// (incorrectly) treated as "incidental" uses, but no canonicalizations need
// them yet.
static SILBasicBlock::iterator
killInstAndIncidentalUses(SingleValueInstruction *inst,
                          SILBasicBlock::iterator nextII,
                          CanonicalizeInstruction &pass) {
  while (!inst->use_empty()) {
    auto *user = inst->use_begin()->getUser();
    assert(user->isDebugInstruction() || isEndOfScopeMarker(user));
    nextII = killInstruction(user, nextII, pass);
  }
  return killInstruction(inst, nextII, pass);
}

//===----------------------------------------------------------------------===//
//                          Instruction Simplification
//===----------------------------------------------------------------------===//

// If simplification is successful, return a valid iterator to the next
// intruction that wasn't erased.
static Optional<SILBasicBlock::iterator>
simplifyAndReplace(SILInstruction *inst, CanonicalizeInstruction &pass) {
  // Erase the simplified instruction and any instructions that end its
  // scope. Nothing needs to be added to the worklist except for Result,
  // because the instruction and all non-replaced users will be deleted.
  pass.callbacks.resetHadCallbackInvocation();
  auto result = simplifyAndReplaceAllSimplifiedUsesAndErase(
      inst, pass.callbacks, &pass.deadEndBlocks);
  if (!pass.callbacks.hadCallbackInvocation())
    return None;

  return result;
}

//===----------------------------------------------------------------------===//
//                        Canonicalize Memory Operations
//===----------------------------------------------------------------------===//

// Replace all uses of an original struct or tuple extract instruction with the
// given load instruction. The caller ensures that the load only loads the
// extracted field.
//
// \p extract has the form:
// (struct_extract (load %base), #field)
//
// \p loadInst has the form:
// (load (struct_element_addr %base, #field)
static void replaceUsesOfExtract(SingleValueInstruction *extract,
                                 LoadOperation loadInst,
                                 CanonicalizeInstruction &pass) {
  assert(extract->getType() == loadInst->getType());

  SingleValueInstruction *loadedVal = *loadInst;
  if (auto qual = loadInst.getOwnershipQualifier()) {
    if (*qual == LoadOwnershipQualifier::Copy) {
      // Borrow the load-copied subelement, with precisely the same scope as
      // the aggregate borrow.
      assert(extract->getNumOperands() == 1);
      auto *origBorrow = cast<BeginBorrowInst>(extract->getOperand(0));
      auto *newBorrow = SILBuilderWithScope(origBorrow)
                            .createBeginBorrow(loadInst->getLoc(), *loadInst);
      pass.notifyNewInstruction(newBorrow);

      assert(extract == origBorrow->getSingleNonEndingUse()->getUser());
      for (auto *origEnd : origBorrow->getEndBorrows()) {
        auto *endBorrow = SILBuilderWithScope(origEnd).createEndBorrow(
            origEnd->getLoc(), newBorrow);
        pass.notifyNewInstruction(endBorrow);
      }
      loadedVal = newBorrow;
    }
  }
  LLVM_DEBUG(llvm::dbgs() << "Replacing " << *extract << "    with "
                          << *loadedVal << "\n");
  extract->replaceAllUsesWith(loadedVal);
}

// Given a load with multiple struct_extracts/tuple_extracts and no other uses,
// canonicalize the load into several (struct_element_addr (load)) pairs.
//
// (struct_extract (load %base))
//   ->
// (load (struct_element_addr %base, #field)
//
// TODO: Consider handling LoadBorrowInst.
static SILBasicBlock::iterator
splitAggregateLoad(LoadOperation loadInst, CanonicalizeInstruction &pass) {
  // Keep track of the next iterator after any newly added or to-be-deleted
  // instructions. This must be valid regardless of whether the pass immediately
  // deletes the instructions or simply records them for later deletion.
  auto nextII = std::next(loadInst->getIterator());

  bool needsBorrow;
  if (auto qual = loadInst.getOwnershipQualifier()) {
    switch (*qual) {
    case LoadOwnershipQualifier::Unqualified:
    case LoadOwnershipQualifier::Trivial:
      needsBorrow = false;
      break;
    case LoadOwnershipQualifier::Copy:
      needsBorrow = true;
      break;
    case LoadOwnershipQualifier::Take:
      // TODO: To handle a "take", we would need to generate additional destroys
      // for any fields that aren't already extracted. This would be
      // out-of-place for this transform, and I'm not sure if this a case that
      // needs to be handled in CanonicalizeInstruction.
      return nextII;
    }
  } else {
    // If we don't have a qual, we have a borrow.
    needsBorrow = false;
  }

  struct ProjInstPair {
    Projection proj;
    SingleValueInstruction *extract;

    // When sorting, just look at the projection and ignore the instruction.
    // Including the instruction address in the sort key would be
    // nondeterministic.
    bool operator<(const ProjInstPair &rhs) const { return proj < rhs.proj; }
  };

  // Add load projections to a projection list.
  llvm::SmallVector<ProjInstPair, 8> projections;
  llvm::SmallVector<BeginBorrowInst *, 8> borrows;
  llvm::SmallVector<SILInstruction *, 8> lifetimeEndingInsts;
  for (auto *use : getNonDebugUses(*loadInst)) {
    auto *user = use->getUser();
    if (needsBorrow) {
      if (auto *destroy = dyn_cast<DestroyValueInst>(user)) {
        lifetimeEndingInsts.push_back(destroy);
        continue;
      }
      auto *borrow = dyn_cast<BeginBorrowInst>(user);
      if (!borrow)
        return nextII;

      // The transformation below also assumes a single borrow use.
      auto *borrowedOper = borrow->getSingleNonEndingUse();
      if (!borrowedOper)
        return nextII;

      borrows.push_back(borrow);
      user = borrowedOper->getUser();
    } else {
      if (isa<EndBorrowInst>(user) &&
          !loadInst.getOwnershipQualifier().hasValue()) {
        lifetimeEndingInsts.push_back(user);
        continue;
      }
    }

    // If we have any non SEI, TEI instruction, don't do anything here.
    if (!isa<StructExtractInst>(user) && !isa<TupleExtractInst>(user))
      return nextII;

    auto extract = cast<SingleValueInstruction>(user);
    projections.push_back({Projection(extract), extract});
  }
  // Sort the list so projections with the same value decl and tuples with the
  // same indices will be processed together. This makes it easy to reuse the
  // load from the first such projection for all subsequent projections on the
  // same value decl or index.
  std::sort(projections.begin(), projections.end());

  // If the original load is dead, then do not delete it before
  // diagnostics. Doing so would suppress DefiniteInitialization in cases like:
  //
  // struct S {
  //   let a: Int
  //   init() {
  //     _ = a // must be diagnosed as use before initialization
  //     a = 0
  //   }
  // }
  //
  // However, if the load has any projections, it must be deleted, otherwise
  // exclusivity checking is too strict:
  //
  // extension S {
  //   mutating func foo() {
  //     _ = a // Must be diagnosed as a read of self.a only not the whole self.
  //   }
  // }
  //
  // TODO: This logic subtly anticipates SILGen behavior. In the future, change
  // SILGen to avoid emitting the full load and never delete loads in Raw SIL.
  if (projections.empty() && loadInst->getModule().getStage() == SILStage::Raw)
    return nextII;

  // Create a new address projection instruction and load instruction for each
  // unique projection.
  Projection *lastProj = nullptr;
  Optional<LoadOperation> lastNewLoad;
  for (auto &pair : projections) {
    auto &proj = pair.proj;
    auto *extract = pair.extract;

    // If this projection is the same as the last projection we processed, just
    // replace all uses of the projection with the load we created previously.
    if (lastProj && proj == *lastProj) {
      replaceUsesOfExtract(extract, *lastNewLoad, pass);
      nextII = killInstruction(extract, nextII, pass);
      continue;
    }

    // This is a unique projection. Create the new address projection and load.
    lastProj = &proj;
    // Insert new instructions before the original load.
    SILBuilderWithScope LoadBuilder(*loadInst);
    auto *projInst =
        proj.createAddressProjection(LoadBuilder, loadInst->getLoc(),
                                     loadInst->getOperand(0))
            .get();
    pass.notifyNewInstruction(projInst);

    // When loading a trivial subelement, convert ownership.
    Optional<LoadOwnershipQualifier> loadOwnership =
        loadInst.getOwnershipQualifier();
    if (loadOwnership.hasValue()) {
      if (*loadOwnership != LoadOwnershipQualifier::Unqualified &&
          projInst->getType().isTrivial(*projInst->getFunction()))
        loadOwnership = LoadOwnershipQualifier::Trivial;
    } else {
      if (projInst->getType().isTrivial(*projInst->getFunction()))
        loadOwnership = LoadOwnershipQualifier::Trivial;
    }

    if (loadOwnership) {
      lastNewLoad =
          LoadBuilder.createLoad(loadInst->getLoc(), projInst, *loadOwnership);
    } else {
      lastNewLoad = LoadBuilder.createLoadBorrow(loadInst->getLoc(), projInst);
    }
    pass.notifyNewInstruction(**lastNewLoad);

    if (loadOwnership) {
      if (*loadOwnership == LoadOwnershipQualifier::Copy) {
        // Destroy the loaded value wherever the aggregate load was destroyed.
        assert(loadInst.getOwnershipQualifier() ==
               LoadOwnershipQualifier::Copy);
        for (SILInstruction *destroy : lifetimeEndingInsts) {
          auto *newInst = SILBuilderWithScope(destroy).createDestroyValue(
              destroy->getLoc(), **lastNewLoad);
          pass.notifyNewInstruction(newInst);
        }
      }
    } else {
      for (SILInstruction *destroy : lifetimeEndingInsts) {
        auto *newInst = SILBuilderWithScope(destroy).createEndBorrow(
            destroy->getLoc(), **lastNewLoad);
        pass.notifyNewInstruction(newInst);
      }
    }
    replaceUsesOfExtract(extract, *lastNewLoad, pass);
    nextII = killInstruction(extract, nextII, pass);
  }

  // Remove the now unused borrows.
  for (auto *borrow : borrows)
    nextII = killInstAndIncidentalUses(borrow, nextII, pass);

  // Erase the old load.
  for (auto *destroy : lifetimeEndingInsts)
    nextII = killInstruction(destroy, nextII, pass);

  return killInstAndIncidentalUses(*loadInst, nextII, pass);
}

// Given a store within a single property struct, recursively form the parent
// struct values and promote the store to the outer struct type.
//
// (store (struct_element_addr %base) object)
//   ->
// (store %base (struct object))
//
// TODO: supporting enums here would be very easy. The main thing is adding
// support in `createAggFromFirstLevelProjections`.
// Note: we will not be able to support tuples because we cannot have a
// single-element tuple.
static SILBasicBlock::iterator
broadenSingleElementStores(StoreInst *storeInst,
                           CanonicalizeInstruction &pass) {
  // Keep track of the next iterator after any newly added or to-be-deleted
  // instructions. This must be valid regardless of whether the pass immediately
  // deletes the instructions or simply records them for later deletion.
  auto nextII = std::next(storeInst->getIterator());
  auto *f = storeInst->getFunction();

  ProjectionPath projections(storeInst->getDest()->getType());
  SILValue op = storeInst->getDest();
  while (isa<StructElementAddrInst>(op)) {
    auto *inst = cast<SingleValueInstruction>(op);
    SILValue baseAddr = inst->getOperand(0);
    SILType baseAddrType = baseAddr->getType();
    auto *decl = baseAddrType.getStructOrBoundGenericStruct();
    assert(
      !decl->isResilient(f->getModule().getSwiftModule(),
                         f->getResilienceExpansion()) &&
        "This code assumes resilient structs can not have fragile fields. If "
        "this assert is hit, this has been changed. Please update this code.");

    // Bail if the store's destination is not a struct_element_addr or if the
    // store's destination (%base) is not a loadable type. If %base is not a
    // loadable type, we can't create it as a struct later on.
    // If our aggregate has unreferenced storage then we can never prove if it
    // actually has a single field.
    if (!baseAddrType.isLoadable(*f) ||
        baseAddrType.aggregateHasUnreferenceableStorage() ||
        decl->getStoredProperties().size() != 1)
      break;

    projections.push_back(Projection(inst));
    op = baseAddr;
  }

  // If we couldn't create a projection, bail.
  if (projections.empty())
    return nextII;

  // Now work our way back up. At this point we know all operations we are going
  // to do succeed (cast<SingleValueInst>, createAggFromFirstLevelProjections,
  // etc.) so we can omit null checks. We should not bail at this point (we
  // could create a double consume, or worse).
  SILBuilderWithScope builder(storeInst);
  SILValue result = storeInst->getSrc();
  SILValue storeAddr = storeInst->getDest();
  for (Projection proj : llvm::reverse(projections)) {
    storeAddr = cast<SingleValueInstruction>(storeAddr)->getOperand(0);
    result = proj.createAggFromFirstLevelProjections(
                     builder, storeInst->getLoc(),
                     storeAddr->getType().getObjectType(), {result})
                 .get();
  }

  // Store the new struct-wrapped value into the final base address.
  builder.createStore(storeInst->getLoc(), result, storeAddr,
                      storeInst->getOwnershipQualifier());

  // Erase the original store.
  return killInstruction(storeInst, nextII, pass);
}

//===----------------------------------------------------------------------===//
//                            Simple ARC Peepholes
//===----------------------------------------------------------------------===//

/// "dead" copies are removed in OSSA, but this may shorten object lifetimes,
/// changing program semantics in unexpected ways by releasing weak references
/// and running deinitializers early. This copy may be the only thing keeping a
/// variable's reference alive. But just because the copy's current SSA value
/// contains no other uses does not mean that there aren't other uses that still
/// correspond to the original variable whose lifetime is protected by this
/// copy. The only way to guarantee the lifetime of a variable is to use a
/// borrow scope--copy/destroy is insufficient by itself.
///
/// FIXME: Technically this should be guarded by a compiler flag like
/// -enable-copy-propagation until SILGen protects scoped variables by
/// borrow scopes.
static SILBasicBlock::iterator
eliminateSimpleCopies(CopyValueInst *cvi, CanonicalizeInstruction &pass) {
  auto next = std::next(cvi->getIterator());

  // Eliminate copies that only have destroy_value uses.
  SmallVector<DestroyValueInst *, 8> destroys;
  for (auto *use : getNonDebugUses(cvi)) {
    if (auto *dvi = dyn_cast<DestroyValueInst>(use->getUser())) {
      destroys.push_back(dvi);
      continue;
    }
    return next;
  }

  while (!destroys.empty()) {
    next = killInstruction(destroys.pop_back_val(), next, pass);
  }

  next = killInstAndIncidentalUses(cvi, next, pass);
  return next;
}

/// Unlike dead copy elimination, dead borrows can be safely removed because the
/// semantics of a borrow scope
static SILBasicBlock::iterator
eliminateSimpleBorrows(BeginBorrowInst *bbi, CanonicalizeInstruction &pass) {
  auto next = std::next(bbi->getIterator());

  // Lexical borrow scopes can only be eliminated under certain circumstances:
  // (1) They can never be eliminated if the module is in the raw stage, because
  //     they may be needed for diagnostic.
  // (2) They can never be eliminated if there is no enclosing lexical scope
  //     which guarantees the lifetime of the value.
  if (bbi->isLexical() && (bbi->getModule().getStage() == SILStage::Raw ||
                           !isNestedLexicalBeginBorrow(bbi)))
    return next;

  // We know that our borrow is completely within the lifetime of its base value
  // if the borrow is never reborrowed. We check for reborrows and do not
  // optimize such cases. Otherwise, we can eliminate our borrow and instead use
  // our operand.
  auto base = bbi->getOperand();
  auto baseOwnership = base.getOwnershipKind();
  SmallVector<EndBorrowInst *, 8> endBorrows;
  for (auto *use : getNonDebugUses(bbi)) {
    if (auto *ebi = dyn_cast<EndBorrowInst>(use->getUser())) {
      endBorrows.push_back(ebi);
      continue;
    }

    // Otherwise, if we have a use that is non-lifetime ending and can accept
    // our base ownership, continue.
    if (!use->isLifetimeEnding() && use->canAcceptKind(baseOwnership))
      continue;

    return next;
  }

  while (!endBorrows.empty()) {
    next = killInstruction(endBorrows.pop_back_val(), next, pass);
  }
  bbi->replaceAllUsesWith(base);
  pass.notifyHasNewUsers(base);
  return killInstruction(bbi, next, pass);
}

/// Delete any result having forwarding instruction that only has destroy_value
/// and debug_value uses.
static SILBasicBlock::iterator
eliminateUnneededForwardingUnarySingleValueInst(SingleValueInstruction *inst,
                                                CanonicalizeInstruction &pass) {
  auto next = std::next(inst->getIterator());

  for (auto *use : getNonDebugUses(inst))
    if (!isa<DestroyValueInst>(use->getUser()))
      return next;
  deleteAllDebugUses(inst, pass.callbacks);
  SILValue op = inst->getOperand(0);
  inst->replaceAllUsesWith(op);
  pass.notifyHasNewUsers(op);
  return killInstruction(inst, next, pass);
}

static Optional<SILBasicBlock::iterator>
tryEliminateUnneededForwardingInst(SILInstruction *i,
                                   CanonicalizeInstruction &pass) {
  assert(OwnershipForwardingMixin::isa(i) &&
         "Must be an ownership forwarding inst");
  if (auto *svi = dyn_cast<SingleValueInstruction>(i))
    if (svi->getNumOperands() == 1)
      return eliminateUnneededForwardingUnarySingleValueInst(svi, pass);

  return None;
}

//===----------------------------------------------------------------------===//
//                   DestructureTuple <-> Tuple Round Trip
//===----------------------------------------------------------------------===//

/// (%x0, ..., %xn) = destructure_tuple %x
/// %y = tuple(%x0, ..., %xn)
/// ->
/// %y = %x
static SILBasicBlock::iterator
eliminateRoundTripTupleOfDestructure(TupleInst *tupleInst,
                                     CanonicalizeInstruction &pass) {
  auto nextII = std::next(tupleInst->getIterator());

  auto tupleElts = tupleInst->getAllOperands();
  if (tupleElts.empty())
    return nextII;

  SILValue firstElt = tupleElts[0].get();

  // We use dyn_cast_or_null in case our tuple's element is from an argument.
  auto *dti = dyn_cast_or_null<DestructureTupleInst>(
      firstElt->getDefiningInstruction());
  if (!dti) {
    return nextII;
  }

  // See if the destructure_tuple is from a full apply site. For now we do not
  // support optimizing such destructures. Instead we are going to wait for
  // applies to be given multiple results.
  if (isa<ApplyInst>(dti->getOperand()))
    return nextII;

  // Make sure the types line up.
  if (tupleInst->getType() != dti->getOperand()->getType())
    return nextII;

  // Then make sure that every result of the dti is one of our tuple operands
  // and they line up in order. Then if our destructure results only have a
  // single non-debug use (which must be our tuple), then we can eliminate both
  // instructions.
  for (auto p : llvm::enumerate(dti->getResults())) {
    if (tupleElts[p.index()].get() != p.value())
      return nextII;
    if (!p.value()->hasOneUse())
      return nextII;
  }

  // Then make everything that used the tuple, use the destructure's opeand and
  // then delete the tuple/destructure in that order.
  pass.callbacks.replaceValueUsesWith(tupleInst, dti->getOperand());
  nextII = killInstruction(tupleInst, nextII, pass);
  nextII = killInstruction(dti, nextII, pass);
  return nextII;
}

//===----------------------------------------------------------------------===//
//                        Destructure Simplifications
//===----------------------------------------------------------------------===//

// %x = tuple(%x0, ..., %xn)
// (%y0, ..., %yn) = destructure_tuple %x
//
// ->
// (%y0, ..., %yn) = (%x0, ..., %xn)
static SILBasicBlock::iterator eliminateRoundTripDestructureOfTuple(
    TupleInst *tupleInst, DestructureTupleInst *dti,
    SILBasicBlock::iterator nextII, CanonicalizeInstruction &pass) {
  // Make sure the types line up.
  if (dti->getOperand()->getType() != tupleInst->getType())
    return nextII;

  // Make sure our tuple only has a single use, us.
  if (!tupleInst->hasOneUse())
    return nextII;

  // Ok, we can eliminate this! Replace all uses of our dti's results with uses
  // of the tupleInst's operands and then destroy the dti and the tuple in that
  // order.
  for (auto p : llvm::enumerate(dti->getAllResults())) {
    p.value()->replaceAllUsesWith(tupleInst->getOperand(p.index()));
  }
  tupleInst->replaceAllUsesOfAllResultsWithUndef();
  dti->replaceAllUsesOfAllResultsWithUndef();
  nextII = killInstruction(dti, nextII, pass);
  nextII = killInstruction(tupleInst, nextII, pass);
  return nextII;
}

// We assume that we are going to see one of two patterns:
//
//   %alloc = alloc_stack $(Int, Int)
//   ...
//   %t0 = tuple_element_addr %alloc, 0
//   %t1 = tuple_element_addr %alloc, 1
//   (%0, %1) = destructure_tuple %x
//   ...
//   store %0 to %t0
//   store %1 to %t1
//
// Or:
//
//   %alloc = alloc_stack $(Int, Int)
//   ...
//   (%0, %1) = destructure_tuple %x
//   ...
//   %t0 = tuple_element_addr %alloc, 0
//   store %0 to %t0
//   %t1 = tuple_element_addr %alloc, 1
//   store %1 to %t1
//
// To:
//
//   %alloc = alloc_stack $(Int, Int)
//   ...
//   store %x to %alloc
//
// We assume everything lines up and there aren't any intervening instructions
// since this is just looking for code out of SILGen.
//
// We iterate over the code sequence twice, once to check for properties and
// then to optimize instead of using a seenInsts or the like.
static SILBasicBlock::iterator
canonicalizeAwayTupleEltAddrDestructurePairs(DestructureTupleInst *dti,
                                             SILBasicBlock::iterator next,
                                             CanonicalizeInstruction &pass) {
  // This is a very specific pattern that we only expect to be produced by
  // SILGen. In order to save a little compile time in later phases of the
  // compiler, just bail early if we are not in Raw SIL.
  if (dti->getModule().getStage() != SILStage::Raw)
    return next;

  // See if the destructure_tuple is from a full apply site. For now we do not
  // support optimizing such destructures. Instead we are going to wait for
  // applies to be given multiple results.
  if (isa<ApplyInst>(dti->getOperand()))
    return next;

  auto results = dti->getResults();
  SILValue firstResult = results[0];
  auto *firstResultUse = firstResult->getSingleUse();
  if (!firstResultUse)
    return next;

  SILInstruction *firstInst = nullptr;
  TupleElementAddrInst *firstTEAI = nullptr;
  Optional<StoreOwnershipQualifier> storeOwnershipQualifier;
  Optional<AssignOwnershipQualifier> assignOwnershipQualifier;
  if (auto *si = dyn_cast<StoreInst>(firstResultUse->getUser())) {
    firstInst = si;
    storeOwnershipQualifier = si->getOwnershipQualifier();
    firstTEAI = dyn_cast<TupleElementAddrInst>(si->getDest());
  }
  if (auto *ai = dyn_cast<AssignInst>(firstResultUse->getUser())) {
    firstInst = ai;
    assignOwnershipQualifier = ai->getOwnershipQualifier();
    firstTEAI = dyn_cast<TupleElementAddrInst>(ai->getDest());
  }
  if (!firstInst || !firstTEAI || firstTEAI->getFieldIndex() != 0)
    return next;

  // Make sure that our memory location and the operand of our tuple have
  // exactly the same type.
  //
  // NOTE: One may think that a tuple is a tuple, but this is not true in the
  // face of TypeLowering preserving named tuple elements forcing us to perform
  // this check.
  if (firstTEAI->getOperand()->getType() !=
      dti->getOperand()->getType().getAddressType())
    return next;

  // Ok, we might have found the start of a tuple sequence. Do a forward walk
  // proving this.
  unsigned numTupleEltStoreSeen = 1;
  unsigned maxNumElements = firstTEAI->getTupleType()->getNumElements();
  SILBasicBlock::iterator lastStore = firstInst->getIterator();
  for (auto ii = std::next(firstInst->getIterator(), 1),
            ie = firstInst->getParent()->end();
       ii != ie && numTupleEltStoreSeen < maxNumElements; ++ii) {
    if (auto *teai = dyn_cast<TupleElementAddrInst>(&*ii)) {
      // We go down this path if we are in an interleaved tuple_element_addr,
      // store pattern.
      if (teai->getFieldIndex() != numTupleEltStoreSeen ||
          teai->getOperand() != firstTEAI->getOperand()) {
        return next;
      }
      auto *teaiUse = teai->getSingleUse();
      if (!teaiUse) {
        return next;
      }
      auto *teaiUser = teaiUse->getUser();
      if (std::next(teai->getIterator(), 1) != teaiUser->getIterator() ||
          teaiUser->getKind() != firstInst->getKind() ||
          &teaiUser->getAllOperands()[CopyLikeInstruction::Dest] != teaiUse) {
        return next;
      }
      // This is an ok tea. Continue.
      continue;
    }

    // Equivalent to checking that ii is a store or an assign but additionally
    // makes sure it is the same kind of instruction as the first instruction.
    if (ii->getKind() != firstInst->getKind()) {
      return next;
    }

    // We are now going to check for a store/assign. We make sure that the
    // tuple_element_addr that is the operand satisfies our requirements. This
    // tuple_element_addr could be earlier or interleaved with our stores. In
    // practice SILGen will always emit one pattern or the other, but for
    // simplicity we handle a more general pattern with restrictions to make
    // sure it is safe.
    //
    // NOTE: The more general pattern is that we technically could allow for
    // some early tuple_element_addr and some interleaved tuple_element_addr and
    // the optimization wouldn't care.
    auto *teai = dyn_cast<TupleElementAddrInst>(
        ii->getOperand(CopyLikeInstruction::Dest));

    // First make sure that our operand is a tuple_element_addr and it is on the
    // next tuple element that we need to initialize.
    //
    // NOTE: We are purposely not checking if our tuple_element_addr here has a
    // single use. The reason why is that we only care about if our
    // tuple_element_addr has a single use in the interleaved case since we are
    // going to delete all instructions in between firstInst to the
    // lastStoreInst (assuming we decide to optimize). That condition is checked
    // earlier in the loop when we visit said interleaved tuple_element_addr. If
    // the tuple_element_addr is not in our instruction range, we don't care
    // about this and can optimize safely.
    if (!teai || teai->getFieldIndex() != numTupleEltStoreSeen ||
        teai->getOperand() != firstTEAI->getOperand()) {
      return next;
    }

    // Make sure we have a store or an assign.
    if (auto *si = dyn_cast<StoreInst>(ii)) {
      // If we have a store, merge in Init if we have it... if we have an
      // init/assign though... bail. We don't handle that case!
      if (*storeOwnershipQualifier != si->getOwnershipQualifier()) {
        if (*storeOwnershipQualifier == StoreOwnershipQualifier::Trivial) {
          if (si->getOwnershipQualifier() != StoreOwnershipQualifier::Trivial) {
            storeOwnershipQualifier = si->getOwnershipQualifier();
          }
        } else {
          // We have two non-trivial... we can't merge! so bail!
          if (si->getOwnershipQualifier() != StoreOwnershipQualifier::Trivial) {
            return next;
          }
          // Otherwise, we are fine.
        }
      }

      // Otherwise, we are ok, we can eliminate this store. Stash the last store
      // so we can just delete all instructions in between the first/last
      // store. We increment the numTupleElementStoreSeen variable to ensure
      // that we can validate that the next store updates the next tuple elt.
      lastStore = si->getIterator();
      ++numTupleEltStoreSeen;
      continue;
    }

    if (auto *ai = dyn_cast<AssignInst>(ii)) {
      // We do not optimize cases where the assign insts have differing
      // qualifiers.
      if (!ai || ai->getOwnershipQualifier() != *assignOwnershipQualifier) {
        return next;
      }

      // Otherwise, we are ok, we can eliminate this store. Stash the last store
      // so we can just delete all instructions in between the first/last
      // store. We increment the numTupleElementStoreSeen variable to ensure
      // that we can validate that the next store updates the next tuple elt.
      lastStore = ai->getIterator();
      ++numTupleEltStoreSeen;
      continue;
    }

    // At this point, we have seen some instruction that we do not
    // understand. Be conservative and bail.
    return next;
  }

  // Then set next to be the instruction after our last store. We know that all
  // of our tuple_element_addr only had single uses, our stores, so we do not
  // need to worry about any uses of the tuple_element_addr after the last
  // store, so we can just walk from our firstInst to the last store deleting
  // instructions when we are done creating the new store/assign.
  next = std::next(lastStore->getIterator());

  // Then create a new store at next that stores the destructures operand
  // directly in the teaiOperand.
  {
    SILBuilderWithScope builder(next);
    if (auto q = storeOwnershipQualifier) {
      // We have a store qualifier, so we have a store.
      builder.emitStoreValueOperation(next->getLoc(), dti->getOperand(),
                                      firstTEAI->getOperand(), *q);
    } else {
      // Otherwise, we had an assign.
      builder.createAssign(next->getLoc(), dti->getOperand(),
                           firstTEAI->getOperand(), *assignOwnershipQualifier);
    }
  }

  // We on purpose recompute std::next of lastStore so we do not destroy our
  // newly inserted instructions.
  //
  // NOTE: Since we checked that all interleaved tuple_element_addr had a single
  // use, we know that we can just delete them without worry and thus can just
  // delete all instructions from firstInst -> lastStore.
  for (auto ii = firstInst->getIterator(),
            ie = std::next(lastStore->getIterator());
       ii != ie;) {
    auto *inst = &*ii;
    ++ii;
    inst->replaceAllUsesOfAllResultsWithUndef();
    pass.killInstruction(&*inst);
  }

  // Then finally eliminate the destructure of the value since we are now just
  // storing the value directly.
  pass.killInstruction(dti);

  return next;
}

/// SILGen uses a construct called RValue that causes a lot of traffic
/// back/forth with destructure_tuple/tuple. Here we try to eliminate certain
/// cases of this.
static SILBasicBlock::iterator
eliminateUnnecessaryDestructuresFromRValue(DestructureTupleInst *dti,
                                           CanonicalizeInstruction &pass) {
  auto next = std::next(dti->getIterator());
  if (auto *tupleInst = dyn_cast<TupleInst>(dti->getOperand()))
    return eliminateRoundTripDestructureOfTuple(tupleInst, dti, next, pass);
  return canonicalizeAwayTupleEltAddrDestructurePairs(dti, next, pass);
}

//===----------------------------------------------------------------------===//
//                            Top-Level Entry Point
//===----------------------------------------------------------------------===//

SILBasicBlock::iterator
CanonicalizeInstruction::canonicalize(SILInstruction *inst) {
  if (auto nextII = simplifyAndReplace(inst, *this))
    return nextII.getValue();

  if (auto *tuple = dyn_cast<TupleInst>(inst)) {
    return eliminateRoundTripTupleOfDestructure(tuple, *this);
  }

  if (auto *dti = dyn_cast<DestructureTupleInst>(inst)) {
    return eliminateUnnecessaryDestructuresFromRValue(dti, *this);
  }

  if (auto li = LoadOperation(inst)) {
    return splitAggregateLoad(li, *this);
  }

  if (auto *storeInst = dyn_cast<StoreInst>(inst)) {
    return broadenSingleElementStores(storeInst, *this);
  }

  if (auto *cvi = dyn_cast<CopyValueInst>(inst))
    return eliminateSimpleCopies(cvi, *this);

  if (auto *bbi = dyn_cast<BeginBorrowInst>(inst))
    return eliminateSimpleBorrows(bbi, *this);

  // If we have ownership and are not in raw SIL, eliminate unneeded forwarding
  // insts. We don't do this in raw SIL as not to disturb the codegen read by
  // diagnostics.
  auto *fn = inst->getFunction();
  if (fn->hasOwnership() && fn->getModule().getStage() != SILStage::Raw) {
    if (OwnershipForwardingMixin::isa(inst))
      if (auto newNext = tryEliminateUnneededForwardingInst(inst, *this))
        return *newNext;
  }

  // Skip ahead.
  return std::next(inst->getIterator());
}
