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
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// STATISTIC uses the default DEBUG_TYPE.
#define DEBUG_TYPE CanonicalizeInstruction::defaultDebugType
STATISTIC(NumSimplified, "Number of instructions simplified");

// Tracing within the implementation can also be activiated by the pass.
#undef DEBUG_TYPE
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
  // FIXME: temporarily bypass simplification untill all simplifications
  // preserve ownership SIL.
  if (inst->getFunction()->hasOwnership())
    return None;

  SILValue result = simplifyInstruction(inst);
  if (!result)
    return None;

  ++NumSimplified;

  LLVM_DEBUG(llvm::dbgs() << "Simplify Old = " << *inst
                          << "    New = " << *result << '\n');

  // Erase the simplified instruction and any instructions that end its
  // scope. Nothing needs to be added to the worklist except for Result,
  // because the instruction and all non-replaced users will be deleted.
  auto nextII = replaceAllSimplifiedUsesAndErase(
      inst, result,
      [&pass](SILInstruction *deleted) { pass.killInstruction(deleted); });

  // Push the new instruction and any users onto the worklist.
  pass.notifyHasNewUsers(result);
  return nextII;
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
                                 LoadInst *loadInst,
                                 CanonicalizeInstruction &pass) {
  assert(extract->getType() == loadInst->getType());

  SingleValueInstruction *loadedVal = loadInst;
  if (loadInst->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
    // Borrow the load-copied subelement, with precisely the same scope as
    // the aggregate borrow.
    assert(extract->getNumOperands() == 1);
    auto *origBorrow = cast<BeginBorrowInst>(extract->getOperand(0));
    auto *newBorrow = SILBuilderWithScope(origBorrow)
                          .createBeginBorrow(loadInst->getLoc(), loadInst);
    pass.notifyNewInstruction(newBorrow);

    assert(extract == origBorrow->getSingleNonEndingUse()->getUser());
    for (auto *origEnd : origBorrow->getEndBorrows()) {
      auto *endBorrow = SILBuilderWithScope(origEnd).createEndBorrow(
          origEnd->getLoc(), newBorrow);
      pass.notifyNewInstruction(endBorrow);
    }
    loadedVal = newBorrow;
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
splitAggregateLoad(LoadInst *loadInst, CanonicalizeInstruction &pass) {
  // Keep track of the next iterator after any newly added or to-be-deleted
  // instructions. This must be valid regardless of whether the pass immediately
  // deletes the instructions or simply records them for later deletion.
  auto nextII = std::next(loadInst->getIterator());

  bool needsBorrow;
  switch (loadInst->getOwnershipQualifier()) {
  case LoadOwnershipQualifier::Unqualified:
  case LoadOwnershipQualifier::Trivial:
    needsBorrow = false;
    break;
  case LoadOwnershipQualifier::Copy:
    needsBorrow = true;
    break;
  case LoadOwnershipQualifier::Take:
    // TODO: To handle a "take", we would need to generate additional destroys
    // for any fields that aren't already extracted. This would be out-of-place
    // for this transform, and I'm not sure if this a case that needs to be
    // handled in SILGenCleanup.
    return nextII;
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
  llvm::SmallVector<DestroyValueInst *, 8> destroys;
  for (auto *use : getNonDebugUses(loadInst)) {
    auto *user = use->getUser();
    if (needsBorrow) {
      if (auto *destroy = dyn_cast<DestroyValueInst>(user)) {
        destroys.push_back(destroy);
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
  LoadInst *lastNewLoad = nullptr;
  for (auto &pair : projections) {
    auto &proj = pair.proj;
    auto *extract = pair.extract;

    // If this projection is the same as the last projection we processed, just
    // replace all uses of the projection with the load we created previously.
    if (lastProj && proj == *lastProj) {
      replaceUsesOfExtract(extract, lastNewLoad, pass);
      nextII = killInstruction(extract, nextII, pass);
      continue;
    }

    // This is a unique projection. Create the new address projection and load.
    lastProj = &proj;
    // Insert new instructions before the original load.
    SILBuilderWithScope LoadBuilder(loadInst);
    auto *projInst =
        proj.createAddressProjection(LoadBuilder, loadInst->getLoc(),
                                     loadInst->getOperand())
            .get();
    pass.notifyNewInstruction(projInst);

    // When loading a trivial subelement, convert ownership.
    LoadOwnershipQualifier loadOwnership = loadInst->getOwnershipQualifier();
    if (loadOwnership != LoadOwnershipQualifier::Unqualified
        && projInst->getType().isTrivial(*projInst->getFunction())) {
      loadOwnership = LoadOwnershipQualifier::Trivial;
    }

    lastNewLoad =
        LoadBuilder.createLoad(loadInst->getLoc(), projInst, loadOwnership);
    pass.notifyNewInstruction(lastNewLoad);

    if (loadOwnership == LoadOwnershipQualifier::Copy) {
      // Destroy the loaded value wherever the aggregate load was destroyed.
      assert(loadInst->getOwnershipQualifier() == LoadOwnershipQualifier::Copy);
      for (DestroyValueInst *destroy : destroys) {
        SILBuilderWithScope(destroy).createDestroyValue(destroy->getLoc(),
                                                        lastNewLoad);
        pass.notifyNewInstruction(destroy);
      }
    }
    replaceUsesOfExtract(extract, lastNewLoad, pass);
    nextII = killInstruction(extract, nextII, pass);
  }
  // Remove the now unused borrows.
  for (auto *borrow : borrows)
    nextII = killInstAndIncidentalUses(borrow, nextII, pass);

  // Erase the old load.
  for (auto *destroy : destroys)
    nextII = killInstruction(destroy, nextII, pass);

  return killInstAndIncidentalUses(loadInst, nextII, pass);
}

// Given a store within a single property struct, recursively form the parent
// struct values and promote the store to the outer struct type.
//
// (store (struct_element_addr %base) object)
//   ->
// (store %base (struct object))
static SILBasicBlock::iterator
broadenSingleElementStores(StoreInst *storeInst,
                           CanonicalizeInstruction &pass) {
  // Keep track of the next iterator after any newly added or to-be-deleted
  // instructions. This must be valid regardless of whether the pass immediately
  // deletes the instructions or simply records them for later deletion.
  auto nextII = std::next(storeInst->getIterator());

  // Bail if the store's destination is not a struct_element_addr.
  auto *sea = dyn_cast<StructElementAddrInst>(storeInst->getDest());
  if (!sea)
    return nextII;

  auto *f = storeInst->getFunction();

  // Continue up the struct_element_addr chain, as long as each struct only has
  // a single property, creating StoreInsts along the way.
  SILBuilderWithScope builder(storeInst);

  SILValue result = storeInst->getSrc();
  SILValue baseAddr = sea->getOperand();
  SILValue storeAddr;
  while (true) {
    SILType baseAddrType = baseAddr->getType();

    // If our aggregate has unreferenced storage then we can never prove if it
    // actually has a single field.
    if (baseAddrType.aggregateHasUnreferenceableStorage())
      break;

    auto *decl = baseAddrType.getStructOrBoundGenericStruct();
    assert(
      !decl->isResilient(f->getModule().getSwiftModule(),
                         f->getResilienceExpansion()) &&
        "This code assumes resilient structs can not have fragile fields. If "
        "this assert is hit, this has been changed. Please update this code.");

    // NOTE: If this is ever changed to support enums, we must check for address
    // only types here. For structs we do not have to check since a single
    // element struct with a loadable element can never be address only. We
    // additionally do not have to worry about our input value being address
    // only since we are storing into it.
    if (decl->getStoredProperties().size() != 1)
      break;

    // Update the store location now that we know it is safe.
    storeAddr = baseAddr;

    // Otherwise, create the struct.
    result = builder.createStruct(storeInst->getLoc(),
                                  baseAddrType.getObjectType(), result);

    // See if we have another struct_element_addr we can strip off. If we don't
    // then this as much as we can promote.
    sea = dyn_cast<StructElementAddrInst>(sea->getOperand());
    if (!sea)
      break;
    baseAddr = sea->getOperand();
  }
  // If we failed to create any structs, bail.
  if (result == storeInst->getSrc())
    return nextII;

  // Store the new struct-wrapped value into the final base address.
  builder.createStore(storeInst->getLoc(), result, storeAddr,
                      storeInst->getOwnershipQualifier());

  // Erase the original store.
  return killInstruction(storeInst, nextII, pass);
}

//===----------------------------------------------------------------------===//
//                            Top-Level Entry Point
//===----------------------------------------------------------------------===//

SILBasicBlock::iterator
CanonicalizeInstruction::canonicalize(SILInstruction *inst) {
  if (auto nextII = simplifyAndReplace(inst, *this))
    return nextII.getValue();

  if (auto *loadInst = dyn_cast<LoadInst>(inst))
    return splitAggregateLoad(loadInst, *this);

  if (auto *storeInst = dyn_cast<StoreInst>(inst))
    return broadenSingleElementStores(storeInst, *this);

  // Skip ahead.
  return std::next(inst->getIterator());
}
