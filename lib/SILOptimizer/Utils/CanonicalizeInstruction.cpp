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
// uses.
static SILBasicBlock::iterator
killInstAndDebugUses(SingleValueInstruction *inst,
                     SILBasicBlock::iterator nextII,
                     CanonicalizeInstruction &pass) {
  for (Operand *du : getDebugUses(inst))
    nextII = killInstruction(du->getUser(), nextII, pass);

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

  switch (loadInst->getOwnershipQualifier()) {
  case LoadOwnershipQualifier::Unqualified:
  case LoadOwnershipQualifier::Trivial:
    break;
  case LoadOwnershipQualifier::Take:
  case LoadOwnershipQualifier::Copy:
    // Skip ahead if the ownership qualifier isn't handled.
    //
    // FIXME: To handle Copy/Take, the code below needs to properly peek through
    // and destroy borrow scopes.
    return nextII;
  }
  struct ProjInstPair {
    Projection proj;
    SingleValueInstruction *svi;

    // When sorting, just look at the projection and ignore the instruction.
    // Including the instruction address in the sort key would be
    // nondeterministic.
    bool operator<(const ProjInstPair &rhs) const { return proj < rhs.proj; }
  };

  // Add load projections to a projection list.
  llvm::SmallVector<ProjInstPair, 8> projections;
  for (auto *use : getNonDebugUses(loadInst)) {
    auto *user = use->getUser();

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

  // Create a new address projection instruction and load instruction for each
  // unique projection.
  Projection *lastProj = nullptr;
  LoadInst *lastNewLoad = nullptr;
  for (auto &pair : projections) {
    auto &proj = pair.proj;
    auto *svi = pair.svi;

    // If this projection is the same as the last projection we processed, just
    // replace all uses of the projection with the load we created previously.
    if (lastProj && proj == *lastProj) {
      LLVM_DEBUG(llvm::dbgs() << "Replacing " << *svi
                 << "    with " << *lastNewLoad<< "\n");
      svi->replaceAllUsesWith(lastNewLoad);
      nextII = killInstruction(svi, nextII, pass);
      continue;
    }

    // This is a unique projection. Create the new address projection and load.
    lastProj = &proj;
    // Insert new instructions before the original load.
    SILBuilderWithScope B(loadInst);
    auto *projInst = proj.createAddressProjection(B, loadInst->getLoc(),
                                                  loadInst->getOperand())
                         .get();
    pass.notifyNewInstruction(projInst);

    lastNewLoad = B.createLoad(loadInst->getLoc(), projInst,
                               loadInst->getOwnershipQualifier());
    pass.notifyNewInstruction(lastNewLoad);

    LLVM_DEBUG(llvm::dbgs() << "Replacing " << *svi
               << "    with " << *lastNewLoad << "\n");
    svi->replaceAllUsesWith(lastNewLoad);
    nextII = killInstruction(svi, nextII, pass);
  }
  // Avoid removing dead loads from raw SIL. That may weaken definite-init.
  if (loadInst->getModule().getStage() == SILStage::Raw)
    return nextII;

  // Erase the old load.
  return killInstAndIncidentalUses(loadInst, nextII, pass);
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

  // Skip ahead.
  return std::next(inst->getIterator());
}
