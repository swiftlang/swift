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
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "swift/SILOptimizer/Utils/DebugOptUtils.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// STATISTIC uses the default DEBUG_TYPE.
#define DEBUG_TYPE CanonicalizeInstruction::defaultDebugType
STATISTIC(NumSimplified, "Number of instructions simplified");

// Tracing within the implementation can also be activiated by the pass.
#undef DEBUG_TYPE
#define DEBUG_TYPE pass.debugType

// Helper to delete an instruction, or mark it for deletion.
//
// \p inst may not have any remaining users except for incidental uses,
// including debug_value, end_borrow. It may also have destroy_value, which is
// effectively an incidental use even though it is not yet explicitly recognized
// as such.
//
// FIXME: fix_lifetime uses are not currently handled here. They are generally
// (incorrectly) treated as "incidental" uses, but no canonicalizations need
// them yet.
static void killInstruction(SILInstruction *inst,
                            CanonicalizeInstruction &pass) {
  pass.deleter.forceDelete(inst);
}

//===----------------------------------------------------------------------===//
//                          Instruction Simplification
//===----------------------------------------------------------------------===//

// Erases the simplified instruction and any instructions that end its
// scope. Nothing needs to be added to the worklist except for Result,
// because the instruction and all non-replaced users will be deleted.
//
// Return true if simplification is successful. \p inst may be deleted.
static bool simplifyAndReplace(SILInstruction *inst,
                               CanonicalizeInstruction &pass) {
#ifndef NDEBUG
  std::string instDesc;
  LLVM_DEBUG(llvm::raw_string_ostream ss(instDesc); ss << *inst);
#endif
  if (simplifyAndReplaceAllSimplifiedUsesAndErase(inst, pass.deleter,
                                                  &pass.deadEndBlocks)) {
    LLVM_DEBUG(llvm::dbgs() << "simplified " << instDesc);
    ++NumSimplified;
    return true;
  }
  return false;
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
static bool splitAggregateLoad(LoadOperation loadInst,
                               CanonicalizeInstruction &pass) {
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
      return false;
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
        return false;

      // The transformation below also assumes a single borrow use.
      auto *borrowedOper = borrow->getSingleNonEndingUse();
      if (!borrowedOper)
        return false;

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
      return false;

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
    return false;

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
      killInstruction(extract, pass);
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
    killInstruction(extract, pass);
  }

  // Remove the now unused borrows.
  for (auto *borrow : borrows)
    killInstruction(borrow, pass);

  // Erase the old load.
  for (auto *destroy : lifetimeEndingInsts)
    killInstruction(destroy, pass);

  killInstruction(*loadInst, pass);
  return true;
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
static bool broadenSingleElementStores(StoreInst *storeInst,
                                       CanonicalizeInstruction &pass) {
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
    return false;

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
  killInstruction(storeInst, pass);
  return true;
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
/// FIXME: This assumes that destroy_value cannot observe side effects.  It
/// should be guarded by a compiler flag like -enable-copy-propagation until
/// SILGen protects scoped variables by lexical borrow scopes.
static bool eliminateSimpleCopies(CopyValueInst *cvi,
                                  CanonicalizeInstruction &pass) {
  // Eliminate copies that only have destroy_value uses.
  SmallVector<DestroyValueInst *, 8> destroys;
  for (auto *use : getNonDebugUses(cvi)) {
    if (auto *dvi = dyn_cast<DestroyValueInst>(use->getUser())) {
      destroys.push_back(dvi);
      continue;
    }
    return false;
  }

  while (!destroys.empty()) {
    killInstruction(destroys.pop_back_val(), pass);
  }

  killInstruction(cvi, pass);
  return true;
}

/// Unlike dead copy elimination, dead borrows can be safely removed because the
/// semantics of a borrow scope
static bool eliminateSimpleBorrows(BeginBorrowInst *bbi,
                                   CanonicalizeInstruction &pass) {
  // Never eliminate lexical borrow scopes.  They must be kept to ensure that
  // value lifetimes aren't observably shortened.
  if (bbi->isLexical())
    return false;

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

    return false;
  }

  while (!endBorrows.empty()) {
    killInstruction(endBorrows.pop_back_val(), pass);
  }
  pass.getCallbacks().notifyWillReplaceUses(bbi, base);
  bbi->replaceAllUsesWith(base);
  killInstruction(bbi, pass);
  return true;
}

/// Delete any result having forwarding instruction that only has destroy_value
/// and debug_value uses.
static bool
eliminateUnneededForwardingUnarySingleValueInst(SingleValueInstruction *inst,
                                                CanonicalizeInstruction &pass) {
  for (auto *use : getNonDebugUses(inst))
    if (!isa<DestroyValueInst>(use->getUser()))
      return false;
  deleteAllDebugUses(inst, pass.getCallbacks());
  SILValue op = inst->getOperand(0);
  pass.getCallbacks().notifyWillReplaceUses(inst, op);
  inst->replaceAllUsesWith(op);
  killInstruction(inst, pass);
  return true;
}

static bool tryEliminateUnneededForwardingInst(SILInstruction *i,
                                               CanonicalizeInstruction &pass) {
  assert(OwnershipForwardingMixin::isa(i) &&
         "Must be an ownership forwarding inst");
  if (auto *svi = dyn_cast<SingleValueInstruction>(i)) {
    if (svi->getNumOperands() == 1) {
      return eliminateUnneededForwardingUnarySingleValueInst(svi, pass);
    }
  }
  return false;
}

//===----------------------------------------------------------------------===//
//                            Top-Level Entry Point
//===----------------------------------------------------------------------===//

bool CanonicalizeInstruction::canonicalize(SILInstruction *inst) {
  if (simplifyAndReplace(inst, *this))
    return true;

  if (auto li = LoadOperation(inst)) {
    return splitAggregateLoad(li, *this);
  }
  if (auto *storeInst = dyn_cast<StoreInst>(inst)) {
    return broadenSingleElementStores(storeInst, *this);
  }
  if (auto *cvi = dyn_cast<CopyValueInst>(inst)) {
    return eliminateSimpleCopies(cvi, *this);
  }
  if (auto *bbi = dyn_cast<BeginBorrowInst>(inst)) {
    return eliminateSimpleBorrows(bbi, *this);
  }
  // If we have ownership and are not in raw SIL, eliminate unneeded forwarding
  // insts. We don't do this in raw SIL as not to disturb the codegen read by
  // diagnostics.
  auto *fn = inst->getFunction();
  if (fn->hasOwnership() && fn->getModule().getStage() != SILStage::Raw) {
    if (OwnershipForwardingMixin::isa(inst))
      return tryEliminateUnneededForwardingInst(inst, *this);
  }
  return false;
}
