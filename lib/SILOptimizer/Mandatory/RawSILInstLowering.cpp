//===--- RawSILInstLowering.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "raw-sil-inst-lowering"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/Statistic.h"

STATISTIC(numAssignRewritten, "Number of assigns rewritten");

using namespace swift;

/// Emit the sequence that an assign instruction lowers to once we know
/// if it is an initialization or an assignment.  If it is an assignment,
/// a live-in value can be provided to optimize out the reload.
static void lowerAssignInstruction(SILBuilderWithScope &b, AssignInst *inst) {
  LLVM_DEBUG(llvm::dbgs() << "  *** Lowering [isInit="
                          << unsigned(inst->getOwnershipQualifier())
                          << "]: " << *inst << "\n");

  ++numAssignRewritten;

  SILValue src = inst->getSrc();
  SILValue dest = inst->getDest();
  SILLocation loc = inst->getLoc();
  AssignOwnershipQualifier qualifier = inst->getOwnershipQualifier();

  // Unknown qualifier is considered unprocessed. Just lower it as [reassign],
  // but if the destination type is trivial, treat it as [init].
  //
  // Unknown should not be lowered because definite initialization should
  // always set an initialization kind for assign instructions, but there exists
  // some situations where SILGen doesn't generate a mark_uninitialized
  // instruction for a full mark_uninitialized. Thus definite initialization
  // doesn't set an initialization kind for some assign instructions.
  //
  // TODO: Fix SILGen so that this is an assert preventing the lowering of
  //       Unknown init kind.
  if (qualifier == AssignOwnershipQualifier::Unknown)
    qualifier = AssignOwnershipQualifier::Reassign;

  if (qualifier == AssignOwnershipQualifier::Init ||
      inst->getDest()->getType().isTrivial(*inst->getFunction())) {

    // If this is an initialization, or the storage type is trivial, we
    // can just replace the assignment with a store.
    assert(qualifier != AssignOwnershipQualifier::Reinit);
    b.createTrivialStoreOr(loc, src, dest, StoreOwnershipQualifier::Init);
    inst->eraseFromParent();
    return;
  }

  if (qualifier == AssignOwnershipQualifier::Reinit) {
    // We have a case where a convenience initializer on a class
    // delegates to a factory initializer from a protocol extension.
    // Factory initializers give us a whole new instance, so the existing
    // instance, which has not been initialized and never will be, must be
    // freed using dealloc_partial_ref.
    SILValue pointer = b.createLoad(loc, dest, LoadOwnershipQualifier::Take);
    b.createStore(loc, src, dest, StoreOwnershipQualifier::Init);

    auto metatypeTy = CanMetatypeType::get(
        dest->getType().getASTType(), MetatypeRepresentation::Thick);
    auto silMetatypeTy = SILType::getPrimitiveObjectType(metatypeTy);
    SILValue metatype = b.createValueMetatype(loc, silMetatypeTy, pointer);

    b.createDeallocPartialRef(loc, pointer, metatype);
    inst->eraseFromParent();
    return;
  }

  assert(qualifier == AssignOwnershipQualifier::Reassign);
  // Otherwise, we need to replace the assignment with a store [assign] which
  // lowers to the load/store/release dance. Note that the new value is already
  // considered to be retained (by the semantics of the storage type),
  // and we're transferring that ownership count into the destination.

  b.createStore(loc, src, dest, StoreOwnershipQualifier::Assign);
  inst->eraseFromParent();
}

static void lowerAssignByDelegateInstruction(SILBuilderWithScope &b,
                                             AssignByDelegateInst *inst,
                            SmallVectorImpl<BeginAccessInst *> &accessMarkers) {
  LLVM_DEBUG(llvm::dbgs() << "  *** Lowering [isInit="
             << unsigned(inst->getOwnershipQualifier())
             << "]: " << *inst << "\n");

  ++numAssignRewritten;

  SILValue src = inst->getSrc();
  SILValue dest = inst->getDest();
  SILLocation loc = inst->getLoc();
  SILArgumentConvention srcConvention(SILArgumentConvention::Indirect_Inout);

  switch (inst->getOwnershipQualifier()) {
    case AssignOwnershipQualifier::Init: {
      SILValue initFn = inst->getInitializer();
      CanSILFunctionType fTy = initFn->getType().castTo<SILFunctionType>();
      SILFunctionConventions convention(fTy, inst->getModule());
      if (convention.hasIndirectSILResults()) {
        b.createApply(loc, initFn, { dest, src }, false);
        srcConvention = convention.getSILArgumentConvention(1);
      } else {
        SILValue wrappedSrc = b.createApply(loc, initFn, { src }, false);
        b.createTrivialStoreOr(loc, wrappedSrc, dest, StoreOwnershipQualifier::Init);
        srcConvention = convention.getSILArgumentConvention(0);
      }
      // TODO: remove the unused setter function, which usually is a dead
      // partial_apply.
      break;
    }
    case AssignOwnershipQualifier::Unknown:
    case AssignOwnershipQualifier::Reassign: {
      SILValue setterFn = inst->getSetter();
      CanSILFunctionType fTy = setterFn->getType().castTo<SILFunctionType>();
      SILFunctionConventions convention(fTy, inst->getModule());
      assert(!convention.hasIndirectSILResults());
      srcConvention = convention.getSILArgumentConvention(0);
      b.createApply(loc, setterFn, { src }, false);

      // The destination address is not used. Remove it if it is a dead access
      // marker. This is important, because also the setter function contains
      // access marker. In case those markers are dynamic it would cause a
      // nested access violation.
      if (auto *BA = dyn_cast<BeginAccessInst>(dest))
        accessMarkers.push_back(BA);
      // TODO: remove the unused init function, which usually is a dead
      // partial_apply.
      break;
    }
    case AssignOwnershipQualifier::Reinit:
      llvm_unreachable("wrong qualifier for assign_by_delegate");
  }
  switch (srcConvention) {
    case SILArgumentConvention::Indirect_In_Guaranteed:
      b.createDestroyAddr(loc, src);
      break;
    case SILArgumentConvention::Direct_Guaranteed:
      b.createDestroyValue(loc, src);
      break;
    case SILArgumentConvention::Direct_Unowned:
    case SILArgumentConvention::Indirect_In:
    case SILArgumentConvention::Indirect_In_Constant:
    case SILArgumentConvention::Direct_Owned:
      break;
    case SILArgumentConvention::Indirect_Inout:
    case SILArgumentConvention::Indirect_InoutAliasable:
    case SILArgumentConvention::Indirect_Out:
    case SILArgumentConvention::Direct_Deallocating:
      llvm_unreachable("wrong convention for setter/initializer src argument");
  }
  inst->eraseFromParent();
}

static void deleteDeadAccessMarker(BeginAccessInst *BA) {
  for (Operand *Op : BA->getUses()) {
    SILInstruction *User = Op->getUser();
    if (!isa<EndAccessInst>(User))
      return;
  }
  for (Operand *Op : BA->getUses()) {
    Op->getUser()->eraseFromParent();
  }
  BA->eraseFromParent();
}

/// lowerRawSILOperations - There are a variety of raw-sil instructions like
/// 'assign' that are only used by this pass.  Now that definite initialization
/// checking is done, remove them.
static bool lowerRawSILOperations(SILFunction &fn) {
  bool changed = false;

  for (auto &bb : fn) {
    SmallVector<BeginAccessInst *, 8> accessMarkers;

    auto i = bb.begin(), e = bb.end();
    while (i != e) {
      SILInstruction *inst = &*i;
      ++i;

      // Lower 'assign' depending on initialization kind defined by definite
      // initialization.
      //
      // * Unknown is considered unprocessed and is treated as [reassign] or
      //   [init] if the destination type is trivial.
      // * Init becomes a store [init] or a store [trivial] if the destination's
      //   type is trivial.
      // * Reinit becomes a load [take], store [init], and a
      //   dealloc_partial_ref.
      // * Reassign becomes a store [assign].
      if (auto *ai = dyn_cast<AssignInst>(inst)) {
        SILBuilderWithScope b(ai);
        lowerAssignInstruction(b, ai);
        // Assign lowering may split the block. If it did,
        // reset our iteration range to the block after the insertion.
        if (b.getInsertionBB() != &bb)
          i = e;
        changed = true;
        continue;
      }

      if (auto *ai = dyn_cast<AssignByDelegateInst>(inst)) {
        SILBuilderWithScope b(ai);
        lowerAssignByDelegateInstruction(b, ai, accessMarkers);
        changed = true;
        continue;
      }

      // mark_uninitialized just becomes a noop, resolving to its operand.
      if (auto *mui = dyn_cast<MarkUninitializedInst>(inst)) {
        mui->replaceAllUsesWith(mui->getOperand());
        mui->eraseFromParent();
        changed = true;
        continue;
      }

      // mark_function_escape just gets zapped.
      if (isa<MarkFunctionEscapeInst>(inst)) {
        inst->eraseFromParent();
        changed = true;
        continue;
      }
    }
    for (BeginAccessInst *BA : accessMarkers) {
      deleteDeadAccessMarker(BA);
    }
  }
  return changed;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class RawSILInstLowering : public SILFunctionTransform {
  void run() override {
    // Do not try to relower raw instructions in canonical SIL. There won't be
    // any there.
    if (getFunction()->wasDeserializedCanonical()) {
      return;
    }

    // Lower raw-sil only instructions used by this pass, like "assign".
    if (lowerRawSILOperations(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
  }
};

} // end anonymous namespace

SILTransform *swift::createRawSILInstLowering() {
  return new RawSILInstLowering();
}
