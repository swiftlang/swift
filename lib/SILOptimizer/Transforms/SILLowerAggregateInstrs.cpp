//===--- SILLowerAggregateInstrs.cpp - Aggregate insts to Scalar insts  ---===//
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
///
/// \file
///
/// Simplify aggregate instructions into scalar instructions using simple
/// peephole transformations.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-lower-aggregate-instrs"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::Lowering;

STATISTIC(NumExpand, "Number of instructions expanded");

//===----------------------------------------------------------------------===//
//                      Higher Level Operation Expansion
//===----------------------------------------------------------------------===//

/// Lower copy_addr into loads/stores/retain/release if we have a
/// non-address only type. We do this here so we can process the resulting
/// loads/stores.
///
/// This peephole implements the following optimizations:
///
/// copy_addr %0 to %1 : $*T
/// ->
///     %new = load %0 : $*T        // Load the new value from the source
///     %old = load %1 : $*T        // Load the old value from the destination
///     strong_retain %new : $T     // Retain the new value
///     strong_release %old : $T    // Release the old
///     store %new to %1 : $*T      // Store the new value to the destination
///
/// copy_addr [take] %0 to %1 : $*T
/// ->
///     %new = load %0 : $*T
///     %old = load %1 : $*T
///     // no retain of %new!
///     strong_release %old : $T
///     store %new to %1 : $*T
///
/// copy_addr %0 to [initialization] %1 : $*T
/// ->
///     %new = load %0 : $*T
///     strong_retain %new : $T
///     // no load/release of %old!
///     store %new to %1 : $*T
///
/// copy_addr [take] %0 to [initialization] %1 : $*T
/// ->
///     %new = load %0 : $*T
///     // no retain of %new!
///     // no load/release of %old!
///     store %new to %1 : $*T
static bool expandCopyAddr(CopyAddrInst *cai) {
  SILFunction *fn = cai->getFunction();
  SILModule &module = cai->getModule();
  SILValue source = cai->getSrc();

  // If we have an address only type don't do anything.
  SILType srcType = source->getType();
  if (srcType.isAddressOnly(*fn))
    return false;

  bool expand = shouldExpand(module, srcType.getObjectType());
  using TypeExpansionKind = Lowering::TypeLowering::TypeExpansionKind;
  auto expansionKind = expand ? TypeExpansionKind::MostDerivedDescendents
                              : TypeExpansionKind::None;

  SILBuilderWithScope builder(cai);

  // %new = load %0 : $*T
  LoadInst *newValue = builder.createLoad(cai->getLoc(), source,
                                          LoadOwnershipQualifier::Unqualified);

  SILValue destAddr = cai->getDest();

  // If our object type is not trivial, we may need to release the old value and
  // retain the new one.

  auto &typeLowering = fn->getTypeLowering(srcType);

  // If we have a non-trivial type...
  if (!typeLowering.isTrivial()) {
    // If we are not initializing:
    // %old = load %1 : $*T
    auto isInit = cai->isInitializationOfDest();
    LoadInst *oldValue = nullptr;
    if (IsInitialization_t::IsNotInitialization == isInit) {
      oldValue = builder.createLoad(cai->getLoc(), destAddr,
                                    LoadOwnershipQualifier::Unqualified);
    }

    // If we are not taking and have a reference type:
    //   strong_retain %new : $*T
    // or if we have a non-trivial non-reference type.
    //   retain_value %new : $*T
    if (IsTake_t::IsNotTake == cai->isTakeOfSrc()) {
      typeLowering.emitLoweredCopyValue(builder, cai->getLoc(), newValue,
                                        expansionKind);
    }

    // If we are not initializing:
    // strong_release %old : $*T
    //   *or*
    // release_value %old : $*T
    if (oldValue) {
      typeLowering.emitLoweredDestroyValue(builder, cai->getLoc(), oldValue,
                                           expansionKind);
    }
  }

  // Create the store.
  builder.createStore(cai->getLoc(), newValue, destAddr,
                      StoreOwnershipQualifier::Unqualified);

  ++NumExpand;
  return true;
}

static bool expandDestroyAddr(DestroyAddrInst *dai) {
  SILFunction *fn = dai->getFunction();
  SILModule &module = dai->getModule();
  SILBuilderWithScope builder(dai);

  // Strength reduce destroy_addr inst into release/store if
  // we have a non-address only type.
  SILValue addr = dai->getOperand();

  // If we have an address only type, do nothing.
  SILType type = addr->getType();
  if (type.isAddressOnly(*fn))
    return false;

  bool expand = shouldExpand(module, type.getObjectType());

  // If we have a non-trivial type...
  if (!type.isTrivial(*fn)) {
    // If we have a type with reference semantics, emit a load/strong release.
    LoadInst *li = builder.createLoad(dai->getLoc(), addr,
                                      LoadOwnershipQualifier::Unqualified);
    auto &typeLowering = fn->getTypeLowering(type);
    using TypeExpansionKind = Lowering::TypeLowering::TypeExpansionKind;
    auto expansionKind = expand ? TypeExpansionKind::MostDerivedDescendents
                                : TypeExpansionKind::None;
    typeLowering.emitLoweredDestroyValue(builder, dai->getLoc(), li,
                                         expansionKind);
  }

  ++NumExpand;
  return true;
}

static bool expandReleaseValue(ReleaseValueInst *rvi) {
  SILFunction *fn = rvi->getFunction();
  SILModule &module = rvi->getModule();
  SILBuilderWithScope builder(rvi);

  // Strength reduce destroy_addr inst into release/store if
  // we have a non-address only type.
  SILValue value = rvi->getOperand();

  // If we have an address only type, do nothing.
  SILType type = value->getType();
  assert(!SILModuleConventions(module).useLoweredAddresses() ||
         type.isLoadable(*fn) &&
             "release_value should never be called on a non-loadable type.");

  if (!shouldExpand(module, type.getObjectType()))
    return false;

  auto &TL = fn->getTypeLowering(type);
  TL.emitLoweredDestroyValueMostDerivedDescendents(builder, rvi->getLoc(),
                                                   value);

  LLVM_DEBUG(llvm::dbgs() << "    Expanding: " << *rvi);

  ++NumExpand;
  return true;
}

static bool expandRetainValue(RetainValueInst *rvi) {
  SILFunction *fn = rvi->getFunction();
  SILModule &module = rvi->getModule();
  SILBuilderWithScope builder(rvi);

  // Strength reduce destroy_addr inst into release/store if
  // we have a non-address only type.
  SILValue value = rvi->getOperand();

  // If we have an address only type, do nothing.
  SILType type = value->getType();
  assert(!SILModuleConventions(module).useLoweredAddresses() ||
         type.isLoadable(*fn) &&
             "Copy Value can only be called on loadable types.");

  if (!shouldExpand(module, type.getObjectType()))
    return false;

  auto &typeLowering = fn->getTypeLowering(type);
  typeLowering.emitLoweredCopyValueMostDerivedDescendents(builder,
                                                          rvi->getLoc(), value);

  LLVM_DEBUG(llvm::dbgs() << "    Expanding: " << *rvi);

  ++NumExpand;
  return true;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool processFunction(SILFunction &fn) {
  bool changed = false;
  for (auto &block : fn) {
    auto ii = block.begin(), ie = block.end();
    while (ii != ie) {
      SILInstruction *inst = &*ii;

      LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *inst);

      if (auto *cai = dyn_cast<CopyAddrInst>(inst))
        if (expandCopyAddr(cai)) {
          ++ii;
          cai->eraseFromParent();
          changed = true;
          continue;
        }

      if (auto *dai = dyn_cast<DestroyAddrInst>(inst))
        if (expandDestroyAddr(dai)) {
          ++ii;
          dai->eraseFromParent();
          changed = true;
          continue;
        }

      if (auto *rvi = dyn_cast<RetainValueInst>(inst))
        if (expandRetainValue(rvi)) {
          ++ii;
          rvi->eraseFromParent();
          changed = true;
          continue;
        }

      if (auto *rvi = dyn_cast<ReleaseValueInst>(inst))
        if (expandReleaseValue(rvi)) {
          ++ii;
          rvi->eraseFromParent();
          changed = true;
          continue;
        }

      ++ii;
    }
  }
  return changed;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class SILLowerAggregate : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    SILFunction *f = getFunction();
    // FIXME: Can we support ownership?
    if (f->hasOwnership())
      return;
    LLVM_DEBUG(llvm::dbgs() << "***** LowerAggregate on function: "
                            << f->getName() << " *****\n");
    bool changed = processFunction(*f);
    if (changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createLowerAggregateInstrs() {
  return new SILLowerAggregate();
}
