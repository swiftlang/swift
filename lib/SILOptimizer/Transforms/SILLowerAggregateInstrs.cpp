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
//
// Simplify aggregate instructions into scalar instructions.
//
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
#include "swift/SILOptimizer/Utils/Local.h"
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
static bool expandCopyAddr(CopyAddrInst *CA) {
  SILModule &M = CA->getModule();
  SILValue Source = CA->getSrc();

  // If we have an address only type don't do anything.
  SILType SrcType = Source->getType();
  if (SrcType.isAddressOnly(M))
    return false;

  bool expand = shouldExpand(M, SrcType.getObjectType());
  using TypeExpansionKind = Lowering::TypeLowering::TypeExpansionKind;
  auto expansionKind = expand ? TypeExpansionKind::MostDerivedDescendents
                              : TypeExpansionKind::None;

  SILBuilderWithScope Builder(CA);

  // %new = load %0 : $*T
  LoadInst *New = Builder.createLoad(CA->getLoc(), Source,
                                     LoadOwnershipQualifier::Unqualified);

  SILValue Destination = CA->getDest();

  // If our object type is not trivial, we may need to release the old value and
  // retain the new one.

  auto &TL = M.getTypeLowering(SrcType);

  // If we have a non-trivial type...
  if (!TL.isTrivial()) {

    // If we are not initializing:
    // %old = load %1 : $*T
    IsInitialization_t IsInit = CA->isInitializationOfDest();
    LoadInst *Old = nullptr;
    if (IsInitialization_t::IsNotInitialization == IsInit) {
      Old = Builder.createLoad(CA->getLoc(), Destination,
                               LoadOwnershipQualifier::Unqualified);
    }

    // If we are not taking and have a reference type:
    //   strong_retain %new : $*T
    // or if we have a non-trivial non-reference type.
    //   retain_value %new : $*T
    IsTake_t IsTake = CA->isTakeOfSrc();
    if (IsTake_t::IsNotTake == IsTake) {
      TL.emitLoweredCopyValue(Builder, CA->getLoc(), New, expansionKind);
    }

    // If we are not initializing:
    // strong_release %old : $*T
    //   *or*
    // release_value %old : $*T
    if (Old) {
      TL.emitLoweredDestroyValue(Builder, CA->getLoc(), Old, expansionKind);
    }
  }

  // Create the store.
  Builder.createStore(CA->getLoc(), New, Destination,
                      StoreOwnershipQualifier::Unqualified);

  ++NumExpand;
  return true;
}

static bool expandDestroyAddr(DestroyAddrInst *DA) {
  SILModule &Module = DA->getModule();
  SILBuilderWithScope Builder(DA);

  // Strength reduce destroy_addr inst into release/store if
  // we have a non-address only type.
  SILValue Addr = DA->getOperand();

  // If we have an address only type, do nothing.
  SILType Type = Addr->getType();
  if (Type.isAddressOnly(Module))
    return false;

  bool expand = shouldExpand(Module, Type.getObjectType());

  // If we have a non-trivial type...
  if (!Type.isTrivial(Module)) {
    // If we have a type with reference semantics, emit a load/strong release.
    LoadInst *LI = Builder.createLoad(DA->getLoc(), Addr,
                                      LoadOwnershipQualifier::Unqualified);
    auto &TL = Module.getTypeLowering(Type);
    using TypeExpansionKind = Lowering::TypeLowering::TypeExpansionKind;
    auto expansionKind = expand ? TypeExpansionKind::MostDerivedDescendents
                                : TypeExpansionKind::None;
    TL.emitLoweredDestroyValue(Builder, DA->getLoc(), LI, expansionKind);
  }

  ++NumExpand;
  return true;
}

static bool expandReleaseValue(ReleaseValueInst *DV) {
  SILModule &Module = DV->getModule();
  SILBuilderWithScope Builder(DV);

  // Strength reduce destroy_addr inst into release/store if
  // we have a non-address only type.
  SILValue Value = DV->getOperand();

  // If we have an address only type, do nothing.
  SILType Type = Value->getType();
  assert(!SILModuleConventions(Module).useLoweredAddresses()
         || Type.isLoadable(Module) &&
         "release_value should never be called on a non-loadable type.");

  if (!shouldExpand(Module, Type.getObjectType()))
    return false;

  auto &TL = Module.getTypeLowering(Type);
  TL.emitLoweredDestroyValueMostDerivedDescendents(Builder, DV->getLoc(),
                                                   Value);

  LLVM_DEBUG(llvm::dbgs() << "    Expanding Destroy Value: " << *DV);

  ++NumExpand;
  return true;
}

static bool expandRetainValue(RetainValueInst *CV) {
  SILModule &Module = CV->getModule();
  SILBuilderWithScope Builder(CV);

  // Strength reduce destroy_addr inst into release/store if
  // we have a non-address only type.
  SILValue Value = CV->getOperand();

  // If we have an address only type, do nothing.
  SILType Type = Value->getType();
  assert(!SILModuleConventions(Module).useLoweredAddresses()
         || Type.isLoadable(Module) &&
         "Copy Value can only be called on loadable types.");

  if (!shouldExpand(Module, Type.getObjectType()))
    return false;

  auto &TL = Module.getTypeLowering(Type);
  TL.emitLoweredCopyValueMostDerivedDescendents(Builder, CV->getLoc(), Value);

  LLVM_DEBUG(llvm::dbgs() << "    Expanding Copy Value: " << *CV);

  ++NumExpand;
  return true;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

static bool processFunction(SILFunction &Fn) {
  bool Changed = false;
  for (auto BI = Fn.begin(), BE = Fn.end(); BI != BE; ++BI) {
    auto II = BI->begin(), IE = BI->end();
    while (II != IE) {
      SILInstruction *Inst = &*II;

      LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *Inst);

      if (auto *CA = dyn_cast<CopyAddrInst>(Inst))
        if (expandCopyAddr(CA)) {
          ++II;
          CA->eraseFromParent();
          Changed = true;
          continue;
        }

      if (auto *DA = dyn_cast<DestroyAddrInst>(Inst))
        if (expandDestroyAddr(DA)) {
          ++II;
          DA->eraseFromParent();
          Changed = true;
          continue;
        }

      if (auto *CV = dyn_cast<RetainValueInst>(Inst))
        if (expandRetainValue(CV)) {
          ++II;
          CV->eraseFromParent();
          Changed = true;
          continue;
        }

      if (auto *DV = dyn_cast<ReleaseValueInst>(Inst))
        if (expandReleaseValue(DV)) {
          ++II;
          DV->eraseFromParent();
          Changed = true;
          continue;
        }

      ++II;
    }
  }
  return Changed;
}

namespace {
class SILLowerAggregate : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    SILFunction *F = getFunction();
    LLVM_DEBUG(llvm::dbgs() << "***** LowerAggregate on function: " <<
          F->getName() << " *****\n");
    bool Changed = processFunction(*F);
    if (Changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }

};
} // end anonymous namespace


SILTransform *swift::createLowerAggregateInstrs() {
  return new SILLowerAggregate();
}
