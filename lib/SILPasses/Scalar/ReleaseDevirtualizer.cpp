//===---- ReleaseDevirtualizer.cpp - Devirtualizes release-instructions ---===//
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

#define DEBUG_TYPE "release-devirtualizer"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/Statistic.h"

STATISTIC(NumReleasesDevirtualized, "Number of devirtualized releases");

using namespace swift;

namespace {

/// Devirtualizes release instructions which are known to destruct the object.
/// This means, it replaces a sequence of
///    %x = alloc_ref [stack] $X
///      ...
///    strong_release %x
///    dealloc_ref [stack] %x
/// with
///    %x = alloc_ref [stack] $X
///      ...
///    %d = function_ref @deinit_of_X
///    %a = apply %d(%x)
///    dealloc_ref [stack] %x
///
/// It also works for array buffers, where the allocation/deallocation is done
/// by calls to the swift_bufferAllocateOnStack/swift_bufferDeallocateFromStack
/// functions.
///
/// The optimization is only done for stack promoted objects because they are
/// known to have no associated objects (which are not explicitly released
/// in the deinit method).
class ReleaseDevirtualizer : public SILFunctionTransform {

public:
  ReleaseDevirtualizer() {}

private:
  /// The entry point to the transformation.
  void run() override;

  /// Devirtualize releases of array buffers.
  bool devirtualizeReleaseOfObject(SILInstruction *ReleaseInst,
                                   DeallocRefInst *DeallocInst);

  /// Devirtualize releases of swift objects.
  bool devirtualizeReleaseOfBuffer(SILInstruction *ReleaseInst,
                                   ApplyInst *DeallocCall);

  /// Replace the release-instruction \p ReleaseInst with an explicit call to
  /// the destructor of \p AllocType for \p object.
  bool createDeinitCall(SILType AllocType, SILInstruction *ReleaseInst,
                        SILValue object);

  StringRef getName() override { return "Release Devirtualizer"; }

  RCIdentityFunctionInfo *RCIA = nullptr;
};

void ReleaseDevirtualizer::run() {
  DEBUG(llvm::dbgs() << "** ReleaseDevirtualizer **\n");

  SILFunction *F = getFunction();
  RCIA = PM->getAnalysis<RCIdentityAnalysis>()->get(F);

  bool Changed = false;
  for (SILBasicBlock &BB : *F) {

    // The last release_value or strong_release instruction before the
    // deallocation.
    SILInstruction *LastRelease = nullptr;

    for (SILInstruction &I : BB) {
      if (LastRelease) {
        if (auto *DRI = dyn_cast<DeallocRefInst>(&I)) {
          Changed |= devirtualizeReleaseOfObject(LastRelease, DRI);
          continue;
        }
        if (auto *AI = dyn_cast<ApplyInst>(&I)) {
          Changed |= devirtualizeReleaseOfBuffer(LastRelease, AI);
          continue;
        }
      }

      if (isa<ReleaseValueInst>(&I) ||
          isa<StrongReleaseInst>(&I)) {
        LastRelease = &I;
      } else if (I.mayReleaseOrReadRefCount()) {
        LastRelease = nullptr;
      }
    }
  }
  if (Changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
  }
}

bool ReleaseDevirtualizer::
devirtualizeReleaseOfObject(SILInstruction *ReleaseInst,
                            DeallocRefInst *DeallocInst) {

  // We only do the optimization for stack promoted object, because for these
  // we know that they don't have associated objects, which are _not_ released
  // by the deinit method.
  // This restriction is no problem because only stack promotion result in this
  // alloc-release-dealloc pattern.
  if (!DeallocInst->canAllocOnStack())
    return false;

  // Is the dealloc_ref paired with an alloc_ref?
  AllocRefInst *ARI = dyn_cast<AllocRefInst>(DeallocInst->getOperand());
  if (!ARI)
    return false;

  // Does the last release really release the allocated object?
  SILValue rcRoot = RCIA->getRCIdentityRoot(ReleaseInst->getOperand(0));
  if (rcRoot != ARI)
    return false;

  SILType AllocType = ARI->getType();
  return createDeinitCall(AllocType, ReleaseInst, ARI);
}

bool ReleaseDevirtualizer::
devirtualizeReleaseOfBuffer(SILInstruction *ReleaseInst,
                            ApplyInst *DeallocCall) {

  // Is this a deallocation of a buffer?
  SILFunction *DeallocFn = DeallocCall->getCalleeFunction();
  if (!DeallocFn || DeallocFn->getName() != "swift_bufferDeallocateFromStack")
    return false;

  // Is the deallocation call paired with an allocation call?
  ApplyInst *AllocAI = dyn_cast<ApplyInst>(DeallocCall->getArgument(0));
  if (!AllocAI || AllocAI->getNumArguments() < 1)
    return false;

  SILFunction *AllocFunc = AllocAI->getCalleeFunction();
  if (!AllocFunc || AllocFunc->getName() != "swift_bufferAllocateOnStack")
    return false;

  // Can we find the buffer type which is allocated? It's metatype is passed
  // as first argument to the allocation function.
  auto *IEMTI = dyn_cast<InitExistentialMetatypeInst>(AllocAI->getArgument(0));
  if (!IEMTI)
    return false;

  SILType MType = IEMTI->getOperand().getType();
  auto *MetaType = MType.getSwiftRValueType()->getAs<AnyMetatypeType>();
  if (!MetaType)
    return false;

  // Is the allocated buffer a class type? This should always be the case.
  auto *ClType = MetaType->getInstanceType()->getAs<BoundGenericClassType>();
  if (!ClType)
    return false;

  // Does the last release really release the allocated buffer?
  SILValue rcRoot = RCIA->getRCIdentityRoot(ReleaseInst->getOperand(0));
  if (rcRoot != AllocAI)
    return false;

  SILType SILClType = SILType::getPrimitiveObjectType(CanType(ClType));
  return createDeinitCall(SILClType, ReleaseInst, AllocAI);
}

bool ReleaseDevirtualizer::createDeinitCall(SILType AllocType,
                                            SILInstruction *ReleaseInst,
                                            SILValue object) {
  ClassDecl *Cl = AllocType.getClassOrBoundGenericClass();
  assert(Cl && "no class type allocated with alloc_ref");

  // Find the destructor of the type.
  DestructorDecl *Destructor = Cl->getDestructor();
  SILDeclRef DeinitRef(Destructor, SILDeclRef::Kind::Destroyer);
  SILModule &M = ReleaseInst->getFunction()->getModule();
  SILFunction *Deinit = M.lookUpFunction(DeinitRef);
  if (!Deinit)
    return false;

  CanSILFunctionType DeinitType = Deinit->getLoweredFunctionType();
  ArrayRef<Substitution> AllocSubsts = AllocType.gatherAllSubstitutions(M);

  assert(!AllocSubsts.empty() == DeinitType->isPolymorphic() &&
         "deinit of generic class is not polymorphic or vice versa");

  if (DeinitType->isPolymorphic())
    DeinitType = DeinitType->substGenericArgs(M, M.getSwiftModule(),
                                              AllocSubsts);

  SILType ReturnType = DeinitType->getResult().getSILType();
  SILType DeinitSILType = SILType::getPrimitiveObjectType(DeinitType);

  SILBuilder B(ReleaseInst);
  if (object.getType() != AllocType)
    object = B.createUncheckedRefCast(ReleaseInst->getLoc(), object, AllocType);

  // Create the call to the destructor with the allocated object as self
  // argument.
  auto *MI = B.createFunctionRef(ReleaseInst->getLoc(), Deinit);
  B.createApply(ReleaseInst->getLoc(), MI, DeinitSILType, ReturnType,
                AllocSubsts, { object }, false);

  NumReleasesDevirtualized++;
  ReleaseInst->eraseFromParent();
  return true;
}

} // end anonymous namespace

SILTransform *swift::createReleaseDevirtualizer() {
  return new ReleaseDevirtualizer();
}
