//===--- ExistentialSpecializer.cpp - Specialization of functions -----===//
//===---                 with existential arguments               -----===//
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
// Specialize functions with existential parameters to generic ones.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-existential-specializer"
#include "ExistentialTransform.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Existential.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumFunctionsWithExistentialArgsSpecialized,
          "Number of functions with existential args specialized");

namespace {

/// ExistentialSpecializer class.
class ExistentialSpecializer : public SILFunctionTransform {

  /// Determine if the current function is a target for existential
  /// specialization of args.
  bool canSpecializeExistentialArgsInFunction(
      ApplySite &Apply,
      llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
          &ExistentialArgDescriptor);

  /// Can Callee be specialized?
  bool canSpecializeCalleeFunction(ApplySite &Apply);

  /// Specialize existential args in function F.
  void specializeExistentialArgsInAppliesWithinFunction(SILFunction &F);

  /// CallerAnalysis information.
  CallerAnalysis *CA;

public:
  void run() override {

    auto *F = getFunction();

    /// Don't optimize functions that should not be optimized.
    if (!F->shouldOptimize() || !F->getModule().getOptions().ExistentialSpecializer) {
      return;
    }

    /// Get CallerAnalysis information handy.
    CA = PM->getAnalysis<CallerAnalysis>();

    /// Perform specialization.
    specializeExistentialArgsInAppliesWithinFunction(*F);
  }
};
} // namespace

/// Find concrete type from init_existential_refs/addrs.
static bool findConcreteTypeFromInitExistential(SILValue Arg,
                                                CanType &ConcreteType) {
  if (auto *IER = dyn_cast<InitExistentialRefInst>(Arg)) {
    ConcreteType = IER->getFormalConcreteType();
    return true;
  } else if (auto *IE = dyn_cast<InitExistentialAddrInst>(Arg)) {
    ConcreteType = IE->getFormalConcreteType();
    return true;
  }
  return false;
}

/// Find the concrete type of the existential argument. Wrapper
/// for findInitExistential in Local.h/cpp. In future, this code
/// can move to Local.h/cpp.
static bool findConcreteType(ApplySite AI, int ArgIdx, CanType &ConcreteType) {
  bool isCopied = false;
  auto Arg = AI.getArgument(ArgIdx);

  /// Ignore any unconditional cast instructions. Is it Safe? Do we need to
  /// also add UnconditionalCheckedCastAddrInst? TODO.
  if (auto *Instance = dyn_cast<UnconditionalCheckedCastInst>(Arg)) {
    Arg = Instance->getOperand();
  }

  /// Return init_existential if the Arg is global_addr.
  if (auto *GAI = dyn_cast<GlobalAddrInst>(Arg)) {
    SILValue InitExistential =
        findInitExistentialFromGlobalAddrAndApply(GAI, AI, ArgIdx);
    /// If the Arg is already init_existential, return the concrete type.
    if (InitExistential &&
        findConcreteTypeFromInitExistential(InitExistential, ConcreteType)) {
      return true;
    }
  }

  /// Handle AllocStack instruction separately.
  if (auto *Instance = dyn_cast<AllocStackInst>(Arg)) {
    if (SILValue Src =
            getAddressOfStackInit(Instance, AI.getInstruction(), isCopied)) {
      Arg = Src;
    }
  }

  /// If the Arg is already init_existential after getAddressofStackInit
  /// call, return the concrete type.
  if (findConcreteTypeFromInitExistential(Arg, ConcreteType)) {
    return true;
  }

  /// Call findInitExistential and determine the init_existential.
  ArchetypeType *OpenedArchetype = nullptr;
  SILValue OpenedArchetypeDef;
  auto FAS = FullApplySite::isa(AI.getInstruction());
  if (!FAS)
    return false;
  SILInstruction *InitExistential =
      findInitExistential(FAS.getArgumentOperands()[ArgIdx], OpenedArchetype,
                          OpenedArchetypeDef, isCopied);
  if (!InitExistential) {
    LLVM_DEBUG(llvm::dbgs() << "ExistentialSpecializer Pass: Bail! Due to "
                               "findInitExistential\n";);
    return false;
  }

  /// Return the concrete type from init_existential returned from
  /// findInitExistential.
  if (auto *SingleVal = InitExistential->castToSingleValueInstruction()) {
    if (findConcreteTypeFromInitExistential(SingleVal, ConcreteType)) {
      return true;
    }
  }
  return false;
}

/// Check if the argument Arg is used in a destroy_use instruction.
static void
findIfCalleeUsesArgInDestroyUse(SILValue Arg,
                                ExistentialTransformArgumentDescriptor &ETAD) {
  for (Operand *ArgUse : Arg->getUses()) {
    auto *ArgUser = ArgUse->getUser();
    if (isa<DestroyAddrInst>(ArgUser)) {
      ETAD.DestroyAddrUse = true;
      break;
    }
  }
}

/// Check if any apply argument meets the criteria for existential
/// specialization.
bool ExistentialSpecializer::canSpecializeExistentialArgsInFunction(
    ApplySite &Apply,
    llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
        &ExistentialArgDescriptor) {
  auto *F = Apply.getReferencedFunction();
  auto Args = F->begin()->getFunctionArguments();
  bool returnFlag = false;

  /// Analyze the argument for protocol conformance.
  for (unsigned Idx = 0, Num = Args.size(); Idx < Num; ++Idx) {
    auto Arg = Args[Idx];
    auto ArgType = Arg->getType();
    auto SwiftArgType = ArgType.getASTType();

    /// Checking for AnyObject and Any is added to ensure that we do not blow up
    /// the code size by specializing to every type that conforms to Any or
    /// AnyObject. In future, we may want to lift these two restrictions in a
    /// controlled way.
    if (!ArgType.isExistentialType() || ArgType.isAnyObject() ||
        SwiftArgType->isAny())
      continue;

    auto ExistentialRepr =
        Arg->getType().getPreferredExistentialRepresentation(F->getModule());
    if (ExistentialRepr != ExistentialRepresentation::Opaque &&
          ExistentialRepr != ExistentialRepresentation::Class)
      continue;

    /// Find the concrete type.
    CanType ConcreteType;
    if (!findConcreteType(Apply, Idx, ConcreteType)) {
      LLVM_DEBUG(
          llvm::dbgs()
              << "ExistentialSpecializer Pass: Bail! Due to findConcreteType "
                 "for callee:"
              << F->getName() << " in caller:"
              << Apply.getInstruction()->getParent()->getParent()->getName()
              << "\n";);
      continue;
    }

    /// Determine attributes of the existential addr arguments such as
    /// destroy_use, immutable_access. 
    ExistentialTransformArgumentDescriptor ETAD;
    ETAD.AccessType =
        Apply.getOrigCalleeType()->getParameters()[Idx].isIndirectMutating() ||
                Apply.getOrigCalleeType()->getParameters()[Idx].isConsumed()
            ? OpenedExistentialAccess::Mutable
            : OpenedExistentialAccess::Immutable;
    ETAD.DestroyAddrUse = false;
    if ((Args[Idx]->getType().getPreferredExistentialRepresentation(
            F->getModule())) != ExistentialRepresentation::Class)
      findIfCalleeUsesArgInDestroyUse(Arg, ETAD);

    /// Save the attributes
    ExistentialArgDescriptor[Idx] = ETAD;
    LLVM_DEBUG(llvm::dbgs()
               << "ExistentialSpecializer Pass:Function: " << F->getName()
               << " Arg:" << Idx << "has a concrete type.\n");
    returnFlag |= true;
  }
  return returnFlag;
}

/// Determine if this callee function can be specialized or not.
bool ExistentialSpecializer::canSpecializeCalleeFunction(ApplySite &Apply) {

  /// Do not handle partial applies.
  if (isa<PartialApplyInst>(Apply.getInstruction())) {
    return false;
  }

  /// Determine the caller of the apply.
  auto *Callee = Apply.getReferencedFunction();
  if (!Callee)
    return false;

  /// Callee should be optimizable.
  if (!Callee->shouldOptimize())
    return false;

  /// External function definitions.
  if (!Callee->isDefinition())
    return false;

  /// Ignore functions with indirect results.
  if (Callee->getConventions().hasIndirectSILResults())
    return false;

  /// Ignore error returning functions.
  if (Callee->getLoweredFunctionType()->hasErrorResult()) 
    return false;

  /// Do not optimize always_inlinable functions.
  if (Callee->getInlineStrategy() == Inline_t::AlwaysInline)
    return false;

  /// Only choose a select few function representations for specialization.
  switch (Callee->getRepresentation()) {
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Block:
    return false;
  default: break;
  }
  return true;
}

/// Specialize existential args passed as arguments to callees. Iterate over all
/// call sites of the caller F and check for legality to apply existential
/// specialization.
void ExistentialSpecializer::specializeExistentialArgsInAppliesWithinFunction(
    SILFunction &F) {
  bool Changed = false;
  for (auto &BB : F) {
    for (auto It = BB.begin(), End = BB.end(); It != End; ++It) {
      auto *I = &*It;

      /// Is it an apply site?
      ApplySite Apply = ApplySite::isa(I);
      if (!Apply)
        continue;

      /// Can the callee be specialized?
      if (!canSpecializeCalleeFunction(Apply)) {
        LLVM_DEBUG(llvm::dbgs() << "ExistentialSpecializer Pass: Bail! Due to "
                                   "canSpecializeCalleeFunction.\n";
                   I->dump(););
        continue;
      }

      auto *Callee = Apply.getReferencedFunction();

      /// Determine the arguments that can be specialized.
      llvm::SmallDenseMap<int, ExistentialTransformArgumentDescriptor>
          ExistentialArgDescriptor;
      if (!canSpecializeExistentialArgsInFunction(Apply,
                                                  ExistentialArgDescriptor)) {
        LLVM_DEBUG(llvm::dbgs()
                   << "ExistentialSpecializer Pass: Bail! Due to "
                      "canSpecializeExistentialArgsInFunction in function: "
                   << Callee->getName() << " -> abort\n");
        continue;
      }

      LLVM_DEBUG(llvm::dbgs()
                 << "ExistentialSpecializer Pass: Function::"
                 << Callee->getName()
                 << " has an existential argument and can be optimized "
                    "via ExistentialSpecializer\n");

      /// Name Mangler for naming the protocol constrained generic method.
      auto P = Demangle::SpecializationPass::FunctionSignatureOpts;
      Mangle::FunctionSignatureSpecializationMangler Mangler(
          P, Callee->isSerialized(), Callee);

      /// Save the arguments in a descriptor.
      llvm::SpecificBumpPtrAllocator<ProjectionTreeNode> Allocator;
      llvm::SmallVector<ArgumentDescriptor, 4> ArgumentDescList;
      auto Args = Callee->begin()->getFunctionArguments();
      for (unsigned i : indices(Args)) {
        ArgumentDescList.emplace_back(Args[i], Allocator);
      }

      /// This is the function to optimize for existential specilizer.
      LLVM_DEBUG(llvm::dbgs()
                 << "*** Running ExistentialSpecializer Pass on function: "
                 << Callee->getName() << " ***\n");

      /// Instantiate the ExistentialSpecializerTransform pass.
      SILOptFunctionBuilder FuncBuilder(*this);
      ExistentialTransform ET(FuncBuilder, Callee, Mangler, ArgumentDescList,
                              ExistentialArgDescriptor);

      /// Run the existential specializer pass.
      Changed = ET.run();

      if (Changed) {
        /// Update statistics on the number of functions specialized.
        ++NumFunctionsWithExistentialArgsSpecialized;

        /// Make sure the PM knows about the new specialized inner function.
        addFunctionToPassManagerWorklist(ET.getExistentialSpecializedFunction(),
                                         Callee);

        /// Invalidate analysis results of Callee.
        PM->invalidateAnalysis(Callee,
                               SILAnalysis::InvalidationKind::Everything);
      }
    }
  }
  return;
}

SILTransform *swift::createExistentialSpecializer() {
  return new ExistentialSpecializer();
}
