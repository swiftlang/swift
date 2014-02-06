//===- PerformanceInliner.cpp - Basic cost based inlining for performance -===//
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

#define DEBUG_TYPE "sil-inliner"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/CallGraph.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/MapVector.h"
using namespace swift;

STATISTIC(NumFunctionsInlined, "Number of functions inlined");

namespace {
  class SILPerformanceInliner {
    const unsigned InlineCostThreshold;

    unsigned getFunctionCost(SILFunction *Callee,
                             SILFunction *Caller);
  public:
    explicit SILPerformanceInliner(unsigned threshold)
      : InlineCostThreshold(threshold) {}

    bool inlineCallsIntoFunction(SILFunction *F);
  };
}

//===----------------------------------------------------------------------===//
//                            Call Graph Creation
//===----------------------------------------------------------------------===//

/// \brief Returns a SILFunction if this ApplyInst calls a recognizable function
/// that is legal to inline.
static SILFunction *getInlinableFunction(ApplyInst *AI) {
  // Avoid substituion lists, we don't support them.
  if (AI->hasSubstitutions())
    return nullptr;

  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee().getDef());
  if (!FRI)
    return nullptr;

  SILFunction *F = FRI->getReferencedFunction();

  if (F->empty() || F->isExternalDeclaration()) {
    DEBUG(llvm::dbgs() << "  Can't inline " << F->getName() << ".\n");
    return nullptr;
  }

  DEBUG(llvm::dbgs() << "  Can inline " << F->getName() << ".\n");
  return F;
}

//===----------------------------------------------------------------------===//
//                                 Cost Model
//===----------------------------------------------------------------------===//

namespace {

// For now Free is 0 and Expensive is 1. This can be changed in the future by
// adding more categories.
enum class InlineCost : unsigned {
  Free = 0,
  Expensive = 1,
  CannotBeInlined = UINT_MAX,
};

} // end anonymous namespace

static bool isValidLinkageForTransparentRef(SILLinkage linkage) {
  switch (linkage) {
  case SILLinkage::Public:
  case SILLinkage::PublicExternal:
  case SILLinkage::Shared:
    return true;

  case SILLinkage::Private:
  case SILLinkage::Hidden:
  case SILLinkage::HiddenExternal:
    return false;
  }
}

/// For now just assume that every SIL instruction is one to one with an LLVM
/// instruction. This is of course very much so not true.
///
/// TODO: Fill this out.
static InlineCost instructionInlineCost(SILInstruction &I,
                                        SILFunction *Caller) {
  switch (I.getKind()) {
    case ValueKind::BuiltinFunctionRefInst:
    case ValueKind::GlobalAddrInst:
    case ValueKind::IntegerLiteralInst:
    case ValueKind::FloatLiteralInst:
    case ValueKind::DebugValueInst:
    case ValueKind::DebugValueAddrInst:
    case ValueKind::StringLiteralInst:
      return InlineCost::Free;

    // Private symbol references cannot be inlined into transparent functions.
    case ValueKind::FunctionRefInst:
      if (Caller->isTransparent()
          && !isValidLinkageForTransparentRef(
              cast<FunctionRefInst>(I).getReferencedFunction()->getLinkage())) {
        return InlineCost::CannotBeInlined;
      }
      return InlineCost::Free;

    case ValueKind::SILGlobalAddrInst:
      if (Caller->isTransparent()
          && !isValidLinkageForTransparentRef(
              cast<SILGlobalAddrInst>(I).getReferencedGlobal()->getLinkage())) {
        return InlineCost::CannotBeInlined;
      }
      return InlineCost::Free;

    case ValueKind::TupleElementAddrInst:
    case ValueKind::StructElementAddrInst: {
      // A gep whose operand is a gep with no other users will get folded by
      // LLVM into one gep implying the second should be free.
      SILValue Op = I.getOperand(0);
      if ((Op->getKind() == ValueKind::TupleElementAddrInst ||
           Op->getKind() == ValueKind::StructElementAddrInst) &&
          Op->hasOneUse())
        return InlineCost::Free;
    }
    // Aggregates are exploded at the IR level; these are effectively no-ops.
    case ValueKind::TupleInst:
    case ValueKind::StructInst:
    case ValueKind::StructExtractInst:
    case ValueKind::TupleExtractInst:
      return InlineCost::Free;

    // Unchecked casts are free.
    case ValueKind::AddressToPointerInst:
    case ValueKind::PointerToAddressInst:

    case ValueKind::ObjectPointerToRefInst:
    case ValueKind::RefToObjectPointerInst:

    case ValueKind::RawPointerToRefInst:
    case ValueKind::RefToRawPointerInst:

    case ValueKind::UpcastExistentialRefInst:
    case ValueKind::UpcastInst:

    case ValueKind::ThinToThickFunctionInst:
    case ValueKind::ConvertFunctionInst:
      return InlineCost::Free;

    case ValueKind::MetatypeInst:
      // Thin metatypes are always free.
      if (I.getType(0).castTo<MetatypeType>()->isThin())
        return InlineCost::Free;
      // TODO: Thick metatypes are free if they don't require generic or lazy
      // instantiation.
      return InlineCost::Expensive;

    // Return and unreachable are free.
    case ValueKind::UnreachableInst:
    case ValueKind::ReturnInst:
      return InlineCost::Free;

    // TODO
    case ValueKind::ApplyInst: {
      // Don't inline functions that contain recursions.
      ApplyInst *AI = cast<ApplyInst>(&I);
      auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee().getDef());
      if (FRI && FRI->getReferencedFunction() == AI->getFunction())
        return InlineCost::CannotBeInlined;
 
      return InlineCost::Expensive;
    }

    case ValueKind::AllocArrayInst:
    case ValueKind::AllocBoxInst:
    case ValueKind::AllocRefInst:
    case ValueKind::AllocStackInst:
    case ValueKind::ArchetypeMetatypeInst:
    case ValueKind::ArchetypeMethodInst:
    case ValueKind::ArchetypeRefToSuperInst:
    case ValueKind::AssignInst:
    case ValueKind::AutoreleaseReturnInst:
    case ValueKind::BranchInst:
    case ValueKind::BridgeToBlockInst:
    case ValueKind::CheckedCastBranchInst:
    case ValueKind::ClassMetatypeInst:
    case ValueKind::ClassMethodInst:
    case ValueKind::CondBranchInst:
    case ValueKind::CondFailInst:
    case ValueKind::CopyAddrInst:
    case ValueKind::CopyValueInst:
    case ValueKind::DeallocBoxInst:
    case ValueKind::DeallocRefInst:
    case ValueKind::DeallocStackInst:
    case ValueKind::DeinitExistentialInst:
    case ValueKind::DestroyAddrInst:
    case ValueKind::DestroyValueInst:
    case ValueKind::DynamicMethodBranchInst:
    case ValueKind::DynamicMethodInst:
    case ValueKind::EnumInst:
    case ValueKind::IndexAddrInst:
    case ValueKind::IndexRawPointerInst:
    case ValueKind::InitEnumDataAddrInst:
    case ValueKind::InitExistentialInst:
    case ValueKind::InitExistentialRefInst:
    case ValueKind::InjectEnumAddrInst:
    case ValueKind::IsNonnullInst:
    case ValueKind::LoadInst:
    case ValueKind::LoadWeakInst:
    case ValueKind::PartialApplyInst:
    case ValueKind::PeerMethodInst:
    case ValueKind::ProjectExistentialInst:
    case ValueKind::ProjectExistentialRefInst:
    case ValueKind::ProtocolMetatypeInst:
    case ValueKind::ProtocolMethodInst:
    case ValueKind::RefElementAddrInst:
    case ValueKind::RefToUnownedInst:
    case ValueKind::StoreInst:
    case ValueKind::StoreWeakInst:
    case ValueKind::StrongReleaseInst:
    case ValueKind::StrongRetainAutoreleasedInst:
    case ValueKind::StrongRetainInst:
    case ValueKind::StrongRetainUnownedInst:
    case ValueKind::SuperMethodInst:
    case ValueKind::SwitchEnumAddrInst:
    case ValueKind::SwitchEnumInst:
    case ValueKind::SwitchIntInst:
    case ValueKind::TakeEnumDataAddrInst:
    case ValueKind::UnconditionalCheckedCastInst:
    case ValueKind::UnownedReleaseInst:
    case ValueKind::UnownedRetainInst:
    case ValueKind::UnownedToRefInst:
    case ValueKind::UpcastExistentialInst:
      return InlineCost::Expensive;

    case ValueKind::SILArgument:
    case ValueKind::SILUndef:
      llvm_unreachable("Only instructions should be passed into this "
                       "function.");
    case ValueKind::MarkFunctionEscapeInst:
    case ValueKind::MarkUninitializedInst:
      llvm_unreachable("not valid in canonical sil");
  }
}

/// \brief Returns the inlining cost of the function.
unsigned SILPerformanceInliner::getFunctionCost(SILFunction *F,
                                                SILFunction *Caller) {
  DEBUG(llvm::dbgs() << "  Calculating cost for " << F->getName() << ".\n");

  if (F->isTransparent() == IsTransparent_t::IsTransparent)
    return 0;

  unsigned Cost = 0;
  for (auto &BB : *F) {
    for (auto &I : BB) {
      auto ICost = instructionInlineCost(I, Caller);
      if (ICost == InlineCost::CannotBeInlined)
        return UINT_MAX;

      Cost += unsigned(ICost);

      // If we're debugging, continue calculating the total cost even if we
      // passed the threshold.
      DEBUG(continue);

      // If i is greater than the InlineCostThreshold, we already know we are
      // not going to inline this given function, so there is no point in
      // continuing to visit instructions.
      if (Cost > InlineCostThreshold)
        return Cost;
    }
  }

  DEBUG(llvm::dbgs() << "  Found cost: " << Cost << "\n");
  return Cost;
}

//===----------------------------------------------------------------------===//
//                                  Inliner
//===----------------------------------------------------------------------===//

/// \brief Attempt to inline all calls smaller than our threshold into F until.
/// returns True if a function was inlined.
bool SILPerformanceInliner::inlineCallsIntoFunction(SILFunction *Caller) {
  bool Changed = false;
  SILInliner Inliner(*Caller, SILInliner::InlineKind::PerformanceInline);

  DEBUG(llvm::dbgs() << "Visiting Function: " << Caller->getName() << "\n");

  llvm::SmallVector<ApplyInst*, 8> CallSites;

  // Collect all of the ApplyInsts in this function. We will be changing the
  // control flow and collecting the AIs simplifies the scan.
  for (auto &BB : *Caller) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      // Check if this is a call site.
      ApplyInst *AI = dyn_cast<ApplyInst>(I++);
      if (AI)
        CallSites.push_back(AI);
    }
  }

  for (auto AI : CallSites) {
    DEBUG(llvm::dbgs() << "  Found call site:" <<  *AI);

    // Get the callee.
    SILFunction *Callee = getInlinableFunction(AI);
    if (!Callee)
      continue;

    DEBUG(llvm::dbgs() << "  Found callee:" <<  Callee->getName() << ".\n");

    // Prevent circular inlining.
    if (Callee == Caller) {
      DEBUG(llvm::dbgs() << "  Skipping recursive calls.\n");
      continue;
    }

    // Calculate the inlining cost of the callee.
    unsigned CalleeCost = getFunctionCost(Callee, Caller);

    if (CalleeCost > InlineCostThreshold) {
      DEBUG(llvm::dbgs() << "  Function too big to inline. Skipping.\n");
      continue;
    }

    // Add the arguments from AI into a SILValue list.
    SmallVector<SILValue, 8> Args;
    for (const auto &Arg : AI->getArguments())
    Args.push_back(Arg);

    // Ok, we are within budget. Attempt to inline.
    DEBUG(llvm::dbgs() << "  Inlining " << Callee->getName() << " Into " <<
          Caller->getName() << "\n");

    // We already moved the iterator to the next instruction because the AI
    // will be erased by the inliner. Notice that we will skip all of the
    // newly inlined ApplyInsts. That's okay because we will visit them in
    // our next invocation of the inliner.
    Inliner.inlineFunction(AI, Callee, ArrayRef<Substitution>(), Args);
    NumFunctionsInlined++;
    Changed = true;
  }
  return Changed;
}

class SILPerformanceInlinerPass : public SILModuleTransform {
  unsigned Threshold;

public:
  SILPerformanceInlinerPass(unsigned th) : Threshold(th) {}

  virtual void runOnModule(SILModule &M, SILPassManager *PM) {
    CallGraphAnalysis* CGA = PM->getAnalysis<CallGraphAnalysis>();

    if (Threshold == 0) {
      DEBUG(llvm::dbgs() << "*** The Performance Inliner is disabled ***\n");
      return;
    }

    // Initialize the worklist with a bottom-up call-graph order list of
    // functions.
    const std::vector<SILFunction *> &Order = CGA->bottomUpCallGraphOrder();
    std::vector<SILFunction *> Worklist(Order);
    std::reverse(Worklist.begin(), Worklist.end());

    SILPerformanceInliner inliner(Threshold);

    bool Changed = false;
    while (!Worklist.empty()) {
      SILFunction *F = Worklist.back();
      Worklist.pop_back();
      Changed |= inliner.inlineCallsIntoFunction(F);
    }

    // Invalidate the call graph.
    if (Changed)
      CGA->invalidate(SILAnalysis::InvalidationKind::CallGraph);
  }
};

SILTransform *swift::createPerfInliner(unsigned threshold) {
  return new SILPerformanceInlinerPass(threshold);
}
