//===-- Specializer.cpp ------ Performs Generic Specialization ------------===//
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

#define DEBUG_TYPE "generic-specializer"

#include "swift/SILPasses/Utils/Generics.h"
#include "swift/SILPasses/Passes.h"

#include "swift/AST/ASTContext.h"

#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Utils/Generics.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/SmallString.h"
using namespace swift;

namespace {

struct GenericSpecializer {
  GenericSpecializer() {}

  private:
  /// A worklist of functions to specialize.
  std::vector<SILFunction*> Worklist;

  bool specializeApplyInstGroup(llvm::SmallVectorImpl<ApplySite> &NewApplies);

  public:
  /// Collect and specialize calls in a specific order specified by
  /// \p BotUpFuncList.
  bool specialize(const std::vector<SILFunction *> &BotUpFuncList);
};


static void addApplyInst(ApplySite AI,
                         llvm::SmallVectorImpl<ApplySite> &NewApplies) {
  if (!AI || !AI.hasSubstitutions())
    return;

  SILValue CalleeVal = AI.getCallee();
  FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);

  if (!FRI)
    return;

  SILFunction *Callee = FRI->getReferencedFunction();
  auto &M = AI.getInstruction()->getModule();
  if (Callee->isExternalDeclaration())
    if (!M.linkFunction(Callee, SILModule::LinkingMode::LinkAll))
      return;

  NewApplies.push_back(AI);
}

static void collectApplyInst(SILFunction &F,
                             llvm::SmallVectorImpl<ApplySite> &NewApplies) {
  // Scan all of the instructions in this function in search of ApplyInsts.
  for (auto &BB : F)
    for (auto &I : BB)
      if (ApplySite AI = ApplySite::isa(&I))
        addApplyInst(AI, NewApplies);
}

bool
GenericSpecializer::specializeApplyInstGroup(
                                 llvm::SmallVectorImpl<ApplySite> &NewApplies) {
  bool Changed = false;

  SILFunction *NewFunction;
  for (auto &AI : NewApplies) {
    if (trySpecializeApplyOfGeneric(AI, &NewFunction)) {
      if (NewFunction)
        Worklist.push_back(NewFunction);
      Changed = true;
    }
  }
  
  NewApplies.clear();
  return Changed;
}

/// Collect and specialize calls in a specific order specified by
/// \p BotUpFuncList.
bool GenericSpecializer::specialize(const std::vector<SILFunction *>
                                    &BotUpFuncList) {
  // Initialize the worklist with a call-graph bottom-up list of functions.
  // We specialize the functions in a top-down order, starting from the end
  // of the list.
  Worklist.insert(Worklist.begin(), BotUpFuncList.begin(),
                  BotUpFuncList.end());

  llvm::SmallVector<ApplySite, 16> NewApplies;
  bool Changed = false;

  // Try to specialize generic calls.
  while (Worklist.size()) {
    SILFunction *F = Worklist.back();
    Worklist.pop_back();

    collectApplyInst(*F, NewApplies);
    if (!NewApplies.empty())
      Changed |= specializeApplyInstGroup(NewApplies);

    assert(NewApplies.empty() && "Expected all applies processed!");
  }
  return Changed;
}

class SILGenericSpecializerTransform : public SILModuleTransform {
public:
  SILGenericSpecializerTransform() {}

  void run() override {
    CallGraphAnalysis* CGA = PM->getAnalysis<CallGraphAnalysis>();

    // Collect a call-graph bottom-up list of functions and specialize the
    // functions in reverse order.
    auto &CG = CGA->getCallGraph();
    auto GS = GenericSpecializer();

    // Try to specialize generic calls.
    bool Changed = GS.specialize(CG.getBottomUpFunctionOrder());

    if (Changed) {
      // Schedule another iteration of the transformation pipe.
      PM->scheduleAnotherIteration();

      // We are creating new functions and modifying calls, but we are
      // preserving the branches in the existing functions.
      invalidateAnalysis(SILAnalysis::PreserveKind::Branches);
    }
  }

  StringRef getName() override { return "Generic Specialization"; }
};
} // end anonymous namespace


SILTransform *swift::createGenericSpecializer() {
  return new SILGenericSpecializerTransform();
}
