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
#include "llvm/ADT/SmallVector.h"
using namespace swift;

namespace {

struct GenericSpecializer {
  GenericSpecializer(CallGraph &CG) : CG(CG) {}

private:
  CallGraph &CG;

  /// A worklist of functions to specialize.
  std::vector<SILFunction*> Worklist;

  bool specializeApplyInstGroup(llvm::SmallVectorImpl<ApplySite> &NewApplies);

public:
  /// Collect and specialize calls in a specific order specified by
  /// \p BotUpFuncList.
  bool specialize(const llvm::SmallVectorImpl<SILFunction *> &BotUpFuncList);
};


static void addApplyInst(ApplySite AI,
                         CallGraph &CG,
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
    if (!M.linkFunction(Callee, SILModule::LinkingMode::LinkAll,
                        CallGraphLinkerEditor(CG).getCallback()))
      return;

  NewApplies.push_back(AI);
}

static void collectApplyInst(SILFunction &F,
                             CallGraph &CG,
                             llvm::SmallVectorImpl<ApplySite> &NewApplies) {
  // Scan all of the instructions in this function in search of ApplyInsts.
  for (auto &BB : F)
    for (auto &I : BB)
      if (ApplySite AI = ApplySite::isa(&I))
        addApplyInst(AI, CG, NewApplies);
}

bool
GenericSpecializer::specializeApplyInstGroup(
                                 llvm::SmallVectorImpl<ApplySite> &ApplyGroup) {
  bool Changed = false;

  SILFunction *NewFunction;
  llvm::SmallVector<FullApplySite, 4> NewApplies;
  CallGraphEditor Editor(CG);
  for (auto AI : ApplyGroup) {
    auto Specialized = trySpecializeApplyOfGeneric(AI, NewFunction, NewApplies);
    if (Specialized) {
      // We need to add a call graph node first if there was a new
      // function created, so that if we notify the call graph of the
      // new apply it can look up the node.

      if (NewFunction) {
        Editor.addCallGraphNode(NewFunction);
        Worklist.push_back(NewFunction);
      }

      // For full applications, we need to notify the call graph of
      // the replacement of the old apply with a new one.
      if (FullApplySite::isa(Specialized.getInstruction()))
        Editor.replaceApplyWithNew(FullApplySite(AI.getInstruction()),
                                   FullApplySite(Specialized.getInstruction()));

      // We collect new applies from the cloned function during
      // cloning, and need to reflect them in the call graph.
      while (!NewApplies.empty()) {
        Editor.addEdgesForApply(NewApplies.back());
        NewApplies.pop_back();
      }

      AI.getInstruction()->replaceAllUsesWith(Specialized.getInstruction());
      recursivelyDeleteTriviallyDeadInstructions(AI.getInstruction(), true);

      Changed = true;
    }
  }
  
  ApplyGroup.clear();
  return Changed;
}

/// Collect and specialize calls in a specific order specified by
/// \p BotUpFuncList.
bool GenericSpecializer::specialize(const llvm::SmallVectorImpl<SILFunction *>
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

    collectApplyInst(*F, CG, NewApplies);
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
    auto &CG = CGA->getOrBuildCallGraph();
    auto GS = GenericSpecializer(CG);

    // Try to specialize generic calls.
    bool Changed = GS.specialize(CG.getBottomUpFunctionOrder());

    if (Changed) {
      // Schedule another iteration of the transformation pipe.
      PM->scheduleAnotherIteration();

      // We are creating new functions and modifying calls, but we are
      // preserving the branches in the existing functions, and we are
      // maintaining the call graph.
      CGA->lockInvalidation();
      invalidateAnalysis(SILAnalysis::PreserveKind::Branches);
      CGA->unlockInvalidation();
    }
  }

  StringRef getName() override { return "Generic Specialization"; }
};
} // end anonymous namespace


SILTransform *swift::createGenericSpecializer() {
  return new SILGenericSpecializerTransform();
}
