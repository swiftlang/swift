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

#define DEBUG_TYPE "specialization"
#include "swift/SILPasses/Passes.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Mangle.h"
#include "swift/SIL/Mangle.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumSpecialized, "Number of functions specialized");

/// Check if we can clone and remap types this function.
static bool canSpecializeFunction(SILFunction *F) {
  return !F->isExternalDeclaration();
}

llvm::cl::opt<bool> EnableSpecializer("enable-specializer",
                                      llvm::cl::init(true));

namespace {

class SpecializingCloner : public TypeSubstCloner<SpecializingCloner> {
public:
  SpecializingCloner(SILFunction *F,
                     TypeSubstitutionMap &InterfaceSubs,
                     TypeSubstitutionMap &ContextSubs,
                     StringRef NewName,
                     ArrayRef<Substitution> ApplySubs)
    : TypeSubstCloner(*initCloned(F, InterfaceSubs, NewName), *F, ContextSubs,
                      ApplySubs) {}
  /// Clone and remap the types in \p F according to the substitution
  /// list in \p Subs.
  static SILFunction *cloneFunction(SILFunction *F,
                                    TypeSubstitutionMap &InterfaceSubs,
                                    TypeSubstitutionMap &ContextSubs,
                                    StringRef NewName, ApplyInst *Caller) {
    // Clone and specialize the function.
    SpecializingCloner SC(F, InterfaceSubs, ContextSubs, NewName,
                          Caller->getSubstitutions());
    SC.populateCloned();
    return SC.getCloned();
  }

private:
  static SILFunction *initCloned(SILFunction *Orig,
                                 TypeSubstitutionMap &InterfaceSubs,
                                 StringRef NewName);
  /// Clone the body of the function into the empty function that was created
  /// by initCloned.
  void populateCloned();
  SILFunction *getCloned() { return &getBuilder().getFunction(); }
};

/// Create a new empty function with the correct arguments and a unique name.
SILFunction *SpecializingCloner::initCloned(SILFunction *Orig,
                                            TypeSubstitutionMap &InterfaceSubs,
                                            StringRef NewName) {
  SILModule &M = Orig->getModule();
  Module *SM = M.getSwiftModule();

  CanSILFunctionType FTy =
    SILType::substFuncType(M, SM, InterfaceSubs,
                           Orig->getLoweredFunctionType(),
                           /*dropGenerics = */ true);

  assert((Orig->isTransparent() || Orig->isBare() || Orig->getLocation())
         && "SILFunction missing location");
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getDebugScope())
         && "SILFunction missing DebugScope");
  assert(!Orig->isGlobalInit() && "Global initializer cannot be cloned");

  // Create a new empty function.
  SILFunction *NewF = SILFunction::create(
      M, getSpecializedLinkage(Orig->getLinkage()), NewName, FTy, nullptr,
      Orig->getLocation(), Orig->isBare(), Orig->isTransparent(),
      Orig->isFragile(), Orig->isThunk(), Orig->getClassVisibility(),
      Orig->getInlineStrategy(), Orig->getEffectsKind(), Orig,
      Orig->getDebugScope(), Orig->getDeclContext());
  NewF->setSemanticsAttr(Orig->getSemanticsAttr());

  NumSpecialized++;
  return NewF;
}

void SpecializingCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block.
  SILBasicBlock *OrigEntryBB = Original.begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);

  // Create the entry basic block with the function arguments.
  auto I = OrigEntryBB->bbarg_begin(), E = OrigEntryBB->bbarg_end();
  while (I != E) {
    SILValue MappedValue =
      new (M) SILArgument(ClonedEntryBB, remapType((*I)->getType()),
                          (*I)->getDecl());
    ValueMap.insert(std::make_pair(*I, MappedValue));
    ++I;
  }

  getBuilder().setInsertionPoint(ClonedEntryBB);
  BBMap.insert(std::make_pair(OrigEntryBB, ClonedEntryBB));
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(OrigEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    visit(BI->first->getTerminator());
  }
}


struct GenericSpecializer {
  /// A list of ApplyInst instructions.
  typedef SmallVector<ApplyInst *, 16> AIList;

  /// The SIL Module.
  SILModule *M;

  /// Maps a function to all of the ApplyInst that call it.
  llvm::MapVector<SILFunction *, AIList> ApplyInstMap;

  /// A worklist of functions to specialize.
  std::vector<SILFunction*> Worklist;

  GenericSpecializer(SILModule *Mod) : M(Mod) {}

  bool specializeApplyInstGroup(SILFunction *F, AIList &List);

  /// Scan the function and collect all of the ApplyInst with generic
  /// substitutions into buckets according to the called function.
  void collectApplyInst(SILFunction &F);

  /// The driver for the generic specialization pass.
  bool specialize(const std::vector<SILFunction *> &BotUpFuncList) {
    bool Changed = false;
    for (auto &F : *M)
      collectApplyInst(F);

    // Initialize the worklist with a call-graph bottom-up list of functions.
    // We specialize the functions in a top-down order, starting from the end
    // of the list.
    Worklist.insert(Worklist.begin(),
                    BotUpFuncList.begin(), BotUpFuncList.end());

    while (Worklist.size()) {
      SILFunction *F = Worklist.back();
      Worklist.pop_back();
      if (ApplyInstMap.count(F))
        Changed |= specializeApplyInstGroup(F, ApplyInstMap[F]);
    }
    return Changed;
  }
};

} // end anonymous namespace.

void GenericSpecializer::collectApplyInst(SILFunction &F) {
  // Don't collect apply inst from transparent functions since we do not want to
  // expose shared functions in the mandatory inliner. We will specialize the
  // relevant callsites after we inline.
  if (F.isTransparent())
    return;

  // Scan all of the instructions in this function in search of ApplyInsts.
  for (auto &BB : F)
    for (auto &I : BB) {
      ApplyInst *AI = dyn_cast<ApplyInst>(&I);

      if (!AI || !AI->hasSubstitutions())
        continue;

      SILValue CalleeVal = AI->getCallee();
      FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);

      if (!FRI)
        continue;

      SILFunction *Callee = FRI->getReferencedFunction();
      if (Callee->isExternalDeclaration())
        if (!M->linkFunction(Callee, SILModule::LinkingMode::LinkAll))
          continue;

      // Save the ApplyInst into the function/bucket that it calls.
      ApplyInstMap[Callee].push_back(AI);
    }
}

static bool hasSameSubstitutions(ApplyInst *A, ApplyInst *B) {
  if (A == B)
    return true;

  ArrayRef<swift::Substitution> SubsA = A->getSubstitutions();
  ArrayRef<swift::Substitution> SubsB = B->getSubstitutions();
  if (SubsA.size() != SubsB.size())
    return false;

  for (int i = 0, e = SubsA.size(); i != e; ++i)
    if (SubsA[i] != SubsB[i])
      return false;

  return true;
}

void dumpTypeSubstitutionMap(const TypeSubstitutionMap &map) {
  llvm::errs() << "{\n";
  for (auto &kv : map) {
    llvm::errs() << "  ";
    kv.first->print(llvm::errs());
    llvm::errs() << " => ";
    kv.second->print(llvm::errs());
    llvm::errs() << "\n";
  }
  llvm::errs() << "}\n";
}

bool
GenericSpecializer::specializeApplyInstGroup(SILFunction *F, AIList &List) {
  bool Changed = false;
  // Make sure we can specialize this function.
  if (!canSpecializeFunction(F))
    return false;

  DEBUG(llvm::dbgs() << "*** Processing: " << F->getName() << "\n");

  SmallVector<AIList, 4> Buckets;

  // Sort the incoming ApplyInst instructions into multiple buckets of AI with
  // exactly the same substitution lists.
  for (auto &AI : List) {
    bool Placed = false;

    DEBUG(llvm::dbgs() << "Function: " << AI->getFunction()->getName() <<
          "; ApplyInst: " << *AI);

    // Scan the existing buckets and search for a bucket of the right type.
    for (int i = 0, e = Buckets.size(); i < e; ++i) {
      assert(Buckets[i].size() && "Found an empty bucket!");
      if (hasSameSubstitutions(Buckets[i][0], AI)) {
        Buckets[i].push_back(AI);
        Placed = true;
        break;
      }
    }

    // Continue if the AI is placed in a bucket.
    if (Placed)
      continue;

    // Create a new bucket and place the AI.
    Buckets.push_back(AIList());
    Buckets[Buckets.size() - 1].push_back(AI);
  }

  // For each bucket of AI instructions of the same type.
  for (auto &Bucket : Buckets) {
    assert(Bucket.size() && "Empty bucket!");

    DEBUG(llvm::dbgs() << "    Bucket: \n");
    DEBUG(for (auto *AI : Bucket) {
      llvm::dbgs() << "        ApplyInst: " << *AI;
    });

    // Create the substitution maps.
    TypeSubstitutionMap InterfaceSubs
      = F->getLoweredFunctionType()->getGenericSignature()
         ->getSubstitutionMap(Bucket[0]->getSubstitutions());

    TypeSubstitutionMap ContextSubs
      = F->getContextGenericParams()
         ->getSubstitutionMap(Bucket[0]->getSubstitutions());

    // We do not support partial specialization.
    if (hasUnboundGenericTypes(InterfaceSubs)) {
      DEBUG(llvm::dbgs() << "    Can not specialize with interface subs.\n");
      continue;
    }

    llvm::SmallString<64> ClonedName;
    {
      llvm::raw_svector_ostream buffer(ClonedName);
      ArrayRef<Substitution> Subs = Bucket[0]->getSubstitutions();
      Mangle::Mangler M(buffer);
      Mangle::GenericSpecializationMangler Mangler(M, F, Subs);
      Mangler.mangle();
    }

    SILFunction *NewF;
    bool createdFunction;
    // If we already have this specialization, reuse it.
    if (auto PrevF = M->lookUpFunction(ClonedName)) {
      NewF = PrevF;
      createdFunction = false;

#ifndef NDEBUG
      // Make sure that NewF's subst type matches the expected type.
      auto Subs = Bucket[0]->getSubstitutions();
      auto FTy =
        F->getLoweredFunctionType()->substGenericArgs(*M,
                                                      M->getSwiftModule(),
                                                      Subs);
      assert(FTy == NewF->getLoweredFunctionType() &&
             "Previously specialized function does not match expected type.");
#endif
    } else {
      // Create a new function.
      NewF = SpecializingCloner::cloneFunction(F, InterfaceSubs, ContextSubs,
                                               ClonedName, Bucket[0]);
      createdFunction = true;
    }

    // Replace all of the AI functions with the new function.
    for (auto &AI : Bucket)
      replaceWithSpecializedFunction(AI, NewF);
    Changed = true;

    // Analyze the ApplyInsts in the new function.
    if (createdFunction) {
      collectApplyInst(*NewF);
      Worklist.push_back(NewF);
    }
  }

  return Changed;
}

namespace {
class SILGenericSpecializerTransform : public SILModuleTransform {
public:
  SILGenericSpecializerTransform() {}

  void run() override {
    if (!EnableSpecializer)
      return;

    CallGraphAnalysis* CGA = PM->getAnalysis<CallGraphAnalysis>();

    // Collect a call-graph bottom-up list of functions and specialize the
    // functions in reverse order.
    auto &CG = CGA->getCallGraph();
    bool Changed = GenericSpecializer(getModule()).
      specialize(CG.getBottomUpFunctionOrder());

    if (Changed) {
      // Schedule another iteration of the transformation pipe.
      PM->scheduleAnotherIteration();

      // Invalidate the call graph.
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
    }
  }

  StringRef getName() override { return "Generic Specialization"; }
};
} // end anonymous namespace


SILTransform *swift::createGenericSpecializer() {
  return new SILGenericSpecializerTransform();
}
