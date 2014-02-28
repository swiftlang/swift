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
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Mangle.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumSpecialized, "Number of functions specialized");
STATISTIC(NumCMSpecialized, "Number of ClassMethodInst specialized");

namespace {

/// TypeSubCloner - a utility class for cloning and remapping types.
class TypeSubCloner : public SILCloner<TypeSubCloner> {
  friend class SILVisitor<TypeSubCloner>;
  friend class SILCloner<TypeSubCloner>;

public:
  /// Clone and remap the types in \p F according to the substitution
  /// list in \p Subs.
  static SILFunction *cloneFunction(SILFunction *F,
                                    TypeSubstitutionMap &InterfaceSubs,
                                    TypeSubstitutionMap &ContextSubs,
                                    StringRef NewName, ApplyInst *Caller) {
    // Clone and specialize the function.
    TypeSubCloner TSC(F, InterfaceSubs, ContextSubs, NewName, Caller);
    TSC.populateCloned();
    return TSC.getCloned();
  }

private:
  TypeSubCloner(SILFunction *F,
                TypeSubstitutionMap &InterfaceSubs,
                TypeSubstitutionMap &ContextSubs,
                StringRef NewName,
                ApplyInst *Caller)
      : SILCloner(*initCloned(F, InterfaceSubs, NewName)),
        SwiftMod(F->getModule().getSwiftModule()),
        SubsMap(ContextSubs),
        OrigFunc(F),
        CallerInst(Caller) { }

  /// Clone the body of the function into the empty function that was created
  /// by initCloned.
  void populateCloned();

  SILType remapType(SILType Ty) {
    return SILType::substType(OrigFunc->getModule(), SwiftMod, SubsMap, Ty);
  }

  void visitClassMethodInst(ClassMethodInst *Inst) {
    NumCMSpecialized++;
    doPostProcess(Inst,
                  Builder.createClassMethod(getOpLocation(Inst->getLoc()),
                                            getOpValue(Inst->getOperand()),
                                            Inst->getMember(),
                                            // No need to translate the return
                                            // type because this is the type of
                                            // the fetched method.
                                            Inst->getType(),
                                            Inst->isVolatile()));
  }

 void visitApplyInst(ApplyInst *Inst) {
   auto Args = getOpValueArray<8>(Inst->getArguments());

   // Handle recursions by replacing the apply to the callee with an apply to
   // the newly specialized function.
   SILValue CalleeVal = Inst->getCallee();
   FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal.getDef());
   if (FRI && FRI->getReferencedFunction() == Inst->getFunction()) {
     FRI = Builder.createFunctionRef(Inst->getLoc(),
                                     &Builder.getFunction());
     ApplyInst *NAI =
     Builder.createApply(Inst->getLoc(), FRI, Args, Inst->isTransparent());
     doPostProcess(Inst, NAI);
     return;
   }

   SmallVector<Substitution, 16> TempSubstList;
   for (auto &Sub : Inst->getSubstitutions())
     TempSubstList.push_back(Sub.subst(Inst->getModule().getSwiftModule(),
                                       CallerInst->getSubstitutions()));

   ApplyInst *N = Builder.createApply(
          getOpLocation(Inst->getLoc()), getOpValue(CalleeVal),
          getOpType(Inst->getSubstCalleeSILType()), getOpType(Inst->getType()),
          TempSubstList, Args, Inst->isTransparent());
   doPostProcess(Inst, N);
 }

  void visitArchetypeMethodInst(ArchetypeMethodInst *Inst) {
    DEBUG(llvm::dbgs()<<"Specializing : " << *Inst << "\n");

    // Specialize the Self substitution of the witness_method.
    auto sub =
    Inst->getSelfSubstitution().subst(Inst->getModule().getSwiftModule(),
                                      CallerInst->getSubstitutions());

    assert(sub.Conformance.size() == 1 &&
           "didn't get conformance from substitution?!");

    doPostProcess(Inst,Builder.
                  createArchetypeMethod(getOpLocation(Inst->getLoc()),
                                        getOpType(Inst->getLookupType()),
                                        getOpConformance(Inst->getLookupType(),
                                                         sub.Conformance[0]),
                                        Inst->getMember(),
                                        getOpType(Inst->getType()),
                                        Inst->isVolatile()));
  }

  static SILLinkage getSpecializedLinkage(SILLinkage orig) {
    switch (orig) {
    case SILLinkage::Public:
    case SILLinkage::PublicExternal:
    case SILLinkage::Shared:
    case SILLinkage::Hidden:
    case SILLinkage::HiddenExternal:
      // Specializations of public or hidden symbols can be shared by all TUs
      // that specialize the definition.
      return SILLinkage::Shared;
        
    case SILLinkage::Private:
      // Specializations of private symbols should remain so.
      return SILLinkage::Private;
    }
  }
  
  /// Create a new empty function with the correct arguments and a unique name.
  static SILFunction *initCloned(SILFunction *Orig,
                                 TypeSubstitutionMap &InterfaceSubs,
                                 StringRef NewName) {
    SILModule &M = Orig->getModule();
    Module *SM = M.getSwiftModule();

    CanSILFunctionType FTy =
        SILType::substFuncType(M, SM, InterfaceSubs,
                               Orig->getLoweredFunctionType(),
                               /*dropGenerics = */ true);

    // Create a new empty function.
    SILFunction *NewF =
        SILFunction::create(M, getSpecializedLinkage(Orig->getLinkage()),
                            NewName, FTy, nullptr,
                            Orig->getLocation(), Orig->isBare(),
                            Orig->isTransparent(), 0,
                            Orig->getDebugScope(), Orig->getDeclContext());

    NumSpecialized++;
    return NewF;
  }

  SILFunction *getCloned() { return &getBuilder().getFunction(); }

  /// The Swift module that the cloned function belongs to.
  Module *SwiftMod;
  /// The substitutions list for the specialization.
  TypeSubstitutionMap &SubsMap;
  /// The original function to specialize.
  SILFunction *OrigFunc;
  /// The ApplyInst that is the caller to the cloned function.
  ApplyInst *CallerInst;
};

} // end anonymous namespace.

void TypeSubCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block.
  SILBasicBlock *OrigEntryBB = OrigFunc->begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);

  // Create the entry basic block with the function arguments.
  auto I = OrigEntryBB->bbarg_begin(), E = OrigEntryBB->bbarg_end();
  while (I != E) {
    SILValue MappedValue =
      new (M) SILArgument(remapType((*I)->getType()), ClonedEntryBB,
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

/// \brief Return true if we can specialize this type.
static bool isGenericType(Type F) {
  return F.findIf([](Type type) ->bool {
                    return isa<ArchetypeType>(type.getPointer());
                  });
}

/// Check if we can clone and remap types this function.
static bool canSpecializeFunction(SILFunction *F) {
  if (F->isExternalDeclaration())
    return false;

  for (auto &BB : *F)
    for (auto &I : BB) {
      // We don't specialize the PartialApply instructions.
      if (PartialApplyInst *PAI = dyn_cast<PartialApplyInst>(&I)) {
        if (PAI->hasSubstitutions())
          return false;
      }
    }

  return true;
}

/// \brief return true if we can specialize the function type with a specific
/// substitution list without doing partial specialization.
static bool canSpecializeFunctionWithSubList(SILFunction *F,
                                             TypeSubstitutionMap &SubsMap) {
  CanSILFunctionType N =
      SILType::substFuncType(F->getModule(), F->getModule().getSwiftModule(),
                             SubsMap, F->getLoweredFunctionType(),
                                                /*dropGenerics = */ true);
  return !isGenericType(N);
}

namespace {

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
  // Scan all of the instructions in this function in search of ApplyInsts.
  for (auto &BB : F)
    for (auto &I : BB) {
      ApplyInst *AI = dyn_cast<ApplyInst>(&I);

      if (!AI || !AI->hasSubstitutions())
        continue;

      SILValue CalleeVal = AI->getCallee();
      FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal.getDef());

      if (!FRI)
        continue;

      SILFunction *Callee = FRI->getReferencedFunction();
      if (Callee->isExternalDeclaration())
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

  SmallVector<AIList, 4> Buckets;

  // Sort the incoming ApplyInst instructions into multiple buckets of AI with
  // exactly the same substitution lists.
  for (auto &AI : List) {
    bool Placed = false;

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

    // Create the substitution maps.
    TypeSubstitutionMap InterfaceSubs
      = F->getLoweredFunctionType()->getGenericSignature()
         ->getSubstitutionMap(Bucket[0]->getSubstitutions());
    
    TypeSubstitutionMap ContextSubs
      = F->getContextGenericParams()
         ->getSubstitutionMap(Bucket[0]->getSubstitutions());

    if (!canSpecializeFunctionWithSubList(F, InterfaceSubs))
      continue;

    llvm::SmallString<64> ClonedName;
    {
      llvm::raw_svector_ostream buffer(ClonedName);
      buffer << "_TTS";

      Mangle::Mangler mangle(buffer);
      
      for (auto &Sub : Bucket[0]->getSubstitutions()) {
        mangle.mangleType(Sub.Replacement->getCanonicalType(),
                          ResilienceExpansion::Minimal, 0);
        for (auto C : Sub.Conformance) {
          if (!C)
            goto null_conformances;
          mangle.mangleProtocolConformance(C);
        }
      null_conformances:;
        buffer << '_';
      }
      
      buffer << '_' << F->getName();
    }
    
    SILFunction *NewF;
    bool createdFunction;
    // If we already have this specialization, reuse it.
    if (auto PrevF = M->lookUpFunction(ClonedName)) {
      NewF = PrevF;
      createdFunction = false;
    } else {
      // Create a new function.
      NewF = TypeSubCloner::cloneFunction(F, InterfaceSubs, ContextSubs,
                                          ClonedName, Bucket[0]);
      createdFunction = false;
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

class SILGenericSpecializerTransform : public SILModuleTransform {

public:
  SILGenericSpecializerTransform() {}

  void run() {
    CallGraphAnalysis* CGA = PM->getAnalysis<CallGraphAnalysis>();

    // Collect a call-graph bottom-up list of functions and specialize the
    // functions in reverse order.
    bool Changed = GenericSpecializer(getModule()).
      specialize(CGA->bottomUpCallGraphOrder());

    if (Changed) {
      // Schedule another iteration of the transformation pipe.
      PM->scheduleAnotherIteration();

      // Invalidate the call graph.
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallGraph);
    }
  }

  StringRef getName() override { return "Generic Specialization"; }
};

SILTransform *swift::createGenericSpecializer() {
  return new SILGenericSpecializerTransform();
}
