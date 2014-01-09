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
  static SILFunction *cloneFunction(SILFunction *F, TypeSubstitutionMap &Subs,
                                    StringRef NewName, ApplyInst *Caller) {
    // Clone and specialize the function.
    TypeSubCloner TSC(F, Subs, NewName, Caller);
    TSC.populateCloned();
    return TSC.getCloned();
  }

private:
  TypeSubCloner(SILFunction *F, TypeSubstitutionMap &Subst, StringRef NewName,
                ApplyInst *Caller) : SILCloner(*initCloned(F, Subst, NewName)),
        SwiftMod(F->getModule().getSwiftModule()), SubsMap(Subst), OrigFunc(F),
  CallerInst(Caller) { }

  /// Clone the body of the function into the empty function that was created
  /// by initCloned.
  void populateCloned();

  SILType remapType(SILType Ty) {
    return SILType::substType(OrigFunc->getModule(), SwiftMod, SubsMap, Ty);
  }

  /// Finds the generic type substitution in the caller substitution list that
  /// corresponds to a specific generic type.
  Substitution getCallerSubstitutionForCalleSub(Type T) {

    for (Substitution &CallerSub : CallerInst->getSubstitutions()) {
      // Find the correct type replacement in the caller ApplyInst
      if (CallerSub.Archetype->getCanonicalType() == T->getCanonicalType())
        return CallerSub;
    }

    // T is not a caller Archetype.
    return  Substitution{ 0, 0, ArrayRef<ProtocolConformance *>() };
  }

  Substitution remapSubstitution(Substitution sub) {
    // Find the substitution of type T, in the caller subst list.
    Substitution S = getCallerSubstitutionForCalleSub(sub.Replacement);

    // If the replacement type is not a caller Archetype then use the existing
    // conformance list. 
    if (!S.Archetype)
      S = sub;

    return Substitution{ sub.Archetype,
       sub.Replacement.subst(SwiftMod, SubsMap, true, 0),
      S.Conformance };
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

    SmallVector<Substitution, 16> TempSubstList;
    for (auto &Sub : Inst->getSubstitutions())
      TempSubstList.push_back(remapSubstitution(Sub));

    ApplyInst *N = Builder.createApply(
        getOpLocation(Inst->getLoc()), getOpValue(Inst->getCallee()),
        getOpType(Inst->getSubstCalleeSILType()), getOpType(Inst->getType()),
        TempSubstList, Args, Inst->isTransparent());
   doPostProcess(Inst, N);
  }

  /// Create a new empty function with the correct arguments and a unique name.
  static SILFunction *initCloned(SILFunction *Orig,
                                 TypeSubstitutionMap &Subst,
                                 StringRef NewName) {
    SILModule &M = Orig->getModule();
    Module *SM = M.getSwiftModule();

    CanSILFunctionType FTy =
        SILType::substFuncType(M, SM, Subst, Orig->getLoweredFunctionType(),
                                                /*dropGenerics = */ true);

    // Create a new empty function.
    SILFunction *NewF =
        SILFunction::create(M, SILLinkage::Private, NewName, FTy,
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

/// \brief Return true if we can specialize this Apply Instruction.
static bool canSpecializeApplyInst(ApplyInst *AI) {
  for (auto &Sub : AI->getSubstitutions())
    if (isGenericType(Sub.Replacement))
      return false;

  return true;
}

/// Check if we can clone and remap types this function.
static bool canSpecializeFunction(SILFunction *F) {
  if (F->isExternalDeclaration())
    return false;

  for (auto &BB : *F)
    for (auto &I : BB) {
      // We don't specialize ArchetypeMethod and PartialApply instructions.
      if (isa<ArchetypeMethodInst>(&I) || isa<PartialApplyInst>(&I))
        return false;

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

struct SILSpecializer {
  /// A list of ApplyInst instructions.
  typedef SmallVector<ApplyInst *, 16> AIList;

  /// The SIL Module.
  SILModule *M;

  /// A list of declared function names.
  llvm::StringSet<> FunctionNameCache;

  /// Maps a function to all of the ApplyInst that call it.
  llvm::MapVector<SILFunction *, AIList> ApplyInstMap;

  /// A worklist of functions to specialize.
  std::vector<SILFunction*> Worklist;

  SILSpecializer(SILModule *Mod) : M(Mod) {

    // Save the list of function names at the beginning of the specialization.
    for (SILFunction &F : *Mod)
      FunctionNameCache.insert(F.getName());
  }

  bool specializeApplyInstGroup(SILFunction *F, AIList &List);

  /// Scan the function and collect all of the ApplyInst with generic
  /// substitutions into buckets according to the called function.
  void collectApplyInst(SILFunction &F);

  /// The driver for the generic specialization pass.
  bool specialize() {
    bool Changed = false;
    for (auto &F : *M)
      collectApplyInst(F);

    // Collect a call-graph bottom-up list of functions. We specialize
    // the functions in a top-down order, starting from the end of the list.
    bottomUpCallGraphOrder(M, Worklist);

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

void SILSpecializer::collectApplyInst(SILFunction &F) {
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

      // Check if we can specialize this AI. We perform this check last
      // because it can be more expensive.
      if (!canSpecializeApplyInst(AI))
        continue;

      // Save the ApplyInst into the function/bucket that it calls.
      ApplyInstMap[Callee].push_back(AI);
    }
}

static void replaceWithSpecializedFunction(ApplyInst *AI, SILFunction *NewF) {
  SILLocation Loc = AI->getLoc();
  ArrayRef<Substitution> Subst;

  SmallVector<SILValue, 4> Arguments;
  for (auto &Op : AI->getArgumentOperands()) {
    Arguments.push_back(Op.get());
  }

  SILBuilder Builder(AI);
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  ApplyInst *NAI =
      Builder.createApply(Loc, FRI, Arguments, AI->isTransparent());
  SILValue(AI, 0).replaceAllUsesWith(SILValue(NAI, 0));
  recursivelyDeleteTriviallyDeadInstructions(AI, true);
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

bool SILSpecializer::specializeApplyInstGroup(SILFunction *F, AIList &List) {
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

    assert(canSpecializeApplyInst(AI) && "Can't specialize this ApplyInst");

    // Create a new bucket and place the AI.
    Buckets.push_back(AIList());
    Buckets[Buckets.size() - 1].push_back(AI);
  }

  // For each bucket of AI instructions of the same type.
  for (auto &Bucket : Buckets) {
    assert(Bucket.size() && "Empty bucket!");
    TypeSubstitutionMap Subs;

    // Create the substitution map.
    for (auto &Sub : Bucket[0]->getSubstitutions())
      Subs[Sub.Archetype] = Sub.Replacement;

    if (!canSpecializeFunctionWithSubList(F, Subs))
      continue;

    // TODO: Mangle the subst types into the new function name and
    // use shared linkage. For now, we use a running counter. rdar://15658321
    unsigned Counter = 0;
    std::string ClonedName;
    do {
      ClonedName.clear();
      llvm::raw_string_ostream buffer(ClonedName);
      buffer << F->getName() << "_spec" << Counter++;
    } while (FunctionNameCache.count(ClonedName));

    // Add the new name to the list of module function names.
    FunctionNameCache.insert(ClonedName);

    // Create a new function.
    SILFunction *NewF = TypeSubCloner::cloneFunction(F, Subs, ClonedName,
                                                     Bucket[0]);

    // Replace all of the AI functions with the new function.
    for (auto &AI : Bucket)
      replaceWithSpecializedFunction(AI, NewF);
    Changed = true;

    // Analyze the ApplyInsts in the new function.
    collectApplyInst(*NewF);
    Worklist.push_back(NewF);
  }

  if (!F->getRefCount() && !isPossiblyUsedExternally(F->getLinkage())) {
    F->getBlocks().clear();
  }

  return Changed;
}

bool swift::performSILSpecialization(SILModule *M) {
  return SILSpecializer(M).specialize();
}
