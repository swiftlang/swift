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

  ProtocolConformance *remapConformance(SILType Ty, ProtocolConformance *C) {
    // If Ty does not have unbound generic types, we did not specialize it so
    // just return C. This relies on the fact that we do not partially
    // specialize.
    if (!hasUnboundGenericTypes(Ty.getSwiftRValueType()->getCanonicalType()))
      return C;

    // Otherwise we need to create a specialized conformance for C. Remap the
    // type...
    CanType Type = remapType(Ty).getSwiftRValueType()->getCanonicalType();

    // And create the new specialized conformance.
    ASTContext &AST = SwiftMod->getASTContext();
    return AST.getSpecializedConformance(Type, C,
                                         CallerInst->getSubstitutions());
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
   FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);
   if (FRI && FRI->getReferencedFunction() == Inst->getFunction()) {
     FRI = Builder.createFunctionRef(getOpLocation(Inst->getLoc()),
                                     &Builder.getFunction());
     ApplyInst *NAI =
       Builder.createApply(getOpLocation(Inst->getLoc()), FRI, Args,
                           Inst->isTransparent());
     doPostProcess(Inst, NAI);
     return;
   }

   SmallVector<Substitution, 16> TempSubstList;
   for (auto &Sub : Inst->getSubstitutions())
     TempSubstList.push_back(Sub.subst(Inst->getModule().getSwiftModule(),
                                       OrigFunc->getContextGenericParams(),
                                       CallerInst->getSubstitutions()));

   ApplyInst *N = Builder.createApply(
          getOpLocation(Inst->getLoc()), getOpValue(CalleeVal),
          getOpType(Inst->getSubstCalleeSILType()), getOpType(Inst->getType()),
          TempSubstList, Args, Inst->isTransparent());
   doPostProcess(Inst, N);
 }

 void visitPartialApplyInst(PartialApplyInst *Inst) {
   auto Args = getOpValueArray<8>(Inst->getArguments());

   // Handle recursions by replacing the apply to the callee with an apply to
   // the newly specialized function.
   SILValue CalleeVal = Inst->getCallee();
   FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);
   if (FRI && FRI->getReferencedFunction() == Inst->getFunction()) {
     FRI = Builder.createFunctionRef(getOpLocation(Inst->getLoc()),
                                     &Builder.getFunction());
     PartialApplyInst *NPAI =
       Builder.createPartialApply(getOpLocation(Inst->getLoc()), FRI,
                                  getOpType(Inst->getSubstCalleeSILType()),
                                  ArrayRef<Substitution>(),
                                  Args,
                                  getOpType(Inst->getType()));
     doPostProcess(Inst, NPAI);
     return;
   }

   SmallVector<Substitution, 16> TempSubstList;
   for (auto &Sub : Inst->getSubstitutions())
     TempSubstList.push_back(Sub.subst(Inst->getModule().getSwiftModule(),
                                       OrigFunc->getContextGenericParams(),
                                       CallerInst->getSubstitutions()));

   PartialApplyInst *N = Builder.createPartialApply(
       getOpLocation(Inst->getLoc()), getOpValue(CalleeVal),
       getOpType(Inst->getSubstCalleeSILType()), TempSubstList, Args,
       getOpType(Inst->getType()));
   doPostProcess(Inst, N);
 }


  void visitWitnessMethodInst(WitnessMethodInst *Inst) {
    DEBUG(llvm::dbgs()<<"Specializing : " << *Inst << "\n");

    // Specialize the Self substitution of the witness_method.
    auto sub =
      Inst->getSelfSubstitution().subst(Inst->getModule().getSwiftModule(),
                                        OrigFunc->getContextGenericParams(),
                                        CallerInst->getSubstitutions());

    assert(sub.Conformance.size() == 1 &&
           "didn't get conformance from substitution?!");

    // If we don't have a witness table for this conformance, create a witness
    // table declaration for it.
    SILModule &OtherMod = getCloned()->getModule();
    if (!OtherMod.lookUpWitnessTable(sub.Conformance[0]).first)
      OtherMod.createWitnessTableDeclaration(sub.Conformance[0]);

    // We already subst so getOpConformance is not needed.
    doPostProcess(Inst, Builder.createWitnessMethod(
                            getOpLocation(Inst->getLoc()),
                            getOpType(Inst->getLookupType()),
                            sub.Conformance[0], Inst->getMember(),
                            getOpType(Inst->getType()), Inst->isVolatile()));
  }

  // If we are performing one of the following checked casts:
  //
  //   1. Archetype to Archetype.
  //   2. Archetype to Concrete.
  //   3. Concrete to Archetype
  //
  // since we don't do partial specialization, we know both must types must now
  // be concrete. Modify the checked cast appropriately.
  //
  // If the two types are non-classes, we enforce that after specialization they
  // must both either be trivially equal (in which case we just propagate the
  // operand) or trivially different implying we insert a trap + undef propagate
  // of uses.
  //
  // If the two types are classes, we allow for additionally down/upcast
  // relationships. Casting in between two unrelated classes means we insert a
  // runtime trap to match the semantics of the instruction.
  void
  convertArchetypeConcreteCastToConcreteCast(UnconditionalCheckedCastInst *Inst) {
    // Grab both the from and to types.
    SILType OpFromTy = getOpType(Inst->getOperand().getType());
    SILType OpToTy = getOpType(Inst->getType());
    SILLocation Loc = getOpLocation(Inst->getLoc());

    // If the from/to type is the same, just propagate the operand along to all
    // uses.
    if (OpFromTy == OpToTy) {
      auto Pair = std::make_pair(SILValue(Inst),
                                 getOpValue(Inst->getOperand()));
      ValueMap.insert(Pair);
      return;
    }

    // Ok, we know that the types differ. See if we are performing an upcast
    // or downcast.
    ClassDecl *FromDecl = OpFromTy.getClassOrBoundGenericClass();
    ClassDecl *ToDecl = OpToTy.getClassOrBoundGenericClass();

    // If either of the types are not classes, then we are either comparing
    // non-classes which differ implying a runtime trap *or* a class and a
    // non-class which yields also a runtime trap.
    if (!FromDecl || !ToDecl) {
      Builder.createApply(Loc, Builder.createBuiltinTrap(Loc),
                          ArrayRef<SILValue>());
      auto Pair = std::make_pair(SILValue(Inst),
                                 SILValue(SILUndef::get(OpToTy,
                                                        Inst->getModule())));
      ValueMap.insert(Pair);
      return;
    }

    // Ok, we know that we have two classes. If From is a super class of To,
    // insert a checked downcast.
    if (OpFromTy.isSuperclassOf(OpToTy)) {
      SILValue OtherOp = getOpValue(Inst->getOperand());
      auto *UCCI = Builder.createUnconditionalCheckedCast(
          Loc, CheckedCastKind::Downcast, OtherOp, OpToTy);
      doPostProcess(Inst, UCCI);
      return;
    }

    // If ToTy is a super class of FromTy, we are performing an upcast.
    if (OpToTy.isSuperclassOf(OpFromTy)) {
      doPostProcess(Inst, Builder.createUpcast(
                      Loc, getOpValue(Inst->getOperand()), OpToTy));
      return;
    }

    // Ok, we have an invalid cast. Insert a trap so we trap at runtime as
    // the spec for the instruction requires and propagate undef to all
    // uses.
    Builder.createApply(Loc, Builder.createBuiltinTrap(Loc),
                        ArrayRef<SILValue>());
    auto Pair = std::make_pair(SILValue(Inst),
                               SILValue(SILUndef::get(OpToTy,
                                                      Inst->getModule())));
    ValueMap.insert(Pair);
  }

  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *Inst) {
    // Change the check kind depending on how our specialization affected.
    CheckedCastKind Kind = Inst->getCastKind();
    switch (Kind) {
    case CheckedCastKind::Unresolved:
    case CheckedCastKind::Coercion:
      llvm_unreachable("invalid for SIL");

    // These are not affected by specialization.
    case CheckedCastKind::Downcast:
    case CheckedCastKind::ExistentialToConcrete:
    case CheckedCastKind::ConcreteToUnrelatedExistential:
      SILCloner<TypeSubCloner>::visitUnconditionalCheckedCastInst(Inst);
      return;
    case CheckedCastKind::SuperToArchetype: {
      // Just change the type of cast to an unconditional_checked_cast downcast
      SILLocation OpLoc = getOpLocation(Inst->getLoc());
      SILValue OpValue = getOpValue(Inst->getOperand());
      SILType OpType = getOpType(Inst->getType());
      CheckedCastKind OpCastKind = CheckedCastKind::Downcast;
      doPostProcess(Inst,
                    getBuilder().createUnconditionalCheckedCast(OpLoc,
                                                                OpCastKind,
                                                                OpValue,
                                                                OpType));
      return;
    }
    case CheckedCastKind::ArchetypeToArchetype:
    case CheckedCastKind::ArchetypeToConcrete:
    case CheckedCastKind::ConcreteToArchetype:
      convertArchetypeConcreteCastToConcreteCast(Inst);
      return;
    case CheckedCastKind::ExistentialToArchetype: {
      // Just convert to ExistentialToConcrete.
      // Just change the type of cast to an unconditional_checked_cast downcast
      SILLocation OpLoc = getOpLocation(Inst->getLoc());
      SILValue OpValue = getOpValue(Inst->getOperand());
      SILType OpType = getOpType(Inst->getType());
      CheckedCastKind OpCastKind = CheckedCastKind::ExistentialToConcrete;
      doPostProcess(Inst,
                    getBuilder().createUnconditionalCheckedCast(OpLoc,
                                                                OpCastKind,
                                                                OpValue,
                                                                OpType));
      return;
    }
    }
  }

  // If we are performing an archetype to archetype cast, since we don't do
  // partial specialization, we know both must types must now be
  // concrete. Modify the checked cast appropriately.
  //
  // If the two types are non-classes, we enforce that after specialization they
  // must both either be trivially equal (in which case we just propagate the
  // operand) or trivially different implying we insert a trap + undef propagate
  // of uses.
  //
  //
  // If the two types are classes, we allow for additionally down/upcast
  // relationships. Casting in between two unrelated classes means we insert a
  // runtime trap to match the semantics of the instruction.
  void visitCheckedCastBrArchToArchCast(CheckedCastBranchInst *Inst) {
    // Grab both the from and to types.
    SILType OpFromTy = getOpType(Inst->getOperand().getType());
    SILType OpToTy = getOpType(Inst->getCastType());
    SILLocation OpLoc = getOpLocation(Inst->getLoc());
    SILBasicBlock *OpSuccBB = getOpBasicBlock(Inst->getSuccessBB());
    SILBasicBlock *OpFailBB = getOpBasicBlock(Inst->getFailureBB());

    // If the from/to type is the same, create a branch to the success basic
    // block with the relevant argument.
    if (OpFromTy == OpToTy) {
      auto Args = ArrayRef<SILValue>(getOpValue(Inst->getOperand()));
      auto *Br = Builder.createBranch(OpLoc, OpSuccBB, Args);
      doPostProcess(Inst, Br);
      return;
    }

    // Ok, we know that the types differ. See if we are performing an upcast
    // or downcast.
    ClassDecl *FromDecl = OpFromTy.getClassOrBoundGenericClass();
    ClassDecl *ToDecl = OpToTy.getClassOrBoundGenericClass();

    // If either of the types are not classes, then we are either comparing
    // non-classes which differ *or* a class and a non-class which implies
    // branch to the failure BB.
    if (!FromDecl || !ToDecl) {
      doPostProcess(Inst, Builder.createBranch(OpLoc, OpFailBB));
      return;
    }

    // Ok, we know that we have two classes. If From is a super class of To,
    // insert a downcast...
    if (OpFromTy.isSuperclassOf(OpToTy)) {
      SILValue OtherOp = getOpValue(Inst->getOperand());

      // FIXME: When we have a unchecked downcast, replace the unconditional
      // checked cast here with that.
      auto *Downcast = Builder.createUnconditionalCheckedCast(
          OpLoc, CheckedCastKind::Downcast, OtherOp, OpToTy);
      doPostProcess(Inst, Builder.createBranch(OpLoc, OpSuccBB,
                                               ArrayRef<SILValue>(Downcast)));
      return;
    }

    // If ToTy is a super class of FromTy, we are performing an upcast.
    if (OpToTy.isSuperclassOf(OpFromTy)) {
      UpcastInst *Upcast =
          Builder.createUpcast(OpLoc, getOpValue(Inst->getOperand()), OpToTy);
      doPostProcess(Inst, Builder.createBranch(OpLoc, OpSuccBB,
                                               ArrayRef<SILValue>(Upcast)));
      return;
    }

    // Ok, we have an invalid cast. Jump to fail BB.
    doPostProcess(Inst, Builder.createBranch(OpLoc, OpFailBB));
  }

  void visitCheckedCastBranchInst(CheckedCastBranchInst *Inst) {
    // Change the check kind depending on how our specialization affected.
    CheckedCastKind Kind = Inst->getCastKind();
    switch (Kind) {
    case CheckedCastKind::Unresolved:
    case CheckedCastKind::Coercion:
      llvm_unreachable("invalid for SIL");

    // These are not affected by specialization.
    case CheckedCastKind::Downcast:
    case CheckedCastKind::ExistentialToConcrete:
    case CheckedCastKind::ConcreteToUnrelatedExistential:
      SILCloner<TypeSubCloner>::visitCheckedCastBranchInst(Inst);
      return;
    case CheckedCastKind::SuperToArchetype:
      // Stub implementation.
      SILCloner<TypeSubCloner>::visitCheckedCastBranchInst(Inst);
      return;
    case CheckedCastKind::ArchetypeToArchetype:
      visitCheckedCastBrArchToArchCast(Inst);
      return;
    case CheckedCastKind::ArchetypeToConcrete:
      // Stub implementation.
      SILCloner<TypeSubCloner>::visitCheckedCastBranchInst(Inst);
      return;
    case CheckedCastKind::ExistentialToArchetype:
      // Stub implementation.
      SILCloner<TypeSubCloner>::visitCheckedCastBranchInst(Inst);
      return;
    case CheckedCastKind::ConcreteToArchetype:
      // Stub implementation.
      SILCloner<TypeSubCloner>::visitCheckedCastBranchInst(Inst);
      return;
    }
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

    assert((Orig->isTransparent() || Orig->isBare() || Orig->getLocation())
           && "SILFunction missing location");
    assert((Orig->isTransparent() || Orig->isBare() || Orig->getDebugScope())
           && "SILFunction missing DebugScope");
    assert(!Orig->isGlobalInit() && "Global initializer cannot be cloned");

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

/// Check if we can clone and remap types this function.
static bool canSpecializeFunction(SILFunction *F) {
  return !F->isExternalDeclaration();
}

/// \brief return true if we can specialize the function type with a specific
/// substitution list without doing partial specialization.
static bool canSpecializeFunctionWithSubList(SILFunction *F,
                                             TypeSubstitutionMap &SubsMap) {
  CanSILFunctionType N =
      SILType::substFuncType(F->getModule(), F->getModule().getSwiftModule(),
                             SubsMap, F->getLoweredFunctionType(),
                                                /*dropGenerics = */ true);
  return !hasUnboundGenericTypes(N);
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

    DEBUG(llvm::dbgs() << "Function: " << AI->getFunction()->getName() << "; ApplyInst: " << *AI);

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

    if (!canSpecializeFunctionWithSubList(F, InterfaceSubs)) {
      DEBUG(llvm::dbgs() << "    Can not specialize with interface subs.\n");
      continue;
    }

    llvm::SmallString<64> ClonedName;
    {
      llvm::raw_svector_ostream buffer(ClonedName);
      buffer << "_TTS";

      Mangle::Mangler mangle(buffer);

      for (auto &Sub : Bucket[0]->getSubstitutions()) {
        DEBUG(llvm::dbgs() << "  Replacement Type: "; Sub.Replacement->getCanonicalType().dump());
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
} // end anonymous namespace


SILTransform *swift::createGenericSpecializer() {
  return new SILGenericSpecializerTransform();
}
