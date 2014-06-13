//===--- TypeSubstCloner.h - Clones code and substitutes types ---*- C++ -*-==//
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
//
// This file defines TypeSubstCloner, which derives from SILCloner and
// has support for type substitution while cloning code that uses generics.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_TYPESUBSTCLONER_H
#define SWIFT_SIL_TYPESUBSTCLONER_H

#include "swift/AST/Type.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// TypeSubstCloner - a utility class for cloning code while remapping types.
template<typename ImplClass>
class TypeSubstCloner : public SILCloner<ImplClass> {
  friend class SILVisitor<ImplClass>;
  friend class SILCloner<ImplClass>;

public:
  using SILCloner<ImplClass>::asImpl;
  using SILCloner<ImplClass>::getBuilder;
  using SILCloner<ImplClass>::getOpLocation;
  using SILCloner<ImplClass>::getOpValue;
  using SILCloner<ImplClass>::getOpType;
  using SILCloner<ImplClass>::getOpBasicBlock;
  using SILCloner<ImplClass>::doPostProcess;
  using SILCloner<ImplClass>::ValueMap;

  TypeSubstCloner(SILFunction &To,
                  SILFunction &From,
                  TypeSubstitutionMap &ContextSubs,
                  ApplyInst *Caller)
    : SILCloner<ImplClass>(To),
      SwiftMod(From.getModule().getSwiftModule()),
      SubsMap(ContextSubs),
      Original(From),
      TheApply(Caller) { }

protected:
  SILType remapType(SILType Ty) {
    return SILType::substType(Original.getModule(), SwiftMod, SubsMap, Ty);
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
                                         TheApply->getSubstitutions());
  }

  void visitClassMethodInst(ClassMethodInst *Inst) {
    doPostProcess(Inst,
                  getBuilder().createClassMethod(getOpLocation(Inst->getLoc()),
                                                 getOpValue(Inst->getOperand()),
                                                 Inst->getMember(),
                                                 // No need to
                                                 // translate the
                                                 // return type
                                                 // because this is
                                                 // the type of the
                                                 // fetched method.
                                                 Inst->getType(),
                                                 Inst->isVolatile()));
  }

 void visitApplyInst(ApplyInst *Inst) {
   auto Args = this->template getOpValueArray<8>(Inst->getArguments());

   // Handle recursions by replacing the apply to the callee with an apply to
   // the newly specialized function.
   SILValue CalleeVal = Inst->getCallee();
   FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);
   SILBuilder &Builder = getBuilder();
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
                                       Original.getContextGenericParams(),
                                       TheApply->getSubstitutions()));

   ApplyInst *N = Builder.createApply(
          getOpLocation(Inst->getLoc()), getOpValue(CalleeVal),
          getOpType(Inst->getSubstCalleeSILType()), getOpType(Inst->getType()),
          TempSubstList, Args, Inst->isTransparent());
   doPostProcess(Inst, N);
 }

 void visitPartialApplyInst(PartialApplyInst *Inst) {
   auto Args = this->template getOpValueArray<8>(Inst->getArguments());

   // Handle recursions by replacing the apply to the callee with an apply to
   // the newly specialized function.
   SILValue CalleeVal = Inst->getCallee();
   FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);
   SILBuilder &Builder = getBuilder();
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
                                       Original.getContextGenericParams(),
                                       TheApply->getSubstitutions()));

   PartialApplyInst *N = Builder.createPartialApply(
       getOpLocation(Inst->getLoc()), getOpValue(CalleeVal),
       getOpType(Inst->getSubstCalleeSILType()), TempSubstList, Args,
       getOpType(Inst->getType()));
   doPostProcess(Inst, N);
 }


  void visitWitnessMethodInst(WitnessMethodInst *Inst) {
    DEBUG(llvm::dbgs()<<"Specializing : " << *Inst << "\n");

    // Specialize the Self substitution of the witness_method.
    //
    // FIXME: This needs to not only handle Self but all Self derived types so
    // we handle type aliases correctly.
    auto sub =
      Inst->getSelfSubstitution().subst(Inst->getModule().getSwiftModule(),
                                        Original.getContextGenericParams(),
                                        TheApply->getSubstitutions());

    assert(sub.Conformance.size() == 1 &&
           "didn't get conformance from substitution?!");

    // If we don't have a witness table for this conformance, create a witness
    // table declaration for it.
    SILFunction &Cloned = getBuilder().getFunction();
    SILModule &OtherMod = Cloned.getModule();
    if (!OtherMod.lookUpWitnessTable(sub.Conformance[0]).first)
      OtherMod.createWitnessTableDeclaration(sub.Conformance[0]);

    // We already subst so getOpConformance is not needed.
    SILBuilder &Builder = getBuilder();
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
  convertArchetypeConcreteCastToConcreteCast(
                                           UnconditionalCheckedCastInst *Inst) {
    // Grab both the from and to types.
    SILType OpFromTy = getOpType(Inst->getOperand().getType());
    SILType OpToTy = getOpType(Inst->getType());
    SILLocation Loc = getOpLocation(Inst->getLoc());
    SILBuilder &Builder = getBuilder();

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
    case CheckedCastKind::ArrayDowncast:
      llvm_unreachable("invalid for SIL");

    // These are not affected by specialization.
    case CheckedCastKind::Identical:
    case CheckedCastKind::Downcast:
    case CheckedCastKind::ExistentialToConcrete:
    case CheckedCastKind::ConcreteToUnrelatedExistential:
      SILCloner<ImplClass>::visitUnconditionalCheckedCastInst(Inst);
      return;
    case CheckedCastKind::SuperToArchetype: {
      SILValue OpValue = getOpValue(Inst->getOperand());
      SILType OpFromTy = OpValue.getType();
      SILType OpToTy = getOpType(Inst->getType());

      // If the from/to type is the same, just propagate the operand along to
      // all uses.
      if (OpFromTy == OpToTy) {
        auto Pair = std::make_pair(SILValue(Inst),
                                   OpValue);
        ValueMap.insert(Pair);
        return;
      }

      // Otherwise we assume this is an unconditional_checked_cast downcast.
      SILLocation OpLoc = getOpLocation(Inst->getLoc());
      CheckedCastKind OpCastKind = CheckedCastKind::Downcast;
      doPostProcess(Inst,
                    getBuilder().createUnconditionalCheckedCast(OpLoc,
                                                                OpCastKind,
                                                                OpValue,
                                                                OpToTy));
      return;
    }
    case CheckedCastKind::ArchetypeToArchetype:
    case CheckedCastKind::ArchetypeToConcrete:
    case CheckedCastKind::ConcreteToArchetype:
      convertArchetypeConcreteCastToConcreteCast(Inst);
      return;
    case CheckedCastKind::ExistentialToArchetype: {
      SILValue OpValue = getOpValue(Inst->getOperand());
      SILType OpFromTy = OpValue.getType();
      SILType OpToTy = getOpType(Inst->getType());

      // If the from/to type is the same, just propagate the operand along to
      // all uses.
      if (OpFromTy == OpToTy) {
        auto Pair = std::make_pair(SILValue(Inst),
                                   OpValue);
        ValueMap.insert(Pair);
        return;
      }

      // Otherwise we assume that we are performing an ExistentialToConcrete
      // cast.
      SILLocation OpLoc = getOpLocation(Inst->getLoc());
      CheckedCastKind OpCastKind = CheckedCastKind::ExistentialToConcrete;
      doPostProcess(Inst,
                    getBuilder().createUnconditionalCheckedCast(OpLoc,
                                                                OpCastKind,
                                                                OpValue,
                                                                OpToTy));
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
    SILBuilder &Builder = getBuilder();

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
    case CheckedCastKind::ArrayDowncast:
      llvm_unreachable("invalid for SIL");

    // These are not affected by specialization.
    case CheckedCastKind::Identical:
    case CheckedCastKind::Downcast:
    case CheckedCastKind::ExistentialToConcrete:
    case CheckedCastKind::ConcreteToUnrelatedExistential:
      SILCloner<ImplClass>::visitCheckedCastBranchInst(Inst);
      return;
    case CheckedCastKind::SuperToArchetype: {
      // Just change the type of cast to a checked_cast_br downcast
      SILLocation OpLoc = getOpLocation(Inst->getLoc());
      SILValue OpValue = getOpValue(Inst->getOperand());
      SILType OpFromTy = OpValue.getType();
      SILType OpToTy = getOpType(Inst->getCastType());
      SILBasicBlock *OpSuccBB = getOpBasicBlock(Inst->getSuccessBB());
      SILBuilder &Builder = getBuilder();

      // If the from/to type is the same, insert a branch to the success basic
      // block with the relevant argument.
      if (OpFromTy == OpToTy) {
        auto Args = ArrayRef<SILValue>(getOpValue(Inst->getOperand()));
        auto *Br = Builder.createBranch(OpLoc, OpSuccBB, Args);
        doPostProcess(Inst, Br);
        return;
      }

      // Otherwise we assume that we are performing a checked_cast_kind
      // downcast.
      SILBasicBlock *OpFailBB = getOpBasicBlock(Inst->getFailureBB());
      CheckedCastKind OpCastKind = CheckedCastKind::Downcast;
      doPostProcess(Inst,
                    getBuilder().createCheckedCastBranch(OpLoc,
                                                         OpCastKind,
                                                         OpValue,
                                                         OpToTy,
                                                         OpSuccBB,
                                                         OpFailBB));
      return;
    }
    case CheckedCastKind::ArchetypeToArchetype:
    case CheckedCastKind::ArchetypeToConcrete:
    case CheckedCastKind::ConcreteToArchetype:
      visitCheckedCastBrArchToArchCast(Inst);
      return;
    case CheckedCastKind::ExistentialToArchetype: {
      SILLocation OpLoc = getOpLocation(Inst->getLoc());
      SILValue OpValue = getOpValue(Inst->getOperand());
      SILType OpFromTy = OpValue.getType();
      SILType OpToTy = getOpType(Inst->getCastType());
      SILBasicBlock *OpSuccBB = getOpBasicBlock(Inst->getSuccessBB());
      SILBuilder &Builder = getBuilder();

      // If the from/to type is the same, insert a branch to the success basic
      // block with the relevant argument.
      if (OpFromTy == OpToTy) {
        auto Args = ArrayRef<SILValue>(getOpValue(Inst->getOperand()));
        auto *Br = Builder.createBranch(OpLoc, OpSuccBB, Args);
        doPostProcess(Inst, Br);
        return;
      }

      // Otherwise we assume that we are performing an ExistentialToConcrete
      // cast.
      SILBasicBlock *OpFailBB = getOpBasicBlock(Inst->getFailureBB());
      CheckedCastKind OpCastKind = CheckedCastKind::ExistentialToConcrete;
      doPostProcess(Inst,
                    getBuilder().createCheckedCastBranch(OpLoc,
                                                         OpCastKind,
                                                         OpValue,
                                                         OpToTy,
                                                         OpSuccBB,
                                                         OpFailBB));
      return;
    }
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

  /// The Swift module that the cloned function belongs to.
  Module *SwiftMod;
  /// The substitutions list for the specialization.
  TypeSubstitutionMap &SubsMap;
  /// The original function to specialize.
  SILFunction &Original;
  /// The ApplyInst that is the caller to the cloned function.
  ApplyInst *TheApply;
};

} // end namespace swift

#endif
