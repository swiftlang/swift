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
#include "swift/SIL/DynamicCasts.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// TypeSubstCloner - a utility class for cloning code while remapping types.
template<typename ImplClass>
class TypeSubstCloner : public SILClonerWithScopes<ImplClass> {
  friend class SILVisitor<ImplClass>;
  friend class SILCloner<ImplClass>;

  typedef SILClonerWithScopes<ImplClass> super;

public:
  using SILClonerWithScopes<ImplClass>::asImpl;
  using SILClonerWithScopes<ImplClass>::getBuilder;
  using SILClonerWithScopes<ImplClass>::getOpLocation;
  using SILClonerWithScopes<ImplClass>::getOpValue;
  using SILClonerWithScopes<ImplClass>::getASTTypeInClonedContext;
  using SILClonerWithScopes<ImplClass>::getOpASTType;
  using SILClonerWithScopes<ImplClass>::getTypeInClonedContext;
  using SILClonerWithScopes<ImplClass>::getOpType;
  using SILClonerWithScopes<ImplClass>::getOpBasicBlock;
  using SILClonerWithScopes<ImplClass>::doPostProcess;
  using SILClonerWithScopes<ImplClass>::ValueMap;

  TypeSubstCloner(SILFunction &To,
                  SILFunction &From,
                  TypeSubstitutionMap &ContextSubs,
                  ArrayRef<Substitution> ApplySubs,
                  bool Inlining =false)
    : SILClonerWithScopes<ImplClass>(To, Inlining),
      SwiftMod(From.getModule().getSwiftModule()),
      SubsMap(ContextSubs),
      Original(From),
      ApplySubs(ApplySubs),
      Inlining(Inlining) { }

protected:
  SILType remapType(SILType Ty) {
    return SILType::substType(Original.getModule(), SwiftMod, SubsMap, Ty);
  }

  CanType remapASTType(CanType ty) {
    return ty.subst(SwiftMod, SubsMap, false, nullptr)->getCanonicalType();
  }

  Substitution remapSubstitution(Substitution sub) {
    auto newSub = sub.subst(SwiftMod,
                            Original.getContextGenericParams(),
                            ApplySubs);
    // Remap opened archetypes into the cloned context.
    newSub = Substitution(newSub.getArchetype(),
                          getASTTypeInClonedContext(newSub.getReplacement()
                                                      ->getCanonicalType()),
                          newSub.getConformances());
    return newSub;
  }

  ProtocolConformance *remapConformance(ArchetypeType *archetype,
                                        CanType type,
                                        ProtocolConformance *conf) {
    Substitution sub(archetype, type, conf);
    return remapSubstitution(sub).getConformances()[0];
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
  
  void visitBuiltinInst(BuiltinInst *Inst) {
    auto Args = this->template getOpValueArray<8>(Inst->getArguments());

    SmallVector<Substitution, 16> TempSubstList;
    for (auto &Sub : Inst->getSubstitutions()) {
      TempSubstList.push_back(asImpl().getOpSubstitution(Sub));
    }

    SILBuilder &Builder = getBuilder();
    auto N = Builder.createBuiltin(getOpLocation(Inst->getLoc()),
                                   Inst->getName(),
                                   getOpType(Inst->getType()),
                                   TempSubstList, Args);
    doPostProcess(Inst, N);
  }
  
  void visitApplyInst(ApplyInst *Inst) {
    auto Args = this->template getOpValueArray<8>(Inst->getArguments());

    // Handle recursions by replacing the apply to the callee with an apply to
    // the newly specialized function.
    SILBuilder &Builder = getBuilder();
    SILValue CalleeVal = Inst->getCallee();
    if (!Inlining) {
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
    }

    SmallVector<Substitution, 16> TempSubstList;
    for (auto &Sub : Inst->getSubstitutions()) {
      TempSubstList.push_back(asImpl().getOpSubstitution(Sub));
    }

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
    SILBuilderWithPostProcess<TypeSubstCloner, 4> Builder(this, Inst);
    if (!Inlining) {
      FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);
      if (FRI && FRI->getReferencedFunction() == Inst->getFunction()) {
        FRI = Builder.createFunctionRef(getOpLocation(Inst->getLoc()),
                                        &Builder.getFunction());
        Builder.createPartialApply(getOpLocation(Inst->getLoc()), FRI,
                                   getOpType(Inst->getSubstCalleeSILType()),
                                   ArrayRef<Substitution>(),
                                   Args,
                                   getOpType(Inst->getType()));
        return;
      }
    }

    SmallVector<Substitution, 16> TempSubstList;
    for (auto &Sub : Inst->getSubstitutions()) {
      TempSubstList.push_back(asImpl().getOpSubstitution(Sub));
    }
    
    Builder.createPartialApply(
      getOpLocation(Inst->getLoc()), getOpValue(CalleeVal),
        getOpType(Inst->getSubstCalleeSILType()), TempSubstList, Args,
        getOpType(Inst->getType()));
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
                                        ApplySubs);

    assert(sub.getConformances().size() == 1 &&
           "didn't get conformance from substitution?!");

    auto Conformance = sub.getConformances()[0];

    auto newLookupType = getOpASTType(Inst->getLookupType());
    if (Conformance) {
      CanType Ty = Conformance->getType()->getCanonicalType();

      if (Ty != newLookupType) {
        assert(Ty->isSuperclassOf(newLookupType, nullptr) &&
               "Should only create upcasts for sub class.");

        // We use the super class as the new look up type.
        newLookupType = Ty;
      }
    }

    // We already subst so getOpConformance is not needed.
    SILBuilder &Builder = getBuilder();
    doPostProcess(
        Inst,
        Builder.createWitnessMethod(
            getOpLocation(Inst->getLoc()), newLookupType, Conformance,
            Inst->getMember(), getOpType(Inst->getType()),
            Inst->hasOperand() ? getOpValue(Inst->getOperand()) : SILValue(),
            Inst->isVolatile()));
  }

  /// Attempt to simplify an unconditional checked cast.
  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *inst) {
    // Grab both the from and to types.
    SILType sourceType = getOpType(inst->getOperand().getType());
    SILType targetType = getOpType(inst->getType());
    SILLocation loc = getOpLocation(inst->getLoc());
    SILBuilderWithPostProcess<TypeSubstCloner, 16> B(this, inst);

    // The non-addr CheckedCastInsts can currently only be used for
    // types that don't have abstraction differences, so this is okay.
    CanType formalSourceType = sourceType.getSwiftRValueType();
    CanType formalTargetType = targetType.getSwiftRValueType();

    SILValue operand = getOpValue(inst->getOperand());
    bool isSourceTypeExact = isa<MetatypeInst>(inst->getOperand());

    switch (classifyDynamicCast(SwiftMod, formalSourceType,
                                formalTargetType, isSourceTypeExact,
                                inst->getModule().isWholeModule())) {
    case DynamicCastFeasibility::WillSucceed: {
      SILValue result =
        emitSuccessfulScalarUnconditionalCast(B, SwiftMod, loc, operand,
                                              targetType, formalSourceType,
                                              formalTargetType);
      ValueMap.insert({SILValue(inst), result});
      return;
    }

    case DynamicCastFeasibility::MaySucceed: {
      SILValue result =
        B.createUnconditionalCheckedCast(loc, operand, targetType);
      ValueMap.insert({SILValue(inst), result});
      return;
    }

    // Ok, we have an invalid cast. Insert a trap so we trap at
    // runtime as the spec for the instruction requires and propagate
    // undef to all uses.
    case DynamicCastFeasibility::WillFail:
      B.createBuiltinTrap(loc);
      ValueMap.insert({SILValue(inst),
                       SILValue(SILUndef::get(targetType, inst->getModule()))});
      return;
    }
    llvm_unreachable("bad classification");
  }

  void visitUnconditionalCheckedCastAddrInst(
                                     UnconditionalCheckedCastAddrInst *inst) {
    SILLocation loc = getOpLocation(inst->getLoc());
    SILValue src = getOpValue(inst->getSrc());
    SILValue dest = getOpValue(inst->getDest());
    SILBuilderWithPostProcess<TypeSubstCloner, 16> B(this, inst);

    CanType sourceType = getOpASTType(inst->getSourceType());
    CanType targetType = getOpASTType(inst->getTargetType());

    switch (classifyDynamicCast(SwiftMod, sourceType, targetType,
                                false, inst->getModule().isWholeModule())) {
    case DynamicCastFeasibility::WillSucceed: {
      emitSuccessfulIndirectUnconditionalCast(B, SwiftMod, loc,
                                              inst->getConsumptionKind(),
                                              src, sourceType,
                                              dest, targetType);
      return;
    }

    case DynamicCastFeasibility::MaySucceed: {
      // TODO: simplify?
      B.createUnconditionalCheckedCastAddr(loc, inst->getConsumptionKind(),
                                           src, sourceType,
                                           dest, targetType);
      return;
    }

    // Ok, we have an invalid cast. Insert a trap so we trap at
    // runtime as the spec for the instruction requires and propagate
    // undef to all uses.
    case DynamicCastFeasibility::WillFail: {
      B.createBuiltinTrap(loc);

      // mem2reg's invariants get unhappy if we don't try to
      // initialize a loadable result.
      auto &resultTL = B.getModule().Types.getTypeLowering(dest.getType());
      if (!resultTL.isAddressOnly()) {
        auto undef = SILValue(SILUndef::get(dest.getType().getObjectType(),
                                            B.getModule()));
        B.createStore(loc, undef, dest);
      }
      return;
    }

    }
    llvm_unreachable("bad classification");
  }

  /// Attempt to simplify a conditional checked cast.
  void visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *inst) {
    SILLocation loc = getOpLocation(inst->getLoc());
    SILValue src = getOpValue(inst->getSrc());
    SILValue dest = getOpValue(inst->getDest());
    CanType sourceType = getOpASTType(inst->getSourceType());
    CanType targetType = getOpASTType(inst->getTargetType());
    SILBasicBlock *succBB = getOpBasicBlock(inst->getSuccessBB());
    SILBasicBlock *failBB = getOpBasicBlock(inst->getFailureBB());

    SILBuilderWithPostProcess<TypeSubstCloner, 16> B(this, inst);
    switch (classifyDynamicCast(SwiftMod, sourceType, targetType,
                                false, inst->getModule().isWholeModule())) {
    case DynamicCastFeasibility::WillSucceed: {
      emitSuccessfulIndirectUnconditionalCast(B, SwiftMod, loc,
                                              inst->getConsumptionKind(),
                                              src, sourceType,
                                              dest, targetType);
      B.createBranch(loc, succBB);
      return;
    }

    case DynamicCastFeasibility::MaySucceed: {
      // Try to use the scalar cast instruction.
      if (canUseScalarCheckedCastInstructions(B.getModule(),
                                              sourceType, targetType)) {
        emitIndirectConditionalCastWithScalar(B, SwiftMod, loc,
                                              inst->getConsumptionKind(),
                                              src, sourceType,
                                              dest, targetType,
                                              succBB, failBB);
        return;
      }

      // Otherwise, use the indirect cast.
      B.createCheckedCastAddrBranch(loc, inst->getConsumptionKind(),
                                    src, sourceType,
                                    dest, targetType,
                                    succBB, failBB);
      return;
    }

    case DynamicCastFeasibility::WillFail:
      if (shouldDestroyOnFailure(inst->getConsumptionKind())) {
        auto &srcTL = B.getModule().getTypeLowering(src.getType());
        srcTL.emitDestroyAddress(B, loc, src);
      }
      B.createBranch(loc, failBB);
      return;
    }    
  }

  /// Attempt to simplify a conditional checked cast.
  void visitCheckedCastBranchInst(CheckedCastBranchInst *inst) {
    // We cannot improve on an exact cast.
    if (inst->isExact()) {
      return super::visitCheckedCastBranchInst(inst);
    }

    // Grab both the from and to types.
    SILType loweredSourceType = getOpType(inst->getOperand().getType());
    SILType loweredTargetType = getOpType(inst->getCastType());
    SILLocation loc = getOpLocation(inst->getLoc());
    SILBasicBlock *succBB = getOpBasicBlock(inst->getSuccessBB());
    SILBasicBlock *failBB = getOpBasicBlock(inst->getFailureBB());
    SILBuilderWithPostProcess<TypeSubstCloner, 16> B(this, inst);

    SILValue operand = getOpValue(inst->getOperand());
    bool isSourceTypeExact = isa<MetatypeInst>(inst->getOperand());

    // The non-addr CheckedCastInsts can currently only be used for
    // types that don't have abstraction differences, so this is okay.
    CanType sourceType = loweredSourceType.getSwiftRValueType();
    CanType targetType = loweredTargetType.getSwiftRValueType();

    switch (classifyDynamicCast(SwiftMod, sourceType,
                                targetType, isSourceTypeExact,
                                inst->getModule().isWholeModule())) {
    case DynamicCastFeasibility::WillSucceed: {
      // FIXME: this is a temporary workaround to the fact that
      // we're still using checked_cast_branch for address-only stuff.
      if (loweredTargetType.isAddress()) {
        if (sourceType == targetType) {
          B.createBranch(loc, succBB, operand);
          return;
        }
        goto maySucceed;
      }

      SILValue result =
        emitSuccessfulScalarUnconditionalCast(B, SwiftMod, loc, operand,
                                              loweredTargetType, sourceType,
                                              targetType);

      B.createBranch(loc, succBB, result);
      return;
    }

    maySucceed:
    case DynamicCastFeasibility::MaySucceed: {
      B.createCheckedCastBranch(loc, /*exact*/ false, operand,
                                loweredTargetType, succBB, failBB);
      return;
    }

    case DynamicCastFeasibility::WillFail:
      B.createBranch(loc, failBB);
      return;
    }
  }

  void visitUpcastInst(UpcastInst *Upcast) {
    // If the type substituted type of the operand type and result types match
    // there is no need for an upcast and we can just use the operand.
    if (getOpType(Upcast->getType()) ==
        getOpValue(Upcast->getOperand()).getType()) {
      ValueMap.insert({SILValue(Upcast), getOpValue(Upcast->getOperand())});
      return;
    }
    super::visitUpcastInst(Upcast);
  }

  /// The Swift module that the cloned function belongs to.
  Module *SwiftMod;
  /// The substitutions list for the specialization.
  TypeSubstitutionMap &SubsMap;
  /// The original function to specialize.
  SILFunction &Original;
  /// The substiutions used at the call site.
  ArrayRef<Substitution> ApplySubs;
  /// True, if used for inlining.
  bool Inlining;
};

} // end namespace swift

#endif
