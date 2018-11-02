//===--- TypeSubstCloner.h - Clones code and substitutes types --*- C++ -*-===//
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
// This file defines TypeSubstCloner, which derives from SILCloner and
// has support for type substitution while cloning code that uses generics.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_TYPESUBSTCLONER_H
#define SWIFT_SIL_TYPESUBSTCLONER_H

#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// TypeSubstCloner - a utility class for cloning code while remapping types.
template<typename ImplClass>
class TypeSubstCloner : public SILClonerWithScopes<ImplClass> {
  friend class SILInstructionVisitor<ImplClass>;
  friend class SILCloner<ImplClass>;

  using super = SILClonerWithScopes<ImplClass>;

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    llvm_unreachable("Clients need to explicitly call a base class impl!");
  }

  // A helper class for cloning different kinds of apply instructions.
  // Supports cloning of self-recursive functions.
  class ApplySiteCloningHelper {
    SILValue Callee;
    SubstitutionMap Subs;
    SmallVector<SILValue, 8> Args;
    SubstitutionMap RecursiveSubs;

  public:
    ApplySiteCloningHelper(ApplySite AI, TypeSubstCloner &Cloner)
        : Callee(Cloner.getOpValue(AI.getCallee())) {
      SILType SubstCalleeSILType = Cloner.getOpType(AI.getSubstCalleeSILType());

      Args = Cloner.template getOpValueArray<8>(AI.getArguments());
      SILBuilder &Builder = Cloner.getBuilder();
      Builder.setCurrentDebugScope(Cloner.super::getOpScope(AI.getDebugScope()));

      // Remap substitutions.
      Subs = Cloner.getOpSubstitutionMap(AI.getSubstitutionMap());

      if (!Cloner.Inlining) {
        FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(AI.getCallee());
        if (FRI && FRI->getReferencedFunction() == AI.getFunction() &&
            Subs == Cloner.SubsMap) {
          // Handle recursions by replacing the apply to the callee with an
          // apply to the newly specialized function, but only if substitutions
          // are the same.
          auto LoweredFnTy = Builder.getFunction().getLoweredFunctionType();
          auto RecursiveSubstCalleeSILType = LoweredFnTy;
          auto GenSig = LoweredFnTy->getGenericSignature();
          if (GenSig) {
            // Compute substitutions for the specialized function. These
            // substitutions may be different from the original ones, e.g.
            // there can be less substitutions.
            RecursiveSubs = SubstitutionMap::get(
              AI.getFunction()
                ->getLoweredFunctionType()
                ->getGenericSignature(),
              Subs);

            // Use the new set of substitutions to compute the new
            // substituted callee type.
            RecursiveSubstCalleeSILType = LoweredFnTy->substGenericArgs(
                AI.getModule(), RecursiveSubs);
          }

          // The specialized recursive function may have different calling
          // convention for parameters. E.g. some of former indirect parameters
          // may become direct. Some of indirect return values may become
          // direct. Do not replace the callee in that case.
          if (SubstCalleeSILType.getASTType() == RecursiveSubstCalleeSILType) {
            Subs = RecursiveSubs;
            Callee = Builder.createFunctionRef(
                Cloner.getOpLocation(AI.getLoc()), &Builder.getFunction());
            SubstCalleeSILType =
                SILType::getPrimitiveObjectType(RecursiveSubstCalleeSILType);
          }
        }
      }

      assert(Subs.empty() ||
             SubstCalleeSILType ==
                 Callee->getType().substGenericArgs(AI.getModule(), Subs));
    }

    ArrayRef<SILValue> getArguments() const {
      return Args;
    }

    SILValue getCallee() const {
      return Callee;
    }

    SubstitutionMap getSubstitutions() const {
      return Subs;
    }
  };

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
  using SILClonerWithScopes<ImplClass>::addBlockWithUnreachable;
  using SILClonerWithScopes<ImplClass>::OpenedArchetypesTracker;

  TypeSubstCloner(SILFunction &To,
                  SILFunction &From,
                  SubstitutionMap ApplySubs,
                  SILOpenedArchetypesTracker &OpenedArchetypesTracker,
                  bool Inlining = false)
    : SILClonerWithScopes<ImplClass>(To, OpenedArchetypesTracker, Inlining),
      SwiftMod(From.getModule().getSwiftModule()),
      SubsMap(ApplySubs),
      Original(From),
      Inlining(Inlining) {
  }

  TypeSubstCloner(SILFunction &To,
                  SILFunction &From,
                  SubstitutionMap ApplySubs,
                  bool Inlining = false)
    : SILClonerWithScopes<ImplClass>(To, Inlining),
      SwiftMod(From.getModule().getSwiftModule()),
      SubsMap(ApplySubs),
      Original(From),
      Inlining(Inlining) {
  }


protected:
  SILType remapType(SILType Ty) {
    SILType &Sty = TypeCache[Ty];
    if (!Sty) {
      Sty = Ty.subst(Original.getModule(), SubsMap);
    }
    return Sty;
  }

  CanType remapASTType(CanType ty) {
    return ty.subst(SubsMap)->getCanonicalType();
  }

  ProtocolConformanceRef remapConformance(Type type,
                                          ProtocolConformanceRef conf) {
    return conf.subst(type, SubsMap);
  }

  SubstitutionMap remapSubstitutionMap(SubstitutionMap Subs) {
    return Subs.subst(SubsMap);
  }

  void visitApplyInst(ApplyInst *Inst) {
    ApplySiteCloningHelper Helper(ApplySite(Inst), *this);
    ApplyInst *N =
        getBuilder().createApply(getOpLocation(Inst->getLoc()),
                                 Helper.getCallee(), Helper.getSubstitutions(),
                                 Helper.getArguments(), Inst->isNonThrowing(),
                                 GenericSpecializationInformation::create(
                                   Inst, getBuilder()));
    doPostProcess(Inst, N);
  }

  void visitTryApplyInst(TryApplyInst *Inst) {
    ApplySiteCloningHelper Helper(ApplySite(Inst), *this);
    TryApplyInst *N = getBuilder().createTryApply(
        getOpLocation(Inst->getLoc()), Helper.getCallee(),
        Helper.getSubstitutions(), Helper.getArguments(),
        getOpBasicBlock(Inst->getNormalBB()),
        getOpBasicBlock(Inst->getErrorBB()),
        GenericSpecializationInformation::create(
          Inst, getBuilder()));
    doPostProcess(Inst, N);
  }

  void visitPartialApplyInst(PartialApplyInst *Inst) {
    ApplySiteCloningHelper Helper(ApplySite(Inst), *this);
    auto ParamConvention =
        Inst->getType().getAs<SILFunctionType>()->getCalleeConvention();
    PartialApplyInst *N = getBuilder().createPartialApply(
        getOpLocation(Inst->getLoc()), Helper.getCallee(),
        Helper.getSubstitutions(), Helper.getArguments(), ParamConvention,
        GenericSpecializationInformation::create(
          Inst, getBuilder()));
    doPostProcess(Inst, N);
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
    B.setCurrentDebugScope(super::getOpScope(inst->getDebugScope()));

    auto TrueCount = inst->getTrueBBCount();
    auto FalseCount = inst->getFalseBBCount();

    // Try to use the scalar cast instruction.
    if (canUseScalarCheckedCastInstructions(B.getModule(),
                                            sourceType, targetType)) {
      emitIndirectConditionalCastWithScalar(
          B, SwiftMod, loc, inst->getConsumptionKind(), src, sourceType, dest,
          targetType, succBB, failBB, TrueCount, FalseCount);
      return;
    }

    // Otherwise, use the indirect cast.
    B.createCheckedCastAddrBranch(loc, inst->getConsumptionKind(),
                                  src, sourceType,
                                  dest, targetType,
                                  succBB, failBB);
    return;
  }

  void visitUpcastInst(UpcastInst *Upcast) {
    // If the type substituted type of the operand type and result types match
    // there is no need for an upcast and we can just use the operand.
    if (getOpType(Upcast->getType()) ==
        getOpValue(Upcast->getOperand())->getType()) {
      ValueMap.insert({SILValue(Upcast), getOpValue(Upcast->getOperand())});
      return;
    }
    super::visitUpcastInst(Upcast);
  }

  void visitCopyValueInst(CopyValueInst *Copy) {
    // If the substituted type is trivial, ignore the copy.
    SILType copyTy = getOpType(Copy->getType());
    if (copyTy.isTrivial(Copy->getModule())) {
      ValueMap.insert({SILValue(Copy), getOpValue(Copy->getOperand())});
      return;
    }
    super::visitCopyValueInst(Copy);
  }

  void visitDestroyValueInst(DestroyValueInst *Destroy) {
    // If the substituted type is trivial, ignore the destroy.
    SILType destroyTy = getOpType(Destroy->getOperand()->getType());
    if (destroyTy.isTrivial(Destroy->getModule())) {
      return;
    }
    super::visitDestroyValueInst(Destroy);
  }

  /// The Swift module that the cloned function belongs to.
  ModuleDecl *SwiftMod;
  /// The substitutions list for the specialization.
  SubstitutionMap SubsMap;
  /// Cache for substituted types.
  llvm::DenseMap<SILType, SILType> TypeCache;
  /// The original function to specialize.
  SILFunction &Original;
  /// True, if used for inlining.
  bool Inlining;
};

} // end namespace swift

#endif
