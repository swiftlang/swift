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
  friend class SILVisitor<ImplClass>;
  friend class SILCloner<ImplClass>;

  typedef SILClonerWithScopes<ImplClass> super;

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    llvm_unreachable("Clients need to explicitly call a base class impl!");
  }

  void computeSubsMap() {
    if (auto *env = Original.getGenericEnvironment()) {
      SubsMap = env->getSubstitutionMap(ApplySubs);
    }
  }

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
                  SubstitutionList ApplySubs,
                  SILOpenedArchetypesTracker &OpenedArchetypesTracker,
                  bool Inlining = false)
    : SILClonerWithScopes<ImplClass>(To, OpenedArchetypesTracker, Inlining),
      SwiftMod(From.getModule().getSwiftModule()),
      Original(From),
      ApplySubs(ApplySubs),
      Inlining(Inlining) {
    computeSubsMap();
  }

  TypeSubstCloner(SILFunction &To,
                  SILFunction &From,
                  SubstitutionList ApplySubs,
                  bool Inlining = false)
    : SILClonerWithScopes<ImplClass>(To, Inlining),
      SwiftMod(From.getModule().getSwiftModule()),
      Original(From),
      ApplySubs(ApplySubs),
      Inlining(Inlining) {
    computeSubsMap();
  }


protected:
  SILType remapType(SILType Ty) {
    return Ty.subst(Original.getModule(), SubsMap);
  }

  CanType remapASTType(CanType ty) {
    return ty.subst(SubsMap)->getCanonicalType();
  }

  Substitution remapSubstitution(Substitution sub) {
    // Remap opened archetypes into the cloned context.
    sub = Substitution(
        getASTTypeInClonedContext(sub.getReplacement()->getCanonicalType()),
        sub.getConformances());
    // Now remap the substitution. 
    return sub.subst(SubsMap);
  }

  ProtocolConformanceRef remapConformance(CanType type,
                                          ProtocolConformanceRef conf) {
    Substitution sub(type, conf);
    return remapSubstitution(sub).getConformances()[0];
  }

  void visitClassMethodInst(ClassMethodInst *Inst) {
    getBuilder().setCurrentDebugScope(super::getOpScope(Inst->getDebugScope()));
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

    getBuilder().setCurrentDebugScope(super::getOpScope(Inst->getDebugScope()));
    auto N = getBuilder().createBuiltin(getOpLocation(Inst->getLoc()),
                                        Inst->getName(),
                                        getOpType(Inst->getType()),
                                        TempSubstList, Args);
    doPostProcess(Inst, N);
  }
  
  void visitApplyInst(ApplyInst *Inst) {
    auto Args = this->template getOpValueArray<8>(Inst->getArguments());

    // Handle recursions by replacing the apply to the callee with an apply to
    // the newly specialized function, but only if substitutions are the same.
    SILBuilder &Builder = getBuilder();
    Builder.setCurrentDebugScope(super::getOpScope(Inst->getDebugScope()));
    SILValue CalleeVal = Inst->getCallee();
    if (!Inlining) {
      FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);
      if (FRI && FRI->getReferencedFunction() == Inst->getFunction() &&
          Inst->getSubstitutions() == this->ApplySubs) {
        FRI = Builder.createFunctionRef(getOpLocation(Inst->getLoc()),
                                        &Builder.getFunction());
        ApplyInst *NAI =
          Builder.createApply(getOpLocation(Inst->getLoc()), FRI, Args, Inst->isNonThrowing());
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
        TempSubstList, Args, Inst->isNonThrowing());
    doPostProcess(Inst, N);
  }

  void visitPartialApplyInst(PartialApplyInst *Inst) {
    auto Args = this->template getOpValueArray<8>(Inst->getArguments());

    // Handle recursions by replacing the apply to the callee with an apply to
    // the newly specialized function.
    SILValue CalleeVal = Inst->getCallee();
    SILBuilderWithPostProcess<TypeSubstCloner, 4> Builder(this, Inst);
    Builder.setCurrentDebugScope(super::getOpScope(Inst->getDebugScope()));
    if (!Inlining) {
      FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(CalleeVal);
      if (FRI && FRI->getReferencedFunction() == Inst->getFunction()) {
        FRI = Builder.createFunctionRef(getOpLocation(Inst->getLoc()),
                                        &Builder.getFunction());
        Builder.createPartialApply(getOpLocation(Inst->getLoc()), FRI,
                                   getOpType(Inst->getSubstCalleeSILType()),
                                   SubstitutionList(),
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
    // Specialize the Self substitution of the witness_method.
    auto sub = Inst->getSelfSubstitution();
    sub = sub.subst(SubsMap);

    assert(sub.getConformances().size() == 1 &&
           "didn't get conformance from substitution?!");

    auto Conformance = sub.getConformances()[0];

    auto newLookupType = getOpASTType(Inst->getLookupType());
    if (Conformance.isConcrete()) {
      CanType Ty = Conformance.getConcrete()->getType()->getCanonicalType();

      if (Ty != newLookupType) {
        assert(Ty->isExactSuperclassOf(newLookupType, nullptr) &&
               "Should only create upcasts for sub class.");

        // We use the super class as the new look up type.
        newLookupType = Ty;
      }
    }

    // We already subst so getOpConformance is not needed.
    getBuilder().setCurrentDebugScope(super::getOpScope(Inst->getDebugScope()));
    doPostProcess(
        Inst,
        getBuilder().createWitnessMethod(
            getOpLocation(Inst->getLoc()), newLookupType, Conformance,
            Inst->getMember(), getOpType(Inst->getType()),
            Inst->isVolatile()));
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

  /// The Swift module that the cloned function belongs to.
  ModuleDecl *SwiftMod;
  /// The substitutions list for the specialization.
  SubstitutionMap SubsMap;
  /// The original function to specialize.
  SILFunction &Original;
  /// The substitutions used at the call site.
  SubstitutionList ApplySubs;
  /// True, if used for inlining.
  bool Inlining;
};

} // end namespace swift

#endif
