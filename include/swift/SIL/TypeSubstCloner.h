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
  using SILCloner<ImplClass>::asImpl;
  using SILCloner<ImplClass>::getBuilder;
  using SILCloner<ImplClass>::getOpLocation;
  using SILCloner<ImplClass>::getOpValue;
  using SILCloner<ImplClass>::getOpASTType;
  using SILCloner<ImplClass>::getOpType;
  using SILCloner<ImplClass>::getOpBasicBlock;
  using SILCloner<ImplClass>::doPostProcess;
  using SILCloner<ImplClass>::ValueMap;

  TypeSubstCloner(SILFunction &To,
                  SILFunction &From,
                  TypeSubstitutionMap &ContextSubs,
                  ArrayRef<Substitution> ApplySubs)
    : SILClonerWithScopes<ImplClass>(To),
      SwiftMod(From.getModule().getSwiftModule()),
      SubsMap(ContextSubs),
      Original(From),
      ApplySubs(ApplySubs) { }

protected:
  SILType remapType(SILType Ty) {
    return SILType::substType(Original.getModule(), SwiftMod, SubsMap, Ty);
  }

  CanType remapASTType(CanType ty) {
    return ty.subst(SwiftMod, SubsMap, false, nullptr)->getCanonicalType();
  }

  ProtocolConformance *remapConformance(SILType Ty, ProtocolConformance *C) {
    // If Ty does not have unbound generic types, we did not specialize it so
    // just return C. This relies on the fact that we do not partially
    // specialize.
    if (!Ty.hasArchetype())
      return C;

    // Otherwise we need to create a specialized conformance for C. Remap the
    // type...
    CanType Type = remapType(Ty).getSwiftRValueType()->getCanonicalType();

    // And create the new specialized conformance.
    ASTContext &AST = SwiftMod->getASTContext();
    return AST.getSpecializedConformance(Type, C, ApplySubs);
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
                                        ApplySubs));

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
                                        ApplySubs));

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
                                        ApplySubs);

    assert(sub.Conformance.size() == 1 &&
           "didn't get conformance from substitution?!");

    // If we don't have a witness table for this conformance, create a witness
    // table declaration for it.
    SILFunction &Cloned = getBuilder().getFunction();
    SILModule &OtherMod = Cloned.getModule();
    if (!OtherMod.lookUpWitnessTable(sub.Conformance[0]).first) {
      auto normal = sub.Conformance[0]->getRootNormalConformance();
      auto linkage = Lowering::TypeConverter
                  ::getLinkageForProtocolConformance(normal, NotForDefinition);
      OtherMod.createWitnessTableDeclaration(sub.Conformance[0],
                                             linkage);
    }
    // We already subst so getOpConformance is not needed.
    SILBuilder &Builder = getBuilder();
    doPostProcess(Inst, Builder.createWitnessMethod(
                            getOpLocation(Inst->getLoc()),
                            getOpType(Inst->getLookupType()),
                            sub.Conformance[0], Inst->getMember(),
                            getOpType(Inst->getType()), Inst->isVolatile()));
  }

  /// Attempt to simplify an unconditional checked cast.
  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *inst) {
    // Grab both the from and to types.
    SILType sourceType = getOpType(inst->getOperand().getType());
    SILType targetType = getOpType(inst->getType());
    SILLocation loc = getOpLocation(inst->getLoc());
    SILBuilder &B = getBuilder();

    // The non-addr CheckedCastInsts can currently only be used for
    // types that don't have abstraction differences, so this is okay.
    CanType formalSourceType = sourceType.getSwiftRValueType();
    CanType formalTargetType = targetType.getSwiftRValueType();

    SILValue operand = getOpValue(inst->getOperand());

    switch (classifyDynamicCast(SwiftMod, formalSourceType, formalTargetType)) {
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
      B.createApply(loc, B.createBuiltinTrap(loc), {});
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
    SILBuilder &B = getBuilder();

    CanType sourceType = getOpASTType(inst->getSourceType());
    CanType targetType = getOpASTType(inst->getTargetType());

    switch (classifyDynamicCast(SwiftMod, sourceType, targetType)) {
    case DynamicCastFeasibility::WillSucceed: {
      emitSuccessfulIndirectUnconditionalCast(B, SwiftMod, loc,
                                              inst->getConsumptionKind(),
                                              src, sourceType,
                                              dest, targetType);
      return;
    }

    case DynamicCastFeasibility::MaySucceed: {
      // TODO: simplify?
      auto newInst =
        B.createUnconditionalCheckedCastAddr(loc, inst->getConsumptionKind(),
                                             src, sourceType,
                                             dest, targetType);
      doPostProcess(inst, newInst);
      return;
    }

    // Ok, we have an invalid cast. Insert a trap so we trap at
    // runtime as the spec for the instruction requires and propagate
    // undef to all uses.
    case DynamicCastFeasibility::WillFail: {
      auto newInst = B.createApply(loc, B.createBuiltinTrap(loc), {});
      doPostProcess(inst, newInst);

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

    SILBuilder &B = getBuilder();
    switch (classifyDynamicCast(SwiftMod, sourceType, targetType)) {
    case DynamicCastFeasibility::WillSucceed: {
      emitSuccessfulIndirectUnconditionalCast(B, SwiftMod, loc,
                                              inst->getConsumptionKind(),
                                              src, sourceType,
                                              dest, targetType);
      auto br = B.createBranch(loc, succBB);
      doPostProcess(inst, br);
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
      auto br = B.createCheckedCastAddrBranch(loc, inst->getConsumptionKind(),
                                              src, sourceType,
                                              dest, targetType,
                                              succBB, failBB);
      doPostProcess(inst, br);
      return;
    }

    case DynamicCastFeasibility::WillFail:
      if (shouldDestroyOnFailure(inst->getConsumptionKind())) {
        auto &srcTL = B.getModule().getTypeLowering(src.getType());
        srcTL.emitDestroyAddress(B, loc, src);
      }
      doPostProcess(inst, B.createBranch(loc, failBB));
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
    SILBuilder &B = getBuilder();

    SILValue operand = getOpValue(inst->getOperand());

    // The non-addr CheckedCastInsts can currently only be used for
    // types that don't have abstraction differences, so this is okay.
    CanType sourceType = loweredSourceType.getSwiftRValueType();
    CanType targetType = loweredTargetType.getSwiftRValueType();

    switch (classifyDynamicCast(SwiftMod, sourceType, targetType)) {
    case DynamicCastFeasibility::WillSucceed: {
      // FIXME: this is a temporary workaround to the fact that
      // we're still using checked_cast_branch for address-only stuff.
      if (loweredTargetType.isAddress()) {
        if (sourceType == targetType) {
          auto br = B.createBranch(loc, succBB, operand);
          doPostProcess(inst, br);
          return;
        }
        goto maySucceed;
      }

      SILValue result =
        emitSuccessfulScalarUnconditionalCast(B, SwiftMod, loc, operand,
                                              loweredTargetType, sourceType,
                                              targetType);

      auto br = B.createBranch(loc, succBB, result);
      doPostProcess(inst, br);
      return;
    }

    maySucceed:
    case DynamicCastFeasibility::MaySucceed: {
      auto br = B.createCheckedCastBranch(loc, /*exact*/ false, operand,
                                          loweredTargetType, succBB, failBB);
      doPostProcess(inst, br);
      return;
    }

    case DynamicCastFeasibility::WillFail:
      doPostProcess(inst, B.createBranch(loc, failBB));
      return;
    }
  }

  static SILLinkage getSpecializedLinkage(SILLinkage orig) {
    switch (orig) {
    case SILLinkage::Public:
    case SILLinkage::PublicExternal:
    case SILLinkage::Shared:
    case SILLinkage::SharedExternal:
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
  /// The substiutions used at the call site.
  ArrayRef<Substitution> ApplySubs;
};

} // end namespace swift

#endif
