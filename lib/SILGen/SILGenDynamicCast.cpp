//===--- SILGenDynamicCast.cpp - SILGen for dynamic casts -----------------===//
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

#include "SILGenDynamicCast.h"

#include "Initialization.h"
#include "RValue.h"
#include "Scope.h"
#include "ExitableFullExpr.h"
#include "swift/AST/AST.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

namespace {
  class CheckedCastEmitter {
    SILGenFunction &SGF;
    SILLocation Loc;
    CanType SourceType;
    CanType TargetType;

    enum class CastStrategy : uint8_t {
      Address,
      Scalar,
    };
    CastStrategy Strategy;

  public:
    CheckedCastEmitter(SILGenFunction &SGF, SILLocation loc,
                       Type sourceType, Type targetType)
      : SGF(SGF), Loc(loc), SourceType(sourceType->getCanonicalType()),
        TargetType(targetType->getCanonicalType()),
        Strategy(getStrategy()) {
    }

    bool isOperandIndirect() const {
      return Strategy == CastStrategy::Address;
    }

    ManagedValue emitOperand(Expr *operand) {
      AbstractionPattern mostGeneral = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origSourceTL = SGF.getTypeLowering(mostGeneral, SourceType);

      SGFContext ctx;

      std::unique_ptr<TemporaryInitialization> temporary;
      if (isOperandIndirect()) {
        temporary = SGF.emitTemporary(Loc, origSourceTL);
        ctx = SGFContext(temporary.get());
      }

      auto result = SGF.emitRValueAsOrig(operand, mostGeneral,
                                         origSourceTL, ctx);

      if (isOperandIndirect()) {
        // Force the result into the temporary if it's not already there.
        if (!result.isInContext()) {
          result.forwardInto(SGF, Loc, temporary->getAddress());
          temporary->finishInitialization(SGF);
        }
        return temporary->getManagedAddress();
      }

      return result;
    }

    RValue emitUnconditionalCast(ManagedValue operand, SGFContext ctx) {
      // The cast functions don't know how to work with anything but
      // the most general possible abstraction level.
      AbstractionPattern abstraction = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origTargetTL = SGF.getTypeLowering(abstraction, TargetType);
      auto &substTargetTL = SGF.getTypeLowering(TargetType);
      bool hasAbstraction =
        (origTargetTL.getLoweredType() != substTargetTL.getLoweredType());

      // If we're using checked_cast_addr, take the operand (which
      // should be an address) and build into the destination buffer.
      ManagedValue abstractResult;
      if (Strategy == CastStrategy::Address) {
        SILValue resultBuffer =
          createAbstractResultBuffer(hasAbstraction, origTargetTL, ctx);
        SGF.B.createUnconditionalCheckedCastAddr(Loc,
                                             CastConsumptionKind::TakeAlways,
                                             operand.forward(SGF), SourceType,
                                             resultBuffer, TargetType);
        return RValue(SGF, Loc, TargetType,
                      finishFromResultBuffer(hasAbstraction, resultBuffer,
                                             abstraction, origTargetTL, ctx));
      }

      SILValue resultScalar =
        SGF.B.createUnconditionalCheckedCast(Loc, operand.forward(SGF),
                                             origTargetTL.getLoweredType());
      return RValue(SGF, Loc, TargetType,
                    finishFromResultScalar(hasAbstraction, resultScalar,
                                           CastConsumptionKind::TakeAlways,
                                           abstraction, origTargetTL, ctx));
    }

    /// Emit a conditional cast.
    void emitConditional(ManagedValue operand, CastConsumptionKind consumption,
                         SGFContext ctx,
                         const std::function<void(ManagedValue)> &handleTrue,
                         const std::function<void()> &handleFalse) {
      // The cast instructions don't know how to work with anything
      // but the most general possible abstraction level.
      AbstractionPattern abstraction = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origTargetTL = SGF.getTypeLowering(abstraction, TargetType);
      auto &substTargetTL = SGF.getTypeLowering(TargetType);
      bool hasAbstraction =
        (origTargetTL.getLoweredType() != substTargetTL.getLoweredType());

      SILBasicBlock *falseBB = SGF.B.splitBlockForFallthrough();
      SILBasicBlock *trueBB = SGF.B.splitBlockForFallthrough();

      // Emit the branch.
      SILValue scalarOperandValue;
      SILValue resultBuffer;
      if (Strategy == CastStrategy::Address) {
        assert(operand.getType().isAddress());
        resultBuffer =
          createAbstractResultBuffer(hasAbstraction, origTargetTL, ctx);
        SGF.B.createCheckedCastAddrBranch(Loc, consumption,
                                          operand.forward(SGF), SourceType,
                                          resultBuffer, TargetType,
                                          trueBB, falseBB);
      } else {
        // Tolerate being passed an address here.  It comes up during switch
        //emission.
        scalarOperandValue = operand.forward(SGF);
        if (scalarOperandValue.getType().isAddress()) {
          scalarOperandValue = SGF.B.createLoad(Loc, scalarOperandValue);
        }
        SGF.B.createCheckedCastBranch(Loc, /*exact*/ false, scalarOperandValue,
                                      origTargetTL.getLoweredType(),
                                      trueBB, falseBB);
      }

      // Emit the success block.
      SGF.B.setInsertionPoint(trueBB);
      {
        FullExpr scope(SGF.Cleanups, CleanupLocation::get(Loc));

        ManagedValue result;
        if (Strategy == CastStrategy::Address) {
          result = finishFromResultBuffer(hasAbstraction, resultBuffer,
                                          abstraction, origTargetTL, ctx);
        } else {
          SILValue argument = new (SGF.F.getModule())
            SILArgument(trueBB, origTargetTL.getLoweredType());
          result = finishFromResultScalar(hasAbstraction, argument, consumption,
                                          abstraction, origTargetTL, ctx);
        }

        handleTrue(result);
        assert(!SGF.B.hasValidInsertionPoint() && "handler did not end block");
      }

      // Emit the failure block.
      SGF.B.setInsertionPoint(falseBB);
      {
        FullExpr scope(SGF.Cleanups, CleanupLocation::get(Loc));

        // If we're using the scalar strategy, handle the consumption rules.
        if (Strategy != CastStrategy::Address &&
            shouldDestroyOnFailure(consumption)) {
          SGF.B.emitReleaseValueOperation(Loc, scalarOperandValue);
        }

        handleFalse();
        assert(!SGF.B.hasValidInsertionPoint() && "handler did not end block");
      }
    }

    SILValue createAbstractResultBuffer(bool hasAbstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      if (!hasAbstraction) {
        if (auto emitInto = ctx.getEmitInto()) {
          if (SILValue addr = emitInto->getAddressOrNull()) {
            return addr;
          }
        }
      }

      return SGF.emitTemporaryAllocation(Loc, origTargetTL.getLoweredType());
    }

    ManagedValue finishFromResultBuffer(bool hasAbstraction,
                                        SILValue buffer,
                                        AbstractionPattern abstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      if (!hasAbstraction) {
        if (auto emitInto = ctx.getEmitInto()) {
          if (emitInto->getAddressOrNull()) {
            emitInto->finishInitialization(SGF);
            return ManagedValue::forInContext();
          }
        }
      }

      ManagedValue result;
      if (!origTargetTL.isAddressOnly()) {
        result = SGF.emitLoad(Loc, buffer, origTargetTL, ctx, IsTake);
      } else {
        result = SGF.emitManagedBufferWithCleanup(buffer, origTargetTL);
      }

      if (hasAbstraction) {
        result = SGF.emitOrigToSubstValue(Loc, result, abstraction,
                                          TargetType, ctx);
      }
      return result;
    }

    /// Our cast succeeded and gave us this abstracted value.
    ManagedValue finishFromResultScalar(bool hasAbstraction, SILValue value,
                                        CastConsumptionKind consumption,
                                        AbstractionPattern abstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      // Retain the result if this is copy-on-success.
      if (!shouldTakeOnSuccess(consumption))
        origTargetTL.emitRetainValue(SGF.B, Loc, value);

      // Enter a cleanup for the +1 result.
      ManagedValue result
        = SGF.emitManagedRValueWithCleanup(value, origTargetTL);

      // Re-abstract if necessary.
      if (hasAbstraction) {
        result = SGF.emitOrigToSubstValue(Loc, result, abstraction,
                                          TargetType, ctx);
      }
      return result;
    }

  private:
    CastStrategy getStrategy() const {
      if (canUseScalarCheckedCastInstructions(SourceType, TargetType))
        return CastStrategy::Scalar;
      return CastStrategy::Address;
    }
  };
}

void SILGenFunction::emitCheckedCastBranch(SILLocation loc, Expr *source,
                                           Type targetType,
                                           SGFContext ctx,
                                std::function<void(ManagedValue)> handleTrue,
                                           std::function<void()> handleFalse) {
  CheckedCastEmitter emitter(*this, loc, source->getType(), targetType);
  ManagedValue operand = emitter.emitOperand(source);
  emitter.emitConditional(operand, CastConsumptionKind::TakeAlways, ctx,
                          handleTrue, handleFalse);
}

void SILGenFunction::emitCheckedCastBranch(SILLocation loc,
                                           ConsumableManagedValue src,
                                           CanType sourceType,
                                           CanType targetType,
                                           SGFContext ctx,
                                std::function<void(ManagedValue)> handleTrue,
                                           std::function<void()> handleFalse) {
  CheckedCastEmitter emitter(*this, loc, sourceType, targetType);
  emitter.emitConditional(src.getFinalManagedValue(), src.getFinalConsumption(),
                          ctx, handleTrue, handleFalse);
}

/// Emit a collection downcast expression.
///
/// \param conditional Whether to emit a conditional downcast; if
/// false, this will emit a forced downcast.
static RValue emitCollectionDowncastExpr(SILGenFunction &SGF,
                                         Expr *source,
                                         SILLocation loc,
                                         Type destType,
                                         SGFContext C,
                                         bool conditional,
                                         bool bridgesFromObjC) {
  // Get the sub expression argument as a managed value
  auto mv = SGF.emitRValueAsSingleValue(source);

  // Compute substitutions for the intrinsic call.
  auto fromCollection = cast<BoundGenericStructType>(
                          source->getType()->getCanonicalType());
  auto toCollection = cast<BoundGenericStructType>(
                        destType->getCanonicalType());
  // Get the intrinsic function.
  auto &ctx = SGF.getASTContext();
  FuncDecl *fn = nullptr;
  if (fromCollection->getDecl() == ctx.getArrayDecl()) {
    fn = conditional ? ctx.getArrayConditionalCast(nullptr)
                          : ctx.getArrayForceCast(nullptr);
  } else if (fromCollection->getDecl() == ctx.getDictionaryDecl()) {
    fn = bridgesFromObjC
           ? (conditional
                ? ctx.getDictionaryBridgeFromObjectiveCConditional(nullptr)
                : ctx.getDictionaryBridgeFromObjectiveC(nullptr))
           : (conditional
                ? ctx.getDictionaryDownCastConditional(nullptr)
                : ctx.getDictionaryDownCast(nullptr));
  } else if (fromCollection->getDecl() == ctx.getSetDecl()) {
    fn = bridgesFromObjC
           ? (conditional
                ? ctx.getSetBridgeFromObjectiveCConditional(nullptr)
                : ctx.getSetBridgeFromObjectiveC(nullptr))
           : (conditional
                ? ctx.getSetDownCastConditional(nullptr)
                : ctx.getSetDownCast(nullptr));
  } else {
    llvm_unreachable("unsupported collection upcast kind");
  }

  auto fnArcheTypes = fn->getGenericParams()->getPrimaryArchetypes();
  auto fromSubsts = fromCollection->getSubstitutions(SGF.SGM.SwiftModule,nullptr);
  auto toSubsts = toCollection->getSubstitutions(SGF.SGM.SwiftModule,nullptr);
  assert(fnArcheTypes.size() == fromSubsts.size() + toSubsts.size() &&
         "wrong number of generic collection parameters");

  // Form type parameter substitutions.
  int aIdx = 0;
  SmallVector<Substitution, 4> subs;
  for (auto sub: fromSubsts){
    subs.push_back(Substitution{fnArcheTypes[aIdx++], sub.getReplacement(),
                                sub.getConformances()});
  }
  for (auto sub: toSubsts){
    subs.push_back(Substitution{fnArcheTypes[aIdx++], sub.getReplacement(),
                                sub.getConformances()});
  }

  auto emitApply = SGF.emitApplyOfLibraryIntrinsic(loc, fn, subs, {mv}, C);

  Type resultType = destType;
  if (conditional)
    resultType = OptionalType::get(resultType);
  return RValue(SGF, loc, resultType->getCanonicalType(), emitApply);
}

RValue Lowering::emitUnconditionalCheckedCast(SILGenFunction &SGF,
                                              SILLocation loc,
                                              Expr *operand,
                                              Type targetType,
                                              CheckedCastKind castKind,
                                              SGFContext C) {
  // Handle collection downcasts directly; they have specific library
  // entry points.
  if (castKind == CheckedCastKind::ArrayDowncast ||
      castKind == CheckedCastKind::DictionaryDowncast ||
      castKind == CheckedCastKind::DictionaryDowncastBridged ||
      castKind == CheckedCastKind::SetDowncast ||
      castKind == CheckedCastKind::SetDowncastBridged) {
    bool bridgesFromObjC
      = (castKind == CheckedCastKind::DictionaryDowncastBridged ||
         castKind == CheckedCastKind::SetDowncastBridged);
    return emitCollectionDowncastExpr(SGF, operand, loc, targetType, C,
                                      /*conditional=*/false,
                                      bridgesFromObjC);
  }

  CheckedCastEmitter emitter(SGF, loc, operand->getType(),
                             targetType);
  ManagedValue operandValue = emitter.emitOperand(operand);
  return emitter.emitUnconditionalCast(operandValue, C);
}

RValue Lowering::emitConditionalCheckedCast(SILGenFunction &SGF,
                                            SILLocation loc,
                                            Expr *operand,
                                            Type optTargetType,
                                            CheckedCastKind castKind,
                                            SGFContext C) {
  // Drill into the result type.
  OptionalTypeKind optKind;
  CanType resultObjectType =
    optTargetType->getCanonicalType().getAnyOptionalObjectType(optKind);
  assert(resultObjectType);

  // Handle collection downcasts directly; they have specific library
  // entry points.
  if (castKind == CheckedCastKind::ArrayDowncast ||
      castKind == CheckedCastKind::DictionaryDowncast ||
      castKind == CheckedCastKind::DictionaryDowncastBridged ||
      castKind == CheckedCastKind::SetDowncast ||
      castKind == CheckedCastKind::SetDowncastBridged) {
    bool bridgesFromObjC
      = (castKind == CheckedCastKind::DictionaryDowncastBridged ||
         castKind == CheckedCastKind::SetDowncastBridged);
    return emitCollectionDowncastExpr(SGF, operand, loc,
                                      resultObjectType, C,
                                      /*conditional=*/true,
                                      bridgesFromObjC);
  }

  auto someDecl = SGF.getASTContext().getOptionalSomeDecl(optKind);
  auto &resultTL = SGF.getTypeLowering(optTargetType);

  // Optional<T> currently fully abstracts its object.
  auto abstraction = SGF.SGM.Types.getMostGeneralAbstraction();
  auto &abstractResultObjectTL =
    SGF.getTypeLowering(abstraction, resultObjectType);
  auto &resultObjectTL = SGF.getTypeLowering(resultObjectType);
  bool hasAbstraction =
    (resultObjectTL.getLoweredType() != abstractResultObjectTL.getLoweredType());

  // Set up a result buffer if desirable/required.
  SILValue resultBuffer;
  SILValue resultObjectBuffer;
  Optional<TemporaryInitialization> resultObjectTemp;
  SGFContext resultObjectCtx;
  if (resultTL.isAddressOnly() ||
      (!hasAbstraction && C.getEmitInto() &&
       C.getEmitInto()->getAddressOrNull())) {
    resultBuffer = SGF.getBufferForExprResult(loc, resultTL.getLoweredType(), C);
    resultObjectBuffer =
      SGF.B.createInitEnumDataAddr(loc, resultBuffer, someDecl,
                     abstractResultObjectTL.getLoweredType().getAddressType());
    resultObjectTemp.emplace(resultObjectBuffer, CleanupHandle::invalid());

    if (!hasAbstraction)
      resultObjectCtx = SGFContext(&resultObjectTemp.getValue());
  }

  // Prepare a jump destination here.
  ExitableFullExpr scope(SGF, CleanupLocation::get(loc));

  SGF.emitCheckedCastBranch(loc, operand,
                            resultObjectType, resultObjectCtx,
    // The success path.
    [&](ManagedValue objectValue) {
      // If we're not emitting into a temporary, just wrap up the result
      // in Some and go to the continuation block.
      if (!resultObjectTemp) {
        if (hasAbstraction)
          objectValue = SGF.emitSubstToOrigValue(loc, objectValue, abstraction,
                                                 resultObjectType);
        auto some = SGF.B.createEnum(loc, objectValue.forward(SGF),
                                     someDecl, resultTL.getLoweredType());
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, { some });
        return;
      }

      // Otherwise, make sure the value is in the context.
      if (!objectValue.isInContext() && hasAbstraction) {
        objectValue = SGF.emitSubstToOrigValue(loc, objectValue, abstraction,
                                               resultObjectType,
                                   SGFContext(&resultObjectTemp.getValue()));
      }
      if (!objectValue.isInContext()) {
        objectValue.forwardInto(SGF, loc, resultObjectBuffer);
      }
      SGF.B.createInjectEnumAddr(loc, resultBuffer, someDecl);
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc);
    },
    // The failure path.
    [&] {
      auto noneDecl = SGF.getASTContext().getOptionalNoneDecl(optKind);

      // If we're not emitting into a temporary, just wrap up the result
      // in None and go to the continuation block.
      if (!resultObjectTemp) {
        auto none =
          SGF.B.createEnum(loc, nullptr, noneDecl, resultTL.getLoweredType());
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, { none });

      // Just construct the enum directly in the context.
      } else {
        SGF.B.createInjectEnumAddr(loc, resultBuffer, noneDecl);
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc);
      }
    });

  // Enter the continuation block.
  auto contBB = scope.exit();

  ManagedValue result;
  if (resultObjectTemp) {
    result = SGF.manageBufferForExprResult(resultBuffer, resultTL, C);
  } else {
    auto argument =
      new (SGF.F.getModule()) SILArgument(contBB, resultTL.getLoweredType());
    result = SGF.emitManagedRValueWithCleanup(argument, resultTL);
  }

  return RValue(SGF, loc, optTargetType->getCanonicalType(), result);
}

SILValue Lowering::emitIsa(SILGenFunction &SGF, SILLocation loc,
                           Expr *operand, Type targetType,
                           CheckedCastKind castKind) {
  // Handle collection downcasts separately.
  if (castKind == CheckedCastKind::ArrayDowncast ||
      castKind == CheckedCastKind::DictionaryDowncast ||
      castKind == CheckedCastKind::DictionaryDowncastBridged ||
      castKind == CheckedCastKind::SetDowncast ||
      castKind == CheckedCastKind::SetDowncastBridged) {
    bool bridgesFromObjC
      = (castKind == CheckedCastKind::DictionaryDowncastBridged ||
         castKind == CheckedCastKind::SetDowncastBridged);
    ManagedValue optValue = emitCollectionDowncastExpr(
                              SGF, operand, loc,
                              targetType,
                              SGFContext(), /*conditional=*/true,
                              bridgesFromObjC)
      .getAsSingleValue(SGF, loc);

    // Materialize the input.
    SILValue optValueTemp;
    if (optValue.getType().isAddress()) {
      optValueTemp = optValue.forward(SGF);
    } else {
      optValueTemp = SGF.emitTemporaryAllocation(loc, optValue.getType());
      optValue.forwardInto(SGF, loc, optValueTemp);
    }

    return SGF.emitDoesOptionalHaveValue(loc, optValueTemp);
  }

  // Prepare a jump destination here.
  ExitableFullExpr scope(SGF, CleanupLocation::get(loc));

  auto i1Ty = SILType::getBuiltinIntegerType(1, SGF.getASTContext());

  SGF.emitCheckedCastBranch(loc, operand, targetType, SGFContext(),
    [&](ManagedValue value) {
      SILValue yes = SGF.B.createIntegerLiteral(loc, i1Ty, 1);
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, yes);
    },
    [&] {
      SILValue no = SGF.B.createIntegerLiteral(loc, i1Ty, 0);
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, no);
    });

  auto contBB = scope.exit();
  auto isa = new (SGF.SGM.M) SILArgument(contBB, i1Ty);
  return isa;
}

