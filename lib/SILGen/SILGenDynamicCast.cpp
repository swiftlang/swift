//===--- SILGenDynamicCast.cpp - SILGen for dynamic casts -----------------===//
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
        Strategy(computeStrategy()) {
    }

    bool isOperandIndirect() const {
      return Strategy == CastStrategy::Address;
    }

    ManagedValue emitOperand(Expr *operand) {
      AbstractionPattern mostGeneral = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origSourceTL = SGF.getTypeLowering(mostGeneral, SourceType);

      SGFContext ctx;

      std::unique_ptr<TemporaryInitialization> temporary;
      if (isOperandIndirect() && SGF.silConv.useLoweredAddresses()) {
        temporary = SGF.emitTemporary(Loc, origSourceTL);
        ctx = SGFContext(temporary.get());
      }

      auto result = SGF.emitRValueAsOrig(operand, mostGeneral,
                                         origSourceTL, ctx);

      if (isOperandIndirect() && SGF.silConv.useLoweredAddresses()) {
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
      if (Strategy == CastStrategy::Address &&
          SGF.silConv.useLoweredAddresses()) {
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

      SILValue resultScalar;
      if (Strategy == CastStrategy::Address) {
        resultScalar = SGF.B.createUnconditionalCheckedCastOpaque(
            Loc, operand.forward(SGF), origTargetTL.getLoweredType());
      } else {
        resultScalar = SGF.B.createUnconditionalCheckedCast(
            Loc, operand.forward(SGF), origTargetTL.getLoweredType());
      }

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
        if (scalarOperandValue->getType().isAddress()) {
          scalarOperandValue = SGF.B.emitLoadValueOperation(
              Loc, scalarOperandValue, LoadOwnershipQualifier::Take);
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
          SILValue argument = trueBB->createPHIArgument(
              origTargetTL.getLoweredType(), ValueOwnershipKind::Owned);
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
          SGF.B.emitDestroyValueOperation(Loc, scalarOperandValue);
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
        value = origTargetTL.emitCopyValue(SGF.B, Loc, value);

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
    CastStrategy computeStrategy() const {
      if (canUseScalarCheckedCastInstructions(SGF.SGM.M,
                                              SourceType, TargetType))
        return CastStrategy::Scalar;
      return CastStrategy::Address;
    }
  };
} // end anonymous namespace

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
                                           Type sourceType,
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
                                         ManagedValue source,
                                         Type sourceType,
                                         SILLocation loc,
                                         Type destType,
                                         SGFContext C,
                                         bool conditional) {
  // Compute substitutions for the intrinsic call.
  auto fromCollection = cast<BoundGenericStructType>(
                          sourceType->getCanonicalType());
  auto toCollection = cast<BoundGenericStructType>(
                        destType->getCanonicalType());
  // Get the intrinsic function.
  auto &ctx = SGF.getASTContext();
  FuncDecl *fn = nullptr;
  if (fromCollection->getDecl() == ctx.getArrayDecl()) {
    fn = conditional ? SGF.SGM.getArrayConditionalCast(loc)
                     : SGF.SGM.getArrayForceCast(loc);
  } else if (fromCollection->getDecl() == ctx.getDictionaryDecl()) {
    fn = (conditional
           ? SGF.SGM.getDictionaryDownCastConditional(loc)
           : SGF.SGM.getDictionaryDownCast(loc));
  } else if (fromCollection->getDecl() == ctx.getSetDecl()) {
    fn = (conditional
           ? SGF.SGM.getSetDownCastConditional(loc)
           : SGF.SGM.getSetDownCast(loc));
  } else {
    llvm_unreachable("unsupported collection upcast kind");
  }

  // This will have been diagnosed by the accessors above.
  if (!fn) return SGF.emitUndefRValue(loc, destType);

  auto fnGenericParams = fn->getGenericSignature()->getGenericParams();
  auto fromSubsts = fromCollection->gatherAllSubstitutions(
      SGF.SGM.SwiftModule, nullptr);
  auto toSubsts = toCollection->gatherAllSubstitutions(
      SGF.SGM.SwiftModule, nullptr);
  assert(fnGenericParams.size() == fromSubsts.size() + toSubsts.size() &&
         "wrong number of generic collection parameters");
  (void) fnGenericParams;

  // Form type parameter substitutions.
  SmallVector<Substitution, 4> subs;
  subs.append(fromSubsts.begin(), fromSubsts.end());
  subs.append(toSubsts.begin(), toSubsts.end());

  return SGF.emitApplyOfLibraryIntrinsic(loc, fn, subs, {source}, C);
}

static ManagedValue
adjustForConditionalCheckedCastOperand(SILLocation loc, ManagedValue src,
                                       CanType sourceType, CanType targetType,
                                       SILGenFunction &SGF) {
  // Reabstract to the most general abstraction, and put it into a
  // temporary if necessary.
  
  // Figure out if we need the value to be in a temporary.
  bool requiresAddress =
    !canUseScalarCheckedCastInstructions(SGF.SGM.M, sourceType, targetType);
  
  AbstractionPattern abstraction = SGF.SGM.M.Types.getMostGeneralAbstraction();
  auto &srcAbstractTL = SGF.getTypeLowering(abstraction, sourceType);
  
  bool hasAbstraction = (src.getType() != srcAbstractTL.getLoweredType());
  
  // Fast path: no re-abstraction required.
  if (!hasAbstraction && (!requiresAddress || src.getType().isAddress())) {
    return src;
  }
  
  std::unique_ptr<TemporaryInitialization> init;
  if (requiresAddress) {
    init = SGF.emitTemporary(loc, srcAbstractTL);

    if (hasAbstraction)
      src = SGF.emitSubstToOrigValue(loc, src, abstraction, sourceType);

    // Okay, if all we need to do is drop the value in an address,
    // this is easy.
    SGF.B.emitStoreValueOperation(loc, src.forward(SGF), init->getAddress(),
                                  StoreOwnershipQualifier::Init);
    init->finishInitialization(SGF);
    return init->getManagedAddress();
  }
  
  assert(hasAbstraction);
  assert(src.getType().isObject() &&
         "address-only type with abstraction difference?");
  
  // Produce the value at +1.
  return SGF.emitSubstToOrigValue(loc, src, abstraction, sourceType);
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
      castKind == CheckedCastKind::SetDowncast) {
    ManagedValue operandMV = SGF.emitRValueAsSingleValue(operand);
    return emitCollectionDowncastExpr(SGF, operandMV, operand->getType(), loc,
                                      targetType, C,
                                      /*conditional=*/false);
  }

  CheckedCastEmitter emitter(SGF, loc, operand->getType(),
                             targetType);
  ManagedValue operandValue = emitter.emitOperand(operand);
  return emitter.emitUnconditionalCast(operandValue, C);
}

RValue Lowering::emitConditionalCheckedCast(SILGenFunction &SGF,
                                            SILLocation loc,
                                            ManagedValue operand,
                                            Type operandType,
                                            Type optTargetType,
                                            CheckedCastKind castKind,
                                            SGFContext C) {
  // Drill into the result type.
  CanType resultObjectType =
    optTargetType->getCanonicalType().getAnyOptionalObjectType();
  assert(resultObjectType);

  // Handle collection downcasts directly; they have specific library
  // entry points.
  if (castKind == CheckedCastKind::ArrayDowncast ||
      castKind == CheckedCastKind::DictionaryDowncast ||
      castKind == CheckedCastKind::SetDowncast) {
    return emitCollectionDowncastExpr(SGF, operand, operandType, loc,
                                      resultObjectType, C,
                                      /*conditional=*/true);
  }

  operand = adjustForConditionalCheckedCastOperand(loc, operand,
                                               operandType->getCanonicalType(),
                                                   resultObjectType, SGF);

  auto someDecl = SGF.getASTContext().getOptionalSomeDecl();
  auto &resultTL = SGF.getTypeLowering(optTargetType);

  // Set up a result buffer if desirable/required.
  SILValue resultBuffer;
  SILValue resultObjectBuffer;
  Optional<TemporaryInitialization> resultObjectTemp;
  SGFContext resultObjectCtx;
  if (resultTL.isAddressOnly() ||
      (C.getEmitInto() && C.getEmitInto()->getAddressOrNull())) {
    SILType resultTy = resultTL.getLoweredType();
    resultBuffer = SGF.getBufferForExprResult(loc, resultTy, C);
    resultObjectBuffer =
      SGF.B.createInitEnumDataAddr(loc, resultBuffer, someDecl,
                    resultTy.getAnyOptionalObjectType().getAddressType());
    resultObjectTemp.emplace(resultObjectBuffer, CleanupHandle::invalid());
    resultObjectCtx = SGFContext(&resultObjectTemp.getValue());
  }

  // Prepare a jump destination here.
  ExitableFullExpr scope(SGF, CleanupLocation::get(loc));

  auto operandCMV = ConsumableManagedValue::forOwned(operand);
  SGF.emitCheckedCastBranch(loc, operandCMV, operandType,
                            resultObjectType, resultObjectCtx,
    // The success path.
    [&](ManagedValue objectValue) {
      // If we're not emitting into a temporary, just wrap up the result
      // in Some and go to the continuation block.
      if (!resultObjectTemp) {
        auto some = SGF.B.createEnum(loc, objectValue.forward(SGF),
                                     someDecl, resultTL.getLoweredType());
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, { some });
        return;
      }

      // Otherwise, make sure the value is in the context.
      if (!objectValue.isInContext()) {
        objectValue.forwardInto(SGF, loc, resultObjectBuffer);
      }
      SGF.B.createInjectEnumAddr(loc, resultBuffer, someDecl);
      SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc);
    },
    // The failure path.
    [&] {
      auto noneDecl = SGF.getASTContext().getOptionalNoneDecl();

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
    auto argument = contBB->createPHIArgument(resultTL.getLoweredType(),
                                              ValueOwnershipKind::Owned);
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
      castKind == CheckedCastKind::SetDowncast) {
    ManagedValue operandMV = SGF.emitRValueAsSingleValue(operand);
    ManagedValue optValue = emitCollectionDowncastExpr(
                              SGF, operandMV, operand->getType(), loc,
                              targetType,
                              SGFContext(), /*conditional=*/true)
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
  auto isa = contBB->createPHIArgument(i1Ty, ValueOwnershipKind::Trivial);
  return isa;
}

