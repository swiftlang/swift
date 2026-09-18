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
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

ManagedValue Lowering::prepareCOMCastSource(SILGenFunction &SGF,
                                            SILLocation loc,
                                            ManagedValue source) {
  if (!source.getType().isMoveOnlyWrapped())
    return source;

  if (source.getType().isAddress()) {
    auto address =
        SGF.B.createMoveOnlyWrapperToCopyableAddr(loc, source.getValue());
    return ManagedValue::forBorrowedAddressRValue(address);
  }

  if (source.getOwnershipKind() != OwnershipKind::Guaranteed)
    source = source.borrow(SGF, loc);
  return SGF.B.createGuaranteedMoveOnlyWrapperToCopyableValue(loc, source);
}

CastStrategy Lowering::computeCastStrategy(SILGenFunction &SGF,
                                           CanType sourceType,
                                           CanType targetType) {
  auto targetObjectType = targetType->lookThroughAllOptionalTypes();
  if (targetObjectType->isCOMExistentialType())
    return CastStrategy::COM;
  if (canSILUseScalarCheckedCastInstructions(
          SGF.SGM.M, SGF.F.hasLoweredAddresses(), sourceType, targetType))
    return CastStrategy::Scalar;
  return CastStrategy::Address;
}

namespace {
  class CheckedCastEmitter {
    SILGenFunction &SGF;
    SILLocation Loc;
    CanType SourceType;
    CanType TargetType;

    CastStrategy Strategy;
    CheckedCastInstOptions Options;

  public:
    CheckedCastEmitter(SILGenFunction &SGF, SILLocation loc, Type sourceType,
                       Type targetType)
        : SGF(SGF), Loc(loc), SourceType(sourceType->getCanonicalType()),
          TargetType(targetType->getCanonicalType()),
          Strategy(computeCastStrategy(SGF, SourceType, TargetType)),
          Options(computedOptions()) {}

    CastStrategy getStrategy() const { return Strategy; }

    CastConsumptionKind getDefaultConsumption() const {
      switch (Strategy) {
      case CastStrategy::Address:
      case CastStrategy::Scalar:
        return CastConsumptionKind::TakeAlways;
      case CastStrategy::COM:
        return CastConsumptionKind::CopyOnSuccess;
      }
      llvm_unreachable("invalid cast strategy");
    }

    ManagedValue emitOperand(Expr *operand) {
      AbstractionPattern mostGeneral = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origSourceTL = SGF.getTypeLowering(mostGeneral, SourceType);

      switch (Strategy) {
      case CastStrategy::COM: {
        auto result = SGF.emitRValueAsOrig(operand, mostGeneral, origSourceTL,
                                           SGFContext::AllowGuaranteedPlusZero);
        result = prepareCOMCastSource(SGF, Loc, result);
        if (result.getType().isAddress())
          return result;

        auto temporary =
            SGF.emitTemporaryAllocation(Loc, origSourceTL.getLoweredType());
        return SGF.B.createStoreBorrowOrTrivial(Loc, result.borrow(SGF, Loc),
                                                temporary);
      }
      case CastStrategy::Address: {
        auto temporary = SGF.emitTemporary(Loc, origSourceTL);
        auto result = SGF.emitRValueAsOrig(operand, mostGeneral, origSourceTL,
                                           SGFContext(temporary.get()));
        // Force the result into the temporary if it's not already there.
        if (!result.isInContext()) {
          result.forwardInto(SGF, Loc, temporary->getAddress());
          temporary->finishInitialization(SGF);
        }
        return temporary->getManagedAddress();
      }
      case CastStrategy::Scalar:
        return SGF.emitRValueAsOrig(operand, mostGeneral, origSourceTL,
                                    SGFContext());
      }
      llvm_unreachable("invalid cast strategy");
    }

    RValue emitUnconditionalCast(ManagedValue operand, SGFContext ctx) {
      // The cast functions don't know how to work with anything but
      // the most general possible abstraction level.
      AbstractionPattern abstraction = SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origTargetTL = SGF.getTypeLowering(abstraction, TargetType);
      auto &substTargetTL = SGF.getTypeLowering(TargetType);
      bool hasAbstraction =
        (origTargetTL.getLoweredType() != substTargetTL.getLoweredType());

      switch (Strategy) {
      case CastStrategy::Address: {
        // Take the operand and build into the destination buffer.
        SILValue resultBuffer =
            createAbstractResultBuffer(hasAbstraction, origTargetTL, ctx);
        SGF.B.createUnconditionalCheckedCastAddr(
            Loc, Options, operand.forward(SGF), SourceType, resultBuffer,
            TargetType);
        return RValue(SGF, Loc, TargetType,
                      finishFromResultBuffer(hasAbstraction, resultBuffer,
                                             abstraction, origTargetTL, ctx));
      }
      case CastStrategy::COM: {
        SILValue resultBuffer =
            createAbstractResultBuffer(hasAbstraction, origTargetTL, ctx);

        SGF.B.createUnconditionalCheckedCastAddr(
            Loc, Options, operand.getValue(), SourceType, resultBuffer,
            TargetType, /*isCopy=*/true);
        return RValue(SGF, Loc, TargetType,
                      finishFromResultBuffer(hasAbstraction, resultBuffer,
                                             abstraction, origTargetTL, ctx));
      }
      case CastStrategy::Scalar: {
        ManagedValue result = SGF.B.createUnconditionalCheckedCast(
            Loc, Options, operand, origTargetTL.getLoweredType(), TargetType);
        return RValue(SGF, Loc, TargetType,
                      finishFromResultScalar(hasAbstraction, result,
                                             CastConsumptionKind::TakeAlways,
                                             abstraction, origTargetTL, ctx));
      }
      }
      llvm_unreachable("invalid cast strategy");
    }

    /// Emit a conditional cast.
    void emitConditional(
        ManagedValue operand,
        CastConsumptionKind consumption, SGFContext ctx,
        llvm::function_ref<void(ManagedValue)> handleTrue,
        llvm::function_ref<void(std::optional<ManagedValue>)> handleFalse,
        ProfileCounter TrueCount = ProfileCounter(),
        ProfileCounter FalseCount = ProfileCounter()) {
      // The cast instructions don't know how to work with anything
      // but the most general possible abstraction level.
      AbstractionPattern abstraction =
          SGF.SGM.Types.getMostGeneralAbstraction();
      auto &origTargetTL = SGF.getTypeLowering(abstraction, TargetType);
      auto &substTargetTL = SGF.getTypeLowering(TargetType);
      bool hasAbstraction =
          (origTargetTL.getLoweredType() != substTargetTL.getLoweredType());

      SILBasicBlock *falseBB = SGF.B.splitBlockForFallthrough();
      SILBasicBlock *trueBB = SGF.B.splitBlockForFallthrough();

      // Emit the branch.
      ManagedValue operandValue;
      SILValue resultBuffer;
      switch (Strategy) {
      case CastStrategy::Address:
      case CastStrategy::COM: {
        assert(operand.getType().isAddress());
        resultBuffer =
            createAbstractResultBuffer(hasAbstraction, origTargetTL, ctx);
        SILValue source = consumption == CastConsumptionKind::CopyOnSuccess
                              ? operand.getValue()
                              : operand.forward(SGF);
        SGF.B.createCheckedCastAddrBranch(
            Loc, Options, consumption, source, SourceType, resultBuffer,
            TargetType, trueBB, falseBB, TrueCount, FalseCount);
        break;
      }
      case CastStrategy::Scalar: {
        // Tolerate being passed an address here.  It comes up during switch
        // emission.
        operandValue = std::move(operand);
        if (operandValue.getType().isAddress()) {
          operandValue = SGF.B.createLoadTake(Loc, operandValue);
        }

        // If we are not supposed to destroy this value on failure, then we need
        // to borrow it.
        if (!shouldDestroyOnFailure(consumption)) {
          operandValue = operandValue.borrow(SGF, Loc);
        }
        SGF.B.createCheckedCastBranch(Loc, /*exact*/ false,
                                      Options, operandValue,
                                      SourceType, origTargetTL.getLoweredType(),
                                      TargetType, trueBB, falseBB, TrueCount,
                                      FalseCount);
        break;
      }
      }

      // Emit the success block.
      SGF.B.setInsertionPoint(trueBB);
      {
        FullExpr scope(SGF.Cleanups, CleanupLocation(Loc));

        ManagedValue result;
        switch (Strategy) {
        case CastStrategy::Address:
        case CastStrategy::COM:
          result = finishFromResultBuffer(hasAbstraction, resultBuffer,
                                          abstraction, origTargetTL, ctx);
          break;
        case CastStrategy::Scalar: {
          // If we had copy_on_success, then we need to use a guaranteed
          // argument.
          assert(!shouldTakeOnSuccess(consumption)
                 || operandValue.getOwnershipKind().isCompatibleWith(
                        OwnershipKind::Owned)
                        && "cast consumption does not match ownership");
          ManagedValue termResult =
              SGF.B.createForwardedTermResult(origTargetTL.getLoweredType());
          result =
              finishFromResultScalar(hasAbstraction, termResult, consumption,
                                     abstraction, origTargetTL, ctx);
          break;
        }
        }

        handleTrue(result);
        assert(!SGF.B.hasValidInsertionPoint() && "handler did not end block");
      }

      // Emit the failure block.
      SGF.B.setInsertionPoint(falseBB);
      {
        FullExpr scope(SGF.Cleanups, CleanupLocation(Loc));

        // If we have an address only type, do not handle the consumption
        // rules. These are handled for us by the user.
        switch (Strategy) {
        case CastStrategy::Address:
        case CastStrategy::COM:
          handleFalse(std::nullopt);
          assert(!SGF.B.hasValidInsertionPoint() &&
                 "handler did not end block");
          return;
        case CastStrategy::Scalar:
          break;
        }

        // Otherwise, we use the following strategy:
        //
        // 1. If we have a take_always, we create a phi node argument for the
        // failure case and a scope for that so that it is immediately
        // destroyed.
        //
        // 2. If we have a take_on_success or copy_on_success, then on failure,
        // we propagate through the default argument, but do not clean it up. On
        // the false case, our user must treat the taken value as a new value.
        if (shouldDestroyOnFailure(consumption)) {
          {
            FullExpr argScope(SGF.Cleanups, CleanupLocation(Loc));
            SGF.B.createForwardedTermResult(operandValue.getType());
          }
          handleFalse(std::nullopt);
          assert(!SGF.B.hasValidInsertionPoint() &&
                 "handler did not end block");
          return;
        }
        ManagedValue result =
            SGF.B.createForwardedTermResult(operandValue.getType());
        switch (consumption) {
        case CastConsumptionKind::BorrowAlways:
        case CastConsumptionKind::CopyOnSuccess:
          handleFalse(std::nullopt);
          break;
        case CastConsumptionKind::TakeAlways:
        case CastConsumptionKind::TakeOnSuccess:
          handleFalse(result);
          break;
        }

        assert(!SGF.B.hasValidInsertionPoint() && "handler did not end block");
      }
    }

    SILValue createAbstractResultBuffer(bool hasAbstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      if (!hasAbstraction) {
        if (auto address = ctx.getAddressForInPlaceInitialization(SGF, Loc))
          return address;
      }

      return SGF.emitTemporaryAllocation(Loc, origTargetTL.getLoweredType());
    }

    ManagedValue finishFromResultBuffer(bool hasAbstraction, SILValue buffer,
                                        AbstractionPattern abstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      if (!hasAbstraction) {
        if (ctx.finishInPlaceInitialization(SGF))
          return ManagedValue::forInContext();
      }

      ManagedValue result;
      if (origTargetTL.isLoadableOrOpaque(SGF.F)) {
        result = SGF.emitLoad(Loc, buffer, origTargetTL, ctx, IsTake);
      } else {
        result = SGF.emitManagedBufferWithCleanup(buffer, origTargetTL);
      }

      if (hasAbstraction) {
        result =
            SGF.emitOrigToSubstValue(Loc, result, abstraction, TargetType, ctx);
      }
      return result;
    }

    /// Our cast succeeded and gave us this abstracted value.
    ManagedValue finishFromResultScalar(bool hasAbstraction, ManagedValue value,
                                        CastConsumptionKind consumption,
                                        AbstractionPattern abstraction,
                                        const TypeLowering &origTargetTL,
                                        SGFContext ctx) {
      ManagedValue result = value;
      // Copy the result if this is copy-on-success.
      if (!shouldTakeOnSuccess(consumption))
        result = result.copy(SGF, Loc);

      // Re-abstract if necessary.
      if (hasAbstraction) {
        result =
            SGF.emitOrigToSubstValue(Loc, result, abstraction, TargetType, ctx);
      }

      return result;
    }

  private:
    CheckedCastInstOptions computedOptions() const {
      return CheckedCastInstOptions()
        .withIsolatedConformances(computedIsolatedConformances());
    }
    
    CastingIsolatedConformances computedIsolatedConformances() const {
      // Non-existential types don't carry conformances, so we always allow
      // isolated conformances.
      if (!TargetType->isAnyExistentialType())
        return CastingIsolatedConformances::Allow;

      // If there is a conformance to SendableMetatype, then this existential
      // can leave the current isolation domain.
      ASTContext &ctx = TargetType->getASTContext();
      Type checkType;
      if (auto existentialMetatype = TargetType->getAs<ExistentialMetatypeType>())
        checkType = existentialMetatype->getInstanceType();
      else
        checkType = TargetType;

      // If there are no non-marker protocols in the existential, there's no
      // need to prohibit isolated conformances.
      auto layout = checkType->getExistentialLayout();
      if (!layout.containsNonMarkerProtocols())
        return CastingIsolatedConformances::Allow;

      // If the type conforms to SendableMetatype, prohibit isolated
      // conformances.
      auto proto = ctx.getProtocol(KnownProtocolKind::SendableMetatype);
      if (proto && lookupConformance(checkType, proto, /*allowMissing=*/false))
        return CastingIsolatedConformances::Prohibit;

      return CastingIsolatedConformances::Allow;
    }
  };
} // end anonymous namespace

void SILGenFunction::emitCheckedCastBranch(
    SILLocation loc, Expr *source, Type targetType, SGFContext ctx,
    llvm::function_ref<void(ManagedValue)> handleTrue,
    llvm::function_ref<void(std::optional<ManagedValue>)> handleFalse,
    ProfileCounter TrueCount, ProfileCounter FalseCount) {
  CheckedCastEmitter emitter(*this, loc, source->getType(), targetType);
  ManagedValue operand = emitter.emitOperand(source);
  emitter.emitConditional(operand, emitter.getDefaultConsumption(), ctx,
                          handleTrue, handleFalse, TrueCount, FalseCount);
}

void SILGenFunction::emitCheckedCastBranch(
    SILLocation loc, ConsumableManagedValue src, Type sourceType,
    CanType targetType, SGFContext ctx,
    llvm::function_ref<void(ManagedValue)> handleTrue,
    llvm::function_ref<void(std::optional<ManagedValue>)> handleFalse,
    ProfileCounter TrueCount, ProfileCounter FalseCount) {
  CheckedCastEmitter emitter(*this, loc, sourceType, targetType);
  emitter.emitConditional(src.getFinalManagedValue(), src.getFinalConsumption(),
                          ctx, handleTrue, handleFalse, TrueCount, FalseCount);
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
  auto fromCollection = sourceType->getCanonicalType();
  auto toCollection = destType->getCanonicalType();
  // Get the intrinsic function.
  FuncDecl *fn = nullptr;
  if (fromCollection->isArray()) {
    fn = conditional ? SGF.SGM.getArrayConditionalCast(loc)
                     : SGF.SGM.getArrayForceCast(loc);
  } else if (fromCollection->isDictionary()) {
    fn = (conditional
           ? SGF.SGM.getDictionaryDownCastConditional(loc)
           : SGF.SGM.getDictionaryDownCast(loc));
  } else if (fromCollection->isSet()) {
    fn = (conditional
           ? SGF.SGM.getSetDownCastConditional(loc)
           : SGF.SGM.getSetDownCast(loc));
  } else {
    llvm_unreachable("unsupported collection upcast kind");
  }

  return SGF.emitCollectionConversion(loc, fn, fromCollection, toCollection,
                                      source, C);
}

static ManagedValue
adjustForConditionalCheckedCastOperand(SILLocation loc, ManagedValue src,
                                       CanType sourceType, SILGenFunction &SGF,
                                       CastStrategy strategy) {
  // Reabstract to the most general abstraction, and put it into a
  // temporary if necessary.
  AbstractionPattern abstraction = SGF.SGM.M.Types.getMostGeneralAbstraction();
  auto &srcAbstractTL = SGF.getTypeLowering(abstraction, sourceType);
  bool hasAbstraction = (src.getType() != srcAbstractTL.getLoweredType());

  switch (strategy) {
  case CastStrategy::COM: {
    src = prepareCOMCastSource(SGF, loc, src);
    if (src.getType().isObject()) {
      // A COM cast only needs the source value's address. Do not reabstract a
      // one-word COM existential to the opaque existential representation.
      auto temporary = SGF.emitTemporaryAllocation(loc, src.getType());
      return SGF.B.createStoreBorrowOrTrivial(loc, src.borrow(SGF, loc),
                                              temporary);
    }
    hasAbstraction = (src.getType() != srcAbstractTL.getLoweredType());
    [[fallthrough]];
  }
  case CastStrategy::Address: {
    if (!hasAbstraction && src.getType().isAddress())
      return src;

    auto init = SGF.emitTemporary(loc, srcAbstractTL);
    if (hasAbstraction)
      src = SGF.emitSubstToOrigValue(loc, src, abstraction, sourceType);

    SGF.B.emitStoreValueOperation(loc, src.forward(SGF), init->getAddress(),
                                  StoreOwnershipQualifier::Init);
    init->finishInitialization(SGF);
    return init->getManagedAddress();
  }
  case CastStrategy::Scalar:
    if (!hasAbstraction)
      return src;
    assert(src.getType().isObject() &&
           "address-only type with abstraction difference?");
    // Produce the value at +1.
    return SGF.emitSubstToOrigValue(loc, src, abstraction, sourceType);
  }
  llvm_unreachable("invalid cast strategy");
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

RValue Lowering::emitConditionalCheckedCast(
    SILGenFunction &SGF, SILLocation loc, ManagedValue operand,
    Type operandType, Type optTargetType, CheckedCastKind castKind,
    SGFContext C, ProfileCounter TrueCount, ProfileCounter FalseCount) {
  // Drill into the result type.
  CanType resultObjectType =
      optTargetType->getCanonicalType().getOptionalObjectType();
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

  CanType sourceType = operandType->getCanonicalType();
  CheckedCastEmitter emitter(SGF, loc, sourceType, resultObjectType);
  operand = adjustForConditionalCheckedCastOperand(loc, operand, sourceType,
                                                   SGF, emitter.getStrategy());

  auto someDecl = SGF.getASTContext().getOptionalSomeDecl();
  auto &resultTL = SGF.getTypeLowering(optTargetType);

  // Set up a result buffer if desirable/required.
  SILValue resultBuffer;
  SILValue resultObjectBuffer;
  std::optional<TemporaryInitialization> resultObjectTemp;
  SGFContext resultObjectCtx;
  if (!resultTL.isLoadableOrOpaque(SGF.F)
      || (C.getEmitInto()
          && C.getEmitInto()->canPerformInPlaceInitialization())) {
    SILType resultTy = resultTL.getLoweredType();
    resultBuffer = SGF.getBufferForExprResult(loc, resultTy, C);
    resultObjectBuffer = SGF.B.createInitEnumDataAddr(
        loc, resultBuffer, someDecl,
        resultTy.getOptionalObjectType().getAddressType());
    resultObjectTemp.emplace(resultObjectBuffer, CleanupHandle::invalid());
    resultObjectCtx = SGFContext(&resultObjectTemp.value());
  }

  // Prepare a jump destination here.
  ExitableFullExpr scope(SGF, CleanupLocation(loc));

  emitter.emitConditional(
      operand, emitter.getDefaultConsumption(), resultObjectCtx,
      // The success path.
      [&](ManagedValue objectValue) {
        // If we're not emitting into a temporary, just wrap up the result
        // in Some and go to the continuation block.
        if (!resultObjectTemp) {
          auto some = SGF.B.createEnum(loc, objectValue.forward(SGF), someDecl,
                                       resultTL.getLoweredType());
          SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, {some});
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
      [&](std::optional<ManagedValue> Value) {
        assert(!Value.has_value() &&
               "cast must not propagate the source on failure");
        auto noneDecl = SGF.getASTContext().getOptionalNoneDecl();

        // If we're not emitting into a temporary, just wrap up the result
        // in None and go to the continuation block.
        if (!resultObjectTemp) {
          auto none = SGF.B.createEnum(loc, nullptr, noneDecl,
                                       resultTL.getLoweredType());
          SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, {none});

          // Just construct the enum directly in the context.
        } else {
          SGF.B.createInjectEnumAddr(loc, resultBuffer, noneDecl);
          SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc);
        }
      },
      TrueCount, FalseCount);

  // Enter the continuation block.
  SILBasicBlock *contBlock = scope.exit();

  ManagedValue result;
  if (resultObjectTemp) {
    result = SGF.manageBufferForExprResult(resultBuffer, resultTL, C);
  } else {
    auto argument = contBlock->createPhiArgument(resultTL.getLoweredType(),
                                                 OwnershipKind::Owned);
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
  ExitableFullExpr scope(SGF, CleanupLocation(loc));

  auto i1Ty = SILType::getBuiltinIntegerType(1, SGF.getASTContext());

  // When we pass in an expr, we perform a take_always cast.
  SGF.emitCheckedCastBranch(
      loc, operand, targetType, SGFContext(),
      [&](ManagedValue value) {
        SILValue yes = SGF.B.createIntegerLiteral(loc, i1Ty, 1);
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, yes);
      },
      [&](std::optional<ManagedValue> Value) {
        assert(!Value.has_value() && "Expected take_always semantics");
        SILValue no = SGF.B.createIntegerLiteral(loc, i1Ty, 0);
        SGF.Cleanups.emitBranchAndCleanups(scope.getExitDest(), loc, no);
      });

  auto contBB = scope.exit();
  auto isa = contBB->createPhiArgument(i1Ty, OwnershipKind::None);
  return isa;
}

