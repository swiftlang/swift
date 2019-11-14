//===--- ResultPlan.cpp ---------------------------------------------------===//
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

#include "ResultPlan.h"
#include "Callee.h"
#include "Conversion.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "SILGenFunction.h"
#include "swift/AST/GenericEnvironment.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                                Result Plans
//===----------------------------------------------------------------------===//

namespace {

/// A result plan for evaluating an indirect result into the address
/// associated with an initialization.
class InPlaceInitializationResultPlan final : public ResultPlan {
  Initialization *init;

public:
  InPlaceInitializationResultPlan(Initialization *init) : init(init) {}

  RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                ArrayRef<ManagedValue> &directResults) override {
    init->finishInitialization(SGF);
    return RValue::forInContext();
  }
  void
  gatherIndirectResultAddrs(SILGenFunction &SGF, SILLocation loc,
                            SmallVectorImpl<SILValue> &outList) const override {
    outList.emplace_back(init->getAddressForInPlaceInitialization(SGF, loc));
  }
};

/// A cleanup that handles the delayed emission of an indirect buffer for opened
/// Self arguments.
class IndirectOpenedSelfCleanup final : public Cleanup {
  SILValue box;
public:
  IndirectOpenedSelfCleanup()
    : box()
  {}
  
  void setBox(SILValue b) {
    assert(!box && "buffer already set?!");
    box = b;
  }
  
  void emit(SILGenFunction &SGF, CleanupLocation loc, ForUnwind_t forUnwind)
  override {
    assert(box && "buffer never emitted before activating cleanup?!");
    SGF.B.createDeallocBox(loc, box);
  }
  
  void dump(SILGenFunction &SGF) const override {
    llvm::errs() << "IndirectOpenedSelfCleanup\n";
    if (box)
      box->print(llvm::errs());
  }
};

/// Map a type expressed in terms of opened archetypes into a context-free
/// dependent type, returning the type, a generic signature with parameters
/// corresponding to each opened type,
static std::tuple<CanType, CanGenericSignature, SubstitutionMap>
mapTypeOutOfOpenedExistentialContext(CanType t) {
  SmallVector<OpenedArchetypeType *, 4> openedTypes;
  t->getOpenedExistentials(openedTypes);

  ArrayRef<Type> openedTypesAsTypes(
    reinterpret_cast<const Type *>(openedTypes.data()),
    openedTypes.size());

  SmallVector<GenericTypeParamType *, 4> params;
  for (unsigned i : indices(openedTypes)) {
    params.push_back(GenericTypeParamType::get(0, i, t->getASTContext()));
  }
  
  auto mappedSig = GenericSignature::get(params, {});
  auto mappedSubs = SubstitutionMap::get(mappedSig, openedTypesAsTypes, {});

  auto mappedTy = t.subst(
    [&](SubstitutableType *t) -> Type {
      auto index = std::find(openedTypes.begin(), openedTypes.end(), t)
        - openedTypes.begin();
      assert(index != openedTypes.end() - openedTypes.begin());
      return params[index];
    },
    MakeAbstractConformanceForGenericType());

  return std::make_tuple(mappedTy->getCanonicalType(mappedSig),
                         mappedSig->getCanonicalSignature(),
                         mappedSubs);
}

/// A result plan for an indirectly-returned opened existential value.
///
/// This defers allocating the temporary for the result to a later point so that
/// it happens after the arguments are evaluated.
class IndirectOpenedSelfResultPlan final : public ResultPlan {
  AbstractionPattern origType;
  CanType substType;
  CleanupHandle handle = CleanupHandle::invalid();
  mutable SILValue resultBox, resultBuf;

public:
  IndirectOpenedSelfResultPlan(SILGenFunction &SGF,
                               AbstractionPattern origType,
                               CanType substType)
    : origType(origType), substType(substType)
  {
    // Create a cleanup to deallocate the stack buffer at the proper scope.
    // We won't emit the buffer till later, after arguments have been opened,
    // though.
    SGF.Cleanups.pushCleanupInState<IndirectOpenedSelfCleanup>(
                                                         CleanupState::Dormant);
    handle = SGF.Cleanups.getCleanupsDepth();
  }
  
  void
  gatherIndirectResultAddrs(SILGenFunction &SGF, SILLocation loc,
                            SmallVectorImpl<SILValue> &outList) const override {
    assert(!resultBox && "already created temporary?!");
    
    // We allocate the buffer as a box because the scope nesting won't clean
    // this up with good stack discipline relative to any stack allocations that
    // occur during argument emission. Escape analysis during mandatory passes
    // ought to clean this up.

    auto resultTy = SGF.getLoweredType(origType, substType).getASTType();
    CanType layoutTy;
    CanGenericSignature layoutSig;
    SubstitutionMap layoutSubs;
    std::tie(layoutTy, layoutSig, layoutSubs)
      = mapTypeOutOfOpenedExistentialContext(resultTy);
    
    auto boxLayout = SILLayout::get(SGF.getASTContext(),
      layoutSig->getCanonicalSignature(),
      SILField(layoutTy->getCanonicalType(layoutSig), true));
    
    resultBox = SGF.B.createAllocBox(loc,
      SILBoxType::get(SGF.getASTContext(),
                      boxLayout,
                      layoutSubs));
    
    // Complete the cleanup to deallocate this buffer later, after we're
    // finished with the argument.
    static_cast<IndirectOpenedSelfCleanup&>(SGF.Cleanups.getCleanup(handle))
      .setBox(resultBox);
    SGF.Cleanups.setCleanupState(handle, CleanupState::Active);

    resultBuf = SGF.B.createProjectBox(loc, resultBox, 0);
    outList.emplace_back(resultBuf);
  }

  RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                ArrayRef<ManagedValue> &directResults) override {
    assert(resultBox && "never emitted temporary?!");
    
    // Lower the unabstracted result type.
    auto &substTL = SGF.getTypeLowering(substType);

    ManagedValue value;
    // If the value isn't address-only, go ahead and load.
    if (!substTL.isAddressOnly()) {
      auto load = substTL.emitLoad(SGF.B, loc, resultBuf,
                                   LoadOwnershipQualifier::Take);
      value = SGF.emitManagedRValueWithCleanup(load);
    } else {
      value = SGF.emitManagedRValueWithCleanup(resultBuf);
    }

    // A Self return should never be further abstracted. It's also never emitted
    // into context; we disable that optimization because Self may not even
    // be available to pre-allocate a stack buffer before we prepare a call.
    return RValue(SGF, loc, substType, value);
  }
};

/// A result plan for working with a single value and potentially
/// reabstracting it.  The value can actually be a tuple if the
/// abstraction is opaque.
class ScalarResultPlan final : public ResultPlan {
  std::unique_ptr<TemporaryInitialization> temporary;
  AbstractionPattern origType;
  Initialization *init;
  SILFunctionTypeRepresentation rep;

public:
  ScalarResultPlan(std::unique_ptr<TemporaryInitialization> &&temporary,
                   AbstractionPattern origType, Initialization *init,
                   SILFunctionTypeRepresentation rep)
      : temporary(std::move(temporary)), origType(origType), init(init),
        rep(rep) {}

  RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                ArrayRef<ManagedValue> &directResults) override {
    // Lower the unabstracted result type.
    auto &substTL = SGF.getTypeLowering(substType);

    // Claim the value:
    ManagedValue value;

    // If we were created with a temporary, that address was passed as
    // an indirect result.
    if (temporary) {
      // Establish the cleanup.
      temporary->finishInitialization(SGF);
      value = temporary->getManagedAddress();

      // If the value isn't address-only, go ahead and load.
      if (!substTL.isAddressOnly()) {
        auto load = substTL.emitLoad(SGF.B, loc, value.forward(SGF),
                                     LoadOwnershipQualifier::Take);
        value = SGF.emitManagedRValueWithCleanup(load);
      }

      // Otherwise, it was returned as a direct result.
    } else {
      value = directResults.front();
      directResults = directResults.slice(1);
    }

    // Reabstract the value if the types don't match.  This can happen
    // due to either substitution reabstractions or bridging.
    SILType loweredResultTy = substTL.getLoweredType();
    if (value.getType().hasAbstractionDifference(rep, loweredResultTy)) {
      Conversion conversion = [&] {
        // Assume that a C-language API doesn't have substitution
        // reabstractions.  This shouldn't be necessary, but
        // emitOrigToSubstValue can get upset.
        if (getSILFunctionLanguage(rep) == SILFunctionLanguage::C) {
          return Conversion::getBridging(Conversion::BridgeResultFromObjC,
                                         origType.getType(), substType,
                                         loweredResultTy);
        } else {
          return Conversion::getOrigToSubst(origType, substType);
        }
      }();

      // Attempt to peephole this conversion into the context.
      if (init) {
        if (auto outerConversion = init->getAsConversion()) {
          if (outerConversion->tryPeephole(SGF, loc, value, conversion)) {
            outerConversion->finishInitialization(SGF);
            return RValue::forInContext();
          }
        }
      }

      // If that wasn't possible, just apply the conversion.
      value = conversion.emit(SGF, loc, value, SGFContext(init));

      // If that successfully emitted into the initialization, we're done.
      if (value.isInContext()) {
        return RValue::forInContext();
      }
    }

    // Otherwise, forcibly emit into the initialization if it exists.
    if (init) {
      init->copyOrInitValueInto(SGF, loc, value, /*init*/ true);
      init->finishInitialization(SGF);
      return RValue::forInContext();

      // Otherwise, we've got the r-value we want.
    } else {
      return RValue(SGF, loc, substType, value);
    }
  }

  void
  gatherIndirectResultAddrs(SILGenFunction &SGF, SILLocation loc,
                            SmallVectorImpl<SILValue> &outList) const override {
    if (!temporary)
      return;
    outList.emplace_back(temporary->getAddress());
  }
};

/// A result plan which calls copyOrInitValueInto on an Initialization
/// using a temporary buffer initialized by a sub-plan.
class InitValueFromTemporaryResultPlan final : public ResultPlan {
  Initialization *init;
  ResultPlanPtr subPlan;
  std::unique_ptr<TemporaryInitialization> temporary;

public:
  InitValueFromTemporaryResultPlan(
      Initialization *init, ResultPlanPtr &&subPlan,
      std::unique_ptr<TemporaryInitialization> &&temporary)
      : init(init), subPlan(std::move(subPlan)),
        temporary(std::move(temporary)) {}

  RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                ArrayRef<ManagedValue> &directResults) override {
    RValue subResult = subPlan->finish(SGF, loc, substType, directResults);
    assert(subResult.isInContext() && "sub-plan didn't emit into context?");
    (void)subResult;

    ManagedValue value = temporary->getManagedAddress();
    init->copyOrInitValueInto(SGF, loc, value, /*init*/ true);
    init->finishInitialization(SGF);

    return RValue::forInContext();
  }

  void
  gatherIndirectResultAddrs(SILGenFunction &SGF, SILLocation loc,
                            SmallVectorImpl<SILValue> &outList) const override {
    subPlan->gatherIndirectResultAddrs(SGF, loc, outList);
  }
};

/// A result plan which calls copyOrInitValueInto using the result of
/// a sub-plan.
class InitValueFromRValueResultPlan final : public ResultPlan {
  Initialization *init;
  ResultPlanPtr subPlan;

public:
  InitValueFromRValueResultPlan(Initialization *init, ResultPlanPtr &&subPlan)
      : init(init), subPlan(std::move(subPlan)) {}

  RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                ArrayRef<ManagedValue> &directResults) override {
    RValue subResult = subPlan->finish(SGF, loc, substType, directResults);
    ManagedValue value = std::move(subResult).getAsSingleValue(SGF, loc);

    init->copyOrInitValueInto(SGF, loc, value, /*init*/ true);
    init->finishInitialization(SGF);

    return RValue::forInContext();
  }

  void
  gatherIndirectResultAddrs(SILGenFunction &SGF, SILLocation loc,
                            SmallVectorImpl<SILValue> &outList) const override {
    subPlan->gatherIndirectResultAddrs(SGF, loc, outList);
  }
};

/// A result plan which produces a larger RValue from a bunch of
/// components.
class TupleRValueResultPlan final : public ResultPlan {
  SmallVector<ResultPlanPtr, 4> eltPlans;

public:
  TupleRValueResultPlan(ResultPlanBuilder &builder, AbstractionPattern origType,
                        CanTupleType substType) {
    // Create plans for all the elements.
    eltPlans.reserve(substType->getNumElements());
    for (auto i : indices(substType->getElementTypes())) {
      AbstractionPattern origEltType = origType.getTupleElementType(i);
      CanType substEltType = substType.getElementType(i);
      eltPlans.push_back(builder.build(nullptr, origEltType, substEltType));
    }
  }

  RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                ArrayRef<ManagedValue> &directResults) override {
    RValue tupleRV(substType);

    // Finish all the component tuples.
    auto substTupleType = cast<TupleType>(substType);
    assert(substTupleType.getElementTypes().size() == eltPlans.size());
    for (auto i : indices(substTupleType.getElementTypes())) {
      RValue eltRV = eltPlans[i]->finish(
          SGF, loc, substTupleType.getElementType(i), directResults);
      tupleRV.addElement(std::move(eltRV));
    }

    return tupleRV;
  }

  void
  gatherIndirectResultAddrs(SILGenFunction &SGF, SILLocation loc,
                            SmallVectorImpl<SILValue> &outList) const override {
    for (const auto &eltPlan : eltPlans) {
      eltPlan->gatherIndirectResultAddrs(SGF, loc, outList);
    }
  }
};

/// A result plan which evaluates into the sub-components
/// of a splittable tuple initialization.
class TupleInitializationResultPlan final : public ResultPlan {
  Initialization *tupleInit;
  SmallVector<InitializationPtr, 4> eltInitsBuffer;
  MutableArrayRef<InitializationPtr> eltInits;
  SmallVector<ResultPlanPtr, 4> eltPlans;

public:
  TupleInitializationResultPlan(ResultPlanBuilder &builder,
                                Initialization *tupleInit,
                                AbstractionPattern origType,
                                CanTupleType substType)
      : tupleInit(tupleInit) {

    // Get the sub-initializations.
    eltInits = tupleInit->splitIntoTupleElements(builder.SGF, builder.loc,
                                                 substType, eltInitsBuffer);

    // Create plans for all the sub-initializations.
    eltPlans.reserve(substType->getNumElements());
    for (auto i : indices(substType->getElementTypes())) {
      AbstractionPattern origEltType = origType.getTupleElementType(i);
      CanType substEltType = substType.getElementType(i);
      Initialization *eltInit = eltInits[i].get();
      eltPlans.push_back(builder.build(eltInit, origEltType, substEltType));
    }
  }

  RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                ArrayRef<ManagedValue> &directResults) override {
    auto substTupleType = cast<TupleType>(substType);
    assert(substTupleType.getElementTypes().size() == eltPlans.size());
    for (auto i : indices(substTupleType.getElementTypes())) {
      auto eltType = substTupleType.getElementType(i);
      RValue eltRV = eltPlans[i]->finish(SGF, loc, eltType, directResults);
      assert(eltRV.isInContext());
      (void)eltRV;
    }
    tupleInit->finishInitialization(SGF);

    return RValue::forInContext();
  }

  void
  gatherIndirectResultAddrs(SILGenFunction &SGF, SILLocation loc,
                            SmallVectorImpl<SILValue> &outList) const override {
    for (const auto &eltPlan : eltPlans) {
      eltPlan->gatherIndirectResultAddrs(SGF, loc, outList);
    }
  }
};

class ForeignErrorInitializationPlan final : public ResultPlan {
  SILLocation loc;
  LValue lvalue;
  ResultPlanPtr subPlan;
  ManagedValue managedErrorTemp;
  CanType unwrappedPtrType;
  PointerTypeKind ptrKind;
  bool isOptional;
  CanType errorPtrType;

public:
  ForeignErrorInitializationPlan(SILGenFunction &SGF, SILLocation loc,
                                 const CalleeTypeInfo &calleeTypeInfo,
                                 ResultPlanPtr &&subPlan)
      : loc(loc), subPlan(std::move(subPlan)) {
    unsigned errorParamIndex =
        calleeTypeInfo.foreignError->getErrorParameterIndex();
    auto substFnType = calleeTypeInfo.substFnType;
    SILParameterInfo errorParameter =
        substFnType->getParameters()[errorParamIndex];
    // We assume that there's no interesting reabstraction here beyond a layer
    // of optional.
    errorPtrType = errorParameter.getArgumentType(SGF.SGM.M, substFnType);
    unwrappedPtrType = errorPtrType;
    Type unwrapped = errorPtrType->getOptionalObjectType();
    isOptional = (bool) unwrapped;

    if (unwrapped)
      unwrappedPtrType = unwrapped->getCanonicalType();

    auto errorType =
        CanType(unwrappedPtrType->getAnyPointerElementType(ptrKind));
    auto &errorTL = SGF.getTypeLowering(errorType);

    // Allocate a temporary.
    SILValue errorTemp =
        SGF.emitTemporaryAllocation(loc, errorTL.getLoweredType());

    // Nil-initialize it.
    SGF.emitInjectOptionalNothingInto(loc, errorTemp, errorTL);

    // Enter a cleanup to destroy the value there.
    managedErrorTemp = SGF.emitManagedBufferWithCleanup(errorTemp, errorTL);

    // Create the appropriate pointer type.
    lvalue = LValue::forAddress(SGFAccessKind::ReadWrite,
                                ManagedValue::forLValue(errorTemp),
                                /*TODO: enforcement*/ None,
                                AbstractionPattern(errorType), errorType);
  }

  RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                ArrayRef<ManagedValue> &directResults) override {
    return subPlan->finish(SGF, loc, substType, directResults);
  }

  void
  gatherIndirectResultAddrs(SILGenFunction &SGF, SILLocation loc,
                            SmallVectorImpl<SILValue> &outList) const override {
    subPlan->gatherIndirectResultAddrs(SGF, loc, outList);
  }

  Optional<std::pair<ManagedValue, ManagedValue>>
  emitForeignErrorArgument(SILGenFunction &SGF, SILLocation loc) override {
    SILGenFunction::PointerAccessInfo pointerInfo = {
      unwrappedPtrType, ptrKind, SGFAccessKind::ReadWrite
    };
    auto pointerValue =
        SGF.emitLValueToPointer(loc, std::move(lvalue), pointerInfo);

    // Wrap up in an Optional if called for.
    if (isOptional) {
      auto &optTL = SGF.getTypeLowering(errorPtrType);
      pointerValue = SGF.getOptionalSomeValue(loc, pointerValue, optTL);
    }

    return std::make_pair(managedErrorTemp, pointerValue);
  }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
//                            Result Plan Builder
//===----------------------------------------------------------------------===//

/// Build a result plan for the results of an apply.
///
/// If the initialization is non-null, the result plan will emit into it.
ResultPlanPtr ResultPlanBuilder::buildTopLevelResult(Initialization *init,
                                                     SILLocation loc) {
  // First check if we do not have a foreign error. If we don't, just call
  // build.
  auto foreignError = calleeTypeInfo.foreignError;
  if (!foreignError) {
    return build(init, calleeTypeInfo.origResultType.getValue(),
                 calleeTypeInfo.substResultType);
  }

  // Otherwise, handle the foreign error first.
  //
  // The plan needs to be built using the formal result type after foreign-error
  // adjustment.
  switch (foreignError->getKind()) {
  // These conventions make the formal result type ().
  case ForeignErrorConvention::ZeroResult:
  case ForeignErrorConvention::NonZeroResult:
    assert(calleeTypeInfo.substResultType->isVoid());
    allResults.clear();
    break;

  // These conventions leave the formal result alone.
  case ForeignErrorConvention::ZeroPreservedResult:
  case ForeignErrorConvention::NonNilError:
    break;

  // This convention changes the formal result to the optional object type; we
  // need to make our own make SILResultInfo array.
  case ForeignErrorConvention::NilResult: {
    assert(allResults.size() == 1);
    auto substFnTy = calleeTypeInfo.substFnType;
    CanType objectType = allResults[0].getReturnValueType(SGF.SGM.M, substFnTy)
                                      .getOptionalObjectType();
    SILResultInfo optResult = allResults[0].getWithInterfaceType(objectType);
    allResults.clear();
    allResults.push_back(optResult);
    break;
  }
  }

  ResultPlanPtr subPlan = build(init, calleeTypeInfo.origResultType.getValue(),
                                calleeTypeInfo.substResultType);
  return ResultPlanPtr(new ForeignErrorInitializationPlan(
      SGF, loc, calleeTypeInfo, std::move(subPlan)));
}

/// Build a result plan for the results of an apply.
///
/// If the initialization is non-null, the result plan will emit into it.
ResultPlanPtr ResultPlanBuilder::build(Initialization *init,
                                       AbstractionPattern origType,
                                       CanType substType) {
  // Destructure original tuples.
  if (origType.isTuple()) {
    return buildForTuple(init, origType, cast<TupleType>(substType));
  }

  // Otherwise, grab the next result.
  auto result = allResults.pop_back_val();

  auto calleeTy = calleeTypeInfo.substFnType;
  
  // If the result is indirect, and we have an address to emit into, and
  // there are no abstraction differences, then just do it.
  if (init && init->canPerformInPlaceInitialization() &&
      SGF.silConv.isSILIndirect(result) &&
      !SGF.getLoweredType(substType).getAddressType().hasAbstractionDifference(
            calleeTypeInfo.getOverrideRep(),
            result.getSILStorageType(SGF.SGM.M, calleeTy))) {
    return ResultPlanPtr(new InPlaceInitializationResultPlan(init));
  }

  // Otherwise, we need to:
  //   - get the value, either directly or indirectly
  //   - possibly reabstract it
  //   - store it to the destination
  // We could break this down into different ResultPlan implementations,
  // but it's easier not to.
  
  // If the result type involves an indirectly-returned opened existential,
  // then we need to evaluate the arguments first in order to have access to
  // the opened Self type. A special result plan defers allocating the stack
  // slot to the point the call is emitted.
  if (result.getReturnValueType(SGF.SGM.M, calleeTy)->hasOpenedExistential()
      && SGF.silConv.isSILIndirect(result)) {
    return ResultPlanPtr(
      new IndirectOpenedSelfResultPlan(SGF, origType, substType));
  }

  // Create a temporary if the result is indirect.
  std::unique_ptr<TemporaryInitialization> temporary;
  if (SGF.silConv.isSILIndirect(result)) {
    auto &resultTL = SGF.getTypeLowering(
                               result.getReturnValueType(SGF.SGM.M, calleeTy));
    temporary = SGF.emitTemporary(loc, resultTL);
  }

  return ResultPlanPtr(new ScalarResultPlan(
      std::move(temporary), origType, init, calleeTypeInfo.getOverrideRep()));
}

ResultPlanPtr ResultPlanBuilder::buildForTuple(Initialization *init,
                                               AbstractionPattern origType,
                                               CanTupleType substType) {
  // If we don't have an initialization for the tuple, just build the
  // individual components.
  if (!init) {
    return ResultPlanPtr(new TupleRValueResultPlan(*this, origType, substType));
  }

  // Okay, we have an initialization for the tuple that we need to emit into.

  // If we can just split the initialization, do so.
  if (init->canSplitIntoTupleElements()) {
    return ResultPlanPtr(
        new TupleInitializationResultPlan(*this, init, origType, substType));
  }

  // Otherwise, we're going to have to call copyOrInitValueInto, which only
  // takes a single value.

  // If the tuple is address-only, we'll get much better code if we
  // emit into a single buffer.
  auto &substTL = SGF.getTypeLowering(substType);
  if (substTL.isAddressOnly()) {
    // Create a temporary.
    auto temporary = SGF.emitTemporary(loc, substTL);

    // Build a sub-plan to emit into the temporary.
    auto subplan = buildForTuple(temporary.get(), origType, substType);

    // Make a plan to initialize into that.
    return ResultPlanPtr(new InitValueFromTemporaryResultPlan(
        init, std::move(subplan), std::move(temporary)));
  }

  // Build a sub-plan that doesn't know about the initialization.
  auto subplan = buildForTuple(nullptr, origType, substType);

  // Make a plan that calls copyOrInitValueInto.
  return ResultPlanPtr(
      new InitValueFromRValueResultPlan(init, std::move(subplan)));
}

ResultPlanPtr
ResultPlanBuilder::computeResultPlan(SILGenFunction &SGF,
                                     const CalleeTypeInfo &calleeTypeInfo,
                                     SILLocation loc, SGFContext evalContext) {
  ResultPlanBuilder builder(SGF, loc, calleeTypeInfo);
  return builder.buildTopLevelResult(evalContext.getEmitInto(), loc);
}
