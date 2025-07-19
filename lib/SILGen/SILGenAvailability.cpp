//===--- SILGenAvailability.cpp - SILGen for availability queries ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SILGenFunction.h"
#include "SILGenFunctionBuilder.h"
#include "Scope.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Platform.h"
#include "swift/SIL/SILDeclRef.h"

using namespace swift;
using namespace Lowering;

/// Emit literals for each of element in `values`, populating `results` with the
/// corresponding `SILValue`.
static void emitIntegerLiterals(SILLocation loc, SILGenBuilder &B,
                                ASTContext &ctx, ArrayRef<unsigned> values,
                                llvm::SmallVectorImpl<SILValue> &results) {

  SILType wordType = SILType::getBuiltinWordType(ctx);
  for (auto value : values) {
    results.emplace_back(B.createIntegerLiteral(loc, wordType, value));
  }
}

static SILValue emitAvailabilityCheck(SILGenFunction &SGF, SILLocation loc,
                                      const AvailabilityQuery &query) {
  auto &ctx = SGF.getASTContext();
  SILType i1 = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
  auto &B = SGF.B;

  // If the query's result is known at compile time, emit a constant.
  if (auto constantResult = query.getConstantResult())
    return B.createIntegerLiteral(loc, i1, *constantResult);

  llvm::SmallVector<unsigned, 6> rawArgs;
  auto queryDecl = query.getDynamicQueryDeclAndArguments(rawArgs, ctx);

  // FIXME: [availability] Once dynamic custom domains have decls, DEBUG_ASSERT.
  if (!queryDecl)
    return B.createIntegerLiteral(loc, i1, !query.isUnavailability());

  auto queryType = queryDecl->getInterfaceType()->getAs<FunctionType>();
  ASSERT(queryType);
  ASSERT(queryType->getResult()->getAs<BuiltinIntegerType>());

  auto queryParams = queryType->getParams();
  ASSERT(rawArgs.size() == queryParams.size());

  llvm::SmallVector<SILValue, 6> silArgs;
  emitIntegerLiterals(loc, B, ctx, rawArgs, silArgs);

  auto declRef = SILDeclRef(queryDecl);
  SILValue availabilityGTEFn = SGF.emitGlobalFunctionRef(
      loc, declRef,
      SGF.getConstantInfo(SGF.getTypeExpansionContext(), declRef));

  SILValue result =
      B.createApply(loc, availabilityGTEFn, SubstitutionMap(), silArgs);

  // If this is an unavailability check, invert the result using xor with -1.
  if (query.isUnavailability()) {
    SILType i1 = SILType::getBuiltinIntegerType(1, ctx);
    SILValue minusOne = B.createIntegerLiteral(loc, i1, -1);
    result =
        B.createBuiltinBinaryFunction(loc, "xor", i1, i1, {result, minusOne});
  }

  return result;
}

/// Given a value, extracts all elements to `result` from this value if it's a
/// tuple. Otherwise, add this value directly to `result`.
static void extractAllElements(SILValue val, SILLocation loc,
                               SILBuilder &builder,
                               SmallVectorImpl<SILValue> &result) {
  auto &fn = builder.getFunction();
  auto tupleType = val->getType().getAs<TupleType>();
  if (!tupleType) {
    result.push_back(val);
    return;
  }
  if (!fn.hasOwnership()) {
    for (auto i : range(tupleType->getNumElements()))
      result.push_back(builder.createTupleExtract(loc, val, i));
    return;
  }
  if (tupleType->getNumElements() == 0)
    return;
  builder.emitDestructureValueOperation(loc, val, result);
}

static Type getResultInterfaceType(AbstractFunctionDecl *AFD) {
  if (auto *FD = dyn_cast<FuncDecl>(AFD))
    return FD->getResultInterfaceType();

  if (auto *CD = dyn_cast<ConstructorDecl>(AFD))
    return CD->getResultInterfaceType();

  llvm_unreachable("Unhandled AbstractFunctionDecl type");
}

static std::optional<AvailabilityQuery>
getAvailabilityQueryForBackDeployment(AbstractFunctionDecl *AFD) {
  auto &ctx = AFD->getASTContext();
  if (ctx.LangOpts.TargetVariant) {
    auto primaryAttrAndRange = AFD->getBackDeployedAttrAndRange(ctx);
    auto variantAttrAndRange =
        AFD->getBackDeployedAttrAndRange(ctx, /*forTargetVariant=*/true);

    if (!primaryAttrAndRange && !variantAttrAndRange)
      return std::nullopt;

    auto attr = primaryAttrAndRange ? primaryAttrAndRange->first
                                    : variantAttrAndRange->first;
    std::optional<AvailabilityRange> primaryRange;
    if (primaryAttrAndRange)
      primaryRange = primaryAttrAndRange->second;

    std::optional<AvailabilityRange> variantRange;
    if (variantAttrAndRange)
      variantRange = variantAttrAndRange->second;

    return AvailabilityQuery::dynamic(attr->getAvailabilityDomain(),
                                      /*isUnavailable=*/false, primaryRange,
                                      variantRange);
  }

  if (auto primaryAttrAndRange = AFD->getBackDeployedAttrAndRange(ctx))
    return AvailabilityQuery::dynamic(
        primaryAttrAndRange->first->getAvailabilityDomain(),
        /*isUnavailable=*/false, primaryAttrAndRange->second, std::nullopt);

  return std::nullopt;
}

/// Emit the following branch SIL instruction:
/// \verbatim
/// if #available(OSVersion) {
///   <availableBB>
/// } else {
///   <unavailableBB>
/// }
/// \endverbatim
static void emitBackDeployIfAvailableCondition(SILGenFunction &SGF,
                                               AbstractFunctionDecl *AFD,
                                               SILLocation loc,
                                               SILBasicBlock *availableBB,
                                               SILBasicBlock *unavailableBB) {
  auto &B = SGF.B;
  SILValue booleanTestValue;
  if (auto query = getAvailabilityQueryForBackDeployment(AFD)) {
    booleanTestValue = emitAvailabilityCheck(SGF, loc, *query);
  } else {
    // Some logic has gone wrong if we've gotten here but lets fail gracefully
    // and emit a branch that always succeeds.
    SILType i1 = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
    booleanTestValue = B.createIntegerLiteral(loc, i1, true);
  }
  B.createCondBranch(loc, booleanTestValue, availableBB, unavailableBB);
}

/// Emits a function or method application, forwarding parameters.
static void emitBackDeployForwardApplyAndReturnOrThrow(
    SILGenFunction &SGF, AbstractFunctionDecl *AFD, SILLocation loc,
    SILDeclRef function, SmallVector<SILValue, 8> &params) {
  // Only statically dispatched class methods are supported.
  if (auto classDecl = dyn_cast<ClassDecl>(AFD->getDeclContext())) {
    assert(classDecl->isFinal() || AFD->isFinal() ||
           AFD->hasForcedStaticDispatch());
  }

  TypeExpansionContext TEC = SGF.getTypeExpansionContext();
  auto fnType = SGF.SGM.Types.getConstantOverrideType(TEC, function);
  auto silFnType =
      SILType::getPrimitiveObjectType(fnType).castTo<SILFunctionType>();
  SILFunctionConventions fnConv(silFnType, SGF.SGM.M);

  SILValue functionRef = SGF.emitGlobalFunctionRef(loc, function);
  auto subs = SGF.F.getForwardingSubstitutionMap();
  SmallVector<SILValue, 4> directResults;

  // If the function is a coroutine, we need to use 'begin_apply'.
  if (silFnType->isCoroutine()) {
    assert(!silFnType->hasErrorResult() && "throwing coroutine?");

    // Apply the coroutine, yield the result, and finally branch to either the
    // terminal return or unwind basic block via intermediate basic blocks. The
    // intermediates are needed to avoid forming critical edges.
    SILBasicBlock *resumeBB = SGF.createBasicBlock();
    SILBasicBlock *unwindBB = SGF.createBasicBlock();

    auto *apply = SGF.B.createBeginApply(loc, functionRef, subs, params);
    SmallVector<SILValue, 4> rawResults;
    for (auto result : apply->getAllResults())
      rawResults.push_back(result);

    auto token = rawResults.pop_back_val();
    SGF.B.createYield(loc, rawResults, resumeBB, unwindBB);

    // Emit resume block.
    SGF.B.emitBlock(resumeBB);
    SGF.B.createEndApply(loc, token,
                         SILType::getEmptyTupleType(SGF.getASTContext()));
    SGF.B.createBranch(loc, SGF.ReturnDest.getBlock());

    // Emit unwind block.
    SGF.B.emitBlock(unwindBB);
    SGF.B.createEndApply(loc, token,
                         SILType::getEmptyTupleType(SGF.getASTContext()));
    SGF.B.createBranch(loc, SGF.CoroutineUnwindDest.getBlock());
    return;
  }

  // Use try_apply for functions that throw.
  if (silFnType->hasErrorResult()) {
    // Apply the throwing function and forward the results and the error to the
    // return/throw blocks via intermediate basic blocks. The intermediates
    // are needed to avoid forming critical edges.
    SILBasicBlock *normalBB = SGF.createBasicBlock();
    SILBasicBlock *errorBB = SGF.createBasicBlock();

    SGF.B.createTryApply(loc, functionRef, subs, params, normalBB, errorBB);

    // Emit error block.
    SGF.B.emitBlock(errorBB);
    ManagedValue error =
        SGF.B.createPhi(SGF.F.mapTypeIntoContext(fnConv.getSILErrorType(TEC)),
                        OwnershipKind::Owned);
    SGF.B.createBranch(loc, SGF.ThrowDest.getBlock(), {error});

    // Emit normal block.
    SGF.B.emitBlock(normalBB);
    SILValue result = normalBB->createPhiArgument(
        SGF.F.mapTypeIntoContext(fnConv.getSILResultType(TEC)),
        OwnershipKind::Owned);
    SmallVector<SILValue, 4> directResults;
    extractAllElements(result, loc, SGF.B, directResults);

    SGF.B.createBranch(loc, SGF.ReturnDest.getBlock(), directResults);
    return;
  }

  // The original function is neither throwing nor a coroutine. Apply it and
  // forward its results straight to the return block.
  auto *apply = SGF.B.createApply(loc, functionRef, subs, params);
  extractAllElements(apply, loc, SGF.B, directResults);

  SGF.B.createBranch(loc, SGF.ReturnDest.getBlock(), directResults);
}

SILValue
SILGenFunction::emitIfAvailableQuery(SILLocation loc,
                                     PoundAvailableInfo *availability) {
  auto &ctx = getASTContext();
  SILType i1 = SILType::getBuiltinIntegerType(1, ctx);
  auto query = availability->getAvailabilityQuery();

  // The query may not have been computed by Sema under the following
  // conditions:
  // - Availability checking was disabled (-disable-availabilty-checking).
  // - The query was marked invalid in the AST for a non-fatal reason.
  //
  // Otherwise, there's a bug in Sema.
  DEBUG_ASSERT(query || availability->isInvalid() ||
               ctx.LangOpts.DisableAvailabilityChecking);

  // If Sema skipped the query then treat the condition as if it always
  // evaluates true.
  if (!query)
    return B.createIntegerLiteral(loc, i1, !availability->isUnavailability());

  return emitAvailabilityCheck(*this, loc, *query);
}

bool SILGenModule::requiresBackDeploymentThunk(ValueDecl *decl,
                                               ResilienceExpansion expansion) {
  auto &ctx = getASTContext();
  auto attrAndRange = decl->getBackDeployedAttrAndRange(ctx);
  if (!attrAndRange)
    return false;

  switch (expansion) {
  case ResilienceExpansion::Minimal:
    // In a minimal resilience expansion we must always call the back deployment
    // thunk since we can't predict the deployment targets of the modules that
    // might inline the call.
    return true;
  case ResilienceExpansion::Maximal:
    // FIXME: We can skip thunking if we're in the same module.
    break;
  }

  // Use of a back deployment thunk is unnecessary if the deployment target is
  // high enough that the ABI implementation of the back deployed declaration is
  // guaranteed to be available.
  auto deploymentAvailability = AvailabilityRange::forDeploymentTarget(ctx);
  if (deploymentAvailability.isContainedIn(attrAndRange->second))
    return false;

  return true;
}

void SILGenFunction::emitBackDeploymentThunk(SILDeclRef thunk) {
  // Generate code equivalent to:
  //
  //  func X_thunk(...) async throws -> ... {
  //    if #available(...) {
  //      return try await X(...)
  //    } else {
  //      return try await X_fallback(...)
  //    }
  //  }

  assert(thunk.isBackDeploymentThunk());

  auto loc = thunk.getAsRegularLocation();
  loc.markAutoGenerated();
  Scope scope(Cleanups, CleanupLocation(loc));
  auto AFD = cast<AbstractFunctionDecl>(thunk.getDecl());

  F.setGenericEnvironment(SGM.Types.getConstantGenericEnvironment(thunk));

  // Generate the thunk prolog by collecting parameters.
  SmallVector<ManagedValue, 4> params;
  SmallVector<ManagedValue, 4> indirectParams;
  SmallVector<ManagedValue, 4> indirectErrorResults;
  ManagedValue implicitIsolationParam;
  collectThunkParams(loc, params, &indirectParams, &indirectErrorResults,
                     &implicitIsolationParam);

  // Build up the list of arguments that we're going to invoke the real
  // function with.
  SmallVector<SILValue, 8> paramsForForwarding;
  for (auto indirectParam : indirectParams) {
    paramsForForwarding.emplace_back(indirectParam.getLValueAddress());
  }
  for (auto indirectErrorResult : indirectErrorResults) {
    paramsForForwarding.emplace_back(indirectErrorResult.getLValueAddress());
  }
  if (implicitIsolationParam) {
    paramsForForwarding.emplace_back(implicitIsolationParam.forward(*this));
  }

  for (auto param : params) {
    // We're going to directly call either the original function or the fallback
    // function with these arguments and then return. Therefore we just forward
    // the arguments instead of handling their ownership conventions.
    paramsForForwarding.emplace_back(param.forward(*this));
  }

  prepareEpilog(AFD,
                getResultInterfaceType(AFD),
                AFD->getEffectiveThrownErrorType(),
                CleanupLocation(AFD));

  SILBasicBlock *availableBB = createBasicBlock("availableBB");
  SILBasicBlock *unavailableBB = createBasicBlock("unavailableBB");

  //  if #available(...) {
  //    <availableBB>
  //  } else {
  //    <unavailableBB>
  //  }
  emitBackDeployIfAvailableCondition(*this, AFD, loc, availableBB,
                                     unavailableBB);

  // <availableBB>:
  //   return (try)? (await)? (self.)?X(...)
  {
    B.emitBlock(availableBB);
    SILDeclRef original =
        thunk.asBackDeploymentKind(SILDeclRef::BackDeploymentKind::None);
    emitBackDeployForwardApplyAndReturnOrThrow(*this, AFD, loc, original,
                                               paramsForForwarding);
  }

  // <unavailableBB>:
  //   return (try)? (await)? (self.)?X_fallback(...)
  {
    B.emitBlock(unavailableBB);
    SILDeclRef fallback =
        thunk.asBackDeploymentKind(SILDeclRef::BackDeploymentKind::Fallback);
    emitBackDeployForwardApplyAndReturnOrThrow(*this, AFD, loc, fallback,
                                               paramsForForwarding);
  }

  emitEpilog(AFD);
}
