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

/// Emit literals for the major, minor, and subminor components of the version
/// and return a tuple of SILValues for them.
static std::tuple<SILValue, SILValue, SILValue>
emitVersionLiterals(SILLocation loc, SILGenBuilder &B, ASTContext &ctx,
                    llvm::VersionTuple Vers) {
  unsigned major = Vers.getMajor();
  unsigned minor = Vers.getMinor().value_or(0);
  unsigned subminor = Vers.getSubminor().value_or(0);

  SILType wordType = SILType::getBuiltinWordType(ctx);

  SILValue majorValue = B.createIntegerLiteral(loc, wordType, major);
  SILValue minorValue = B.createIntegerLiteral(loc, wordType, minor);
  SILValue subminorValue = B.createIntegerLiteral(loc, wordType, subminor);

  return std::make_tuple(majorValue, minorValue, subminorValue);
}

/// Emit a check that returns 1 if the running OS version is in
/// the specified version range and 0 otherwise. The returned SILValue
/// (which has type Builtin.Int1) represents the result of this check.
static SILValue emitOSVersionRangeCheck(SILGenFunction &SGF, SILLocation loc,
                                        const VersionRange &range,
                                        bool forTargetVariant) {
  auto &ctx = SGF.getASTContext();
  auto &B = SGF.B;

  // Emit constants for the checked version range.
  SILValue majorValue;
  SILValue minorValue;
  SILValue subminorValue;

  std::tie(majorValue, minorValue, subminorValue) =
      emitVersionLiterals(loc, B, ctx, range.getLowerEndpoint());

  // Emit call to _stdlib_isOSVersionAtLeast(major, minor, patch)
  FuncDecl *versionQueryDecl = ctx.getIsOSVersionAtLeastDecl();

  // When targeting macCatalyst, the version number will be an iOS version
  // number and so we call a variant of the query function that understands iOS
  // versions.
  if (forTargetVariant)
    versionQueryDecl = ctx.getIsVariantOSVersionAtLeastDecl();

  assert(versionQueryDecl);

  auto declRef = SILDeclRef(versionQueryDecl);
  SILValue availabilityGTEFn = SGF.emitGlobalFunctionRef(
      loc, declRef,
      SGF.getConstantInfo(SGF.getTypeExpansionContext(), declRef));

  SILValue args[] = {majorValue, minorValue, subminorValue};
  return B.createApply(loc, availabilityGTEFn, SubstitutionMap(), args);
}

static SILValue
emitOSVersionOrVariantVersionRangeCheck(SILGenFunction &SGF, SILLocation loc,
                                        const VersionRange &targetRange,
                                        const VersionRange &variantRange) {
  auto &ctx = SGF.getASTContext();
  auto &B = SGF.B;

  SILValue targetMajorValue;
  SILValue targetMinorValue;
  SILValue targetSubminorValue;

  std::tie(targetMajorValue, targetMinorValue, targetSubminorValue) =
      emitVersionLiterals(loc, B, ctx, targetRange.getLowerEndpoint());

  SILValue variantMajorValue;
  SILValue variantMinorValue;
  SILValue variantSubminorValue;

  std::tie(variantMajorValue, variantMinorValue, variantSubminorValue) =
      emitVersionLiterals(loc, B, ctx, variantRange.getLowerEndpoint());

  FuncDecl *versionQueryDecl =
      ctx.getIsOSVersionAtLeastOrVariantVersionAtLeast();

  assert(versionQueryDecl);

  auto declRef = SILDeclRef(versionQueryDecl);
  SILValue availabilityGTEFn = SGF.emitGlobalFunctionRef(
      loc, declRef,
      SGF.getConstantInfo(SGF.getTypeExpansionContext(), declRef));

  SILValue args[] = {targetMajorValue,    targetMinorValue,
                     targetSubminorValue, variantMajorValue,
                     variantMinorValue,   variantSubminorValue};
  return B.createApply(loc, availabilityGTEFn, SubstitutionMap(), args);
}

SILValue emitZipperedOSVersionRangeCheck(SILGenFunction &SGF, SILLocation loc,
                                         const VersionRange &targetRange,
                                         const VersionRange &variantRange) {
  auto &ctx = SGF.getASTContext();
  assert(ctx.LangOpts.TargetVariant);

  VersionRange targetVersion = targetRange;
  VersionRange variantVersion = variantRange;

  // We're building zippered, so we need to pass both macOS and iOS versions to
  // the runtime version range check. At run time that check will determine what
  // kind of process this code is loaded into. In a macOS process it will use
  // the macOS version; in an macCatalyst process it will use the iOS version.
  llvm::Triple targetTriple = ctx.LangOpts.Target;
  llvm::Triple variantTriple = *ctx.LangOpts.TargetVariant;

  // From perspective of the driver and most of the frontend, -target and
  // -target-variant are symmetric. That is, the user can pass either:
  //    -target x86_64-apple-macosx10.15 \
  //    -target-variant x86_64-apple-ios13.1-macabi
  // or:
  //    -target x86_64-apple-ios13.1-macabi \
  //    -target-variant x86_64-apple-macosx10.15
  //
  // However, the runtime availability-checking entry points need to compare
  // against an actual running OS version and so can't be symmetric. Here we
  // standardize on "target" means macOS version and "targetVariant" means iOS
  // version.
  if (tripleIsMacCatalystEnvironment(targetTriple)) {
    assert(variantTriple.isMacOSX());
    // Normalize so that "variant" always means iOS version.
    std::swap(targetVersion, variantVersion);
    std::swap(targetTriple, variantTriple);
  }

  DEBUG_ASSERT(targetVersion.hasLowerEndpoint() ||
               variantVersion.hasLowerEndpoint());

  // The variant-only availability-checking entrypoint is not part of the
  // Swift 5.0 ABI. It is only available in macOS 10.15 and above.
  bool isVariantEntrypointAvailable = !targetTriple.isMacOSXVersionLT(10, 15);

  // If there is no check for the target but there is for the variant, then we
  // only need to emit code for the variant check.
  if (isVariantEntrypointAvailable && targetVersion.isAll() &&
      !variantVersion.isAll())
    return emitOSVersionRangeCheck(SGF, loc, variantVersion,
                                   /*forVariant*/ true);

  // Similarly, if there is a check for the target but not for the target
  // variant then we only to emit code for the target check.
  if (!targetVersion.isAll() && variantVersion.isAll())
    return emitOSVersionRangeCheck(SGF, loc, targetVersion,
                                   /*forVariant*/ false);

  if (!isVariantEntrypointAvailable ||
      (!targetVersion.isAll() && !variantVersion.isAll())) {

    // If the variant-only entrypoint isn't available (as is the case
    // pre-macOS 10.15) we need to use the zippered entrypoint (which is part of
    // the Swift 5.0 ABI) even when the macOS version  is '*' (all). In this
    // case, use the minimum macOS deployment version from the target triple.
    // This ensures the check always passes on macOS.
    if (!isVariantEntrypointAvailable && targetVersion.isAll()) {
      assert(targetTriple.isMacOSX());

      llvm::VersionTuple macosVersion;
      targetTriple.getMacOSXVersion(macosVersion);
      targetVersion = VersionRange::allGTE(macosVersion);
    }

    return emitOSVersionOrVariantVersionRangeCheck(SGF, loc, targetVersion,
                                                   variantVersion);
  }

  llvm_unreachable("Unhandled zippered configuration");
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

static SILValue emitZipperedBackDeployIfAvailableBooleanTestValue(
    SILGenFunction &SGF, AbstractFunctionDecl *AFD, SILLocation loc,
    SILBasicBlock *availableBB, SILBasicBlock *unavailableBB) {
  auto &ctx = SGF.getASTContext();
  assert(ctx.LangOpts.TargetVariant);

  VersionRange OSVersion = VersionRange::all();
  if (auto version = AFD->getBackDeployedBeforeOSVersion(ctx)) {
    OSVersion = VersionRange::allGTE(*version);
  }

  VersionRange VariantOSVersion = VersionRange::all();
  if (auto version =
          AFD->getBackDeployedBeforeOSVersion(ctx, /*forTargetVariant=*/true)) {
    VariantOSVersion = VersionRange::allGTE(*version);
  }

  return emitZipperedOSVersionRangeCheck(SGF, loc, OSVersion, VariantOSVersion);
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
  if (SGF.getASTContext().LangOpts.TargetVariant) {
    SILValue booleanTestValue =
        emitZipperedBackDeployIfAvailableBooleanTestValue(
            SGF, AFD, loc, availableBB, unavailableBB);
    SGF.B.createCondBranch(loc, booleanTestValue, availableBB, unavailableBB);
    return;
  }

  auto version = AFD->getBackDeployedBeforeOSVersion(SGF.SGM.getASTContext());
  VersionRange OSVersion = VersionRange::empty();
  if (version.has_value()) {
    OSVersion = VersionRange::allGTE(*version);
  }

  SILValue booleanTestValue;
  if (OSVersion.isEmpty() || OSVersion.isAll()) {
    // If there's no check for the current platform, this condition is
    // trivially true.
    SILType i1 = SILType::getBuiltinIntegerType(1, SGF.getASTContext());
    booleanTestValue = SGF.B.createIntegerLiteral(loc, i1, 1);
  } else {
    bool isMacCatalyst =
        tripleIsMacCatalystEnvironment(SGF.getASTContext().LangOpts.Target);
    booleanTestValue =
        emitOSVersionRangeCheck(SGF, loc, OSVersion, isMacCatalyst);
  }

  SGF.B.createCondBranch(loc, booleanTestValue, availableBB, unavailableBB);
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

  // The query might not have been computed by Sema if availability checking
  // was disabled. Otherwise, there's a bug in Sema.
  DEBUG_ASSERT(query || ctx.LangOpts.DisableAvailabilityChecking);

  // Treat the condition as if it always evaluates true if Sema skipped it.
  if (!query)
    return B.createIntegerLiteral(loc, i1, !availability->isUnavailability());

  // If Sema indicated that the query's result is known at compile time, emit
  // a constant.
  if (auto constantResult = query->getConstantResult())
    return B.createIntegerLiteral(loc, i1, *constantResult);

  // FIXME: [availability] Add support for custom domain queries.
  auto domain = query->getDomain();
  if (!domain.isPlatform())
    return B.createIntegerLiteral(loc, i1, !availability->isUnavailability());

  SILValue result;
  auto primaryRange =
      query->getPrimaryRange().value_or(AvailabilityRange::alwaysAvailable());

  if (ctx.LangOpts.TargetVariant) {
    auto variantRange =
        query->getVariantRange().value_or(AvailabilityRange::alwaysAvailable());

    // We're building zippered, so we need to pass both macOS and iOS versions
    // to the the runtime version range check. At run time that check will
    // determine what kind of process this code is loaded into. In a macOS
    // process it will use the macOS version; in an macCatalyst process it will
    // use the iOS version.
    result = emitZipperedOSVersionRangeCheck(*this, loc,
                                             primaryRange.getRawVersionRange(),
                                             variantRange.getRawVersionRange());
  } else {
    bool isMacCatalyst = tripleIsMacCatalystEnvironment(ctx.LangOpts.Target);
    result = emitOSVersionRangeCheck(
        *this, loc, primaryRange.getRawVersionRange(), isMacCatalyst);
  }

  // If this is an unavailability check, invert the result using xor with -1.
  if (query->isUnavailability()) {
    SILType i1 = SILType::getBuiltinIntegerType(1, ctx);
    SILValue minusOne = B.createIntegerLiteral(loc, i1, -1);
    result =
        B.createBuiltinBinaryFunction(loc, "xor", i1, i1, {result, minusOne});
  }

  return result;
}

bool SILGenModule::requiresBackDeploymentThunk(ValueDecl *decl,
                                               ResilienceExpansion expansion) {
  auto &ctx = getASTContext();
  auto backDeployBeforeVersion = decl->getBackDeployedBeforeOSVersion(ctx);
  if (!backDeployBeforeVersion)
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
  auto declAvailability = AvailabilityRange(*backDeployBeforeVersion);
  if (deploymentAvailability.isContainedIn(declAvailability))
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
  collectThunkParams(loc, params, &indirectParams, &indirectErrorResults);

  // Build up the list of arguments that we're going to invoke the real
  // function with.
  SmallVector<SILValue, 8> paramsForForwarding;
  for (auto indirectParam : indirectParams) {
    paramsForForwarding.emplace_back(indirectParam.getLValueAddress());
  }
  for (auto indirectErrorResult : indirectErrorResults) {
    paramsForForwarding.emplace_back(indirectErrorResult.getLValueAddress());
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
