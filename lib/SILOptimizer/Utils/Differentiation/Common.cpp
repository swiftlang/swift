//===--- Differentiation.cpp - SIL Automatic Differentiation --*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
// Automatic differentiation utilities.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/Differentiation/Common.h"

namespace swift {
namespace autodiff {

raw_ostream &getADDebugStream() { return llvm::dbgs() << "[AD] "; }

bool isArrayLiteralIntrinsic(ApplyInst *ai) {
  return ai->hasSemantics("array.uninitialized_intrinsic");
}

ApplyInst *getAllocateUninitializedArrayIntrinsic(SILValue v) {
  if (auto *ai = dyn_cast<ApplyInst>(v))
    if (isArrayLiteralIntrinsic(ai))
      return ai;
  return nullptr;
}

ApplyInst *
getAllocateUninitializedArrayIntrinsicElementAddress(SILValue v) {
  // Find the `pointer_to_address` result, peering through `index_addr`.
  auto *ptai = dyn_cast<PointerToAddressInst>(v);
  if (auto *iai = dyn_cast<IndexAddrInst>(v))
    ptai = dyn_cast<PointerToAddressInst>(iai->getOperand(0));
  if (!ptai)
    return nullptr;
  // Return the `array.uninitialized_intrinsic` application, if it exists.
  if (auto *dti = dyn_cast<DestructureTupleInst>(
          ptai->getOperand()->getDefiningInstruction())) {
    if (auto *ai = getAllocateUninitializedArrayIntrinsic(dti->getOperand()))
      return ai;
  }
  return nullptr;
}

DestructureTupleInst *getSingleDestructureTupleUser(SILValue value) {
  bool foundDestructureTupleUser = false;
  if (!value->getType().is<TupleType>())
    return nullptr;
  DestructureTupleInst *result = nullptr;
  for (auto *use : value->getUses()) {
    if (auto *dti = dyn_cast<DestructureTupleInst>(use->getUser())) {
      assert(!foundDestructureTupleUser &&
             "There should only be one `destructure_tuple` user of a tuple");
      foundDestructureTupleUser = true;
      result = dti;
    }
  }
  return result;
}

void forEachApplyDirectResult(
    ApplyInst *ai, llvm::function_ref<void(SILValue)> resultCallback) {
  if (!ai->getType().is<TupleType>()) {
    resultCallback(ai);
    return;
  }
  if (auto *dti = getSingleDestructureTupleUser(ai))
    for (auto result : dti->getResults())
      resultCallback(result);
}

/// Returns the canonical derivative generic signature for the given witness
/// and original function.
/// - Return the witness derivative generic signature if it exists.
/// - Otherwise, return the original function's generic signature.
CanGenericSignature
getDerivativeGenericSignature(SILDifferentiabilityWitness *witness,
                              SILFunction *original) {
  if (auto sig = witness->getDerivativeGenericSignature())
    return sig->getCanonicalSignature();
  return original->getLoweredFunctionType()->getSubstGenericSignature();
}

void collectAllFormalResultsInTypeOrder(SILFunction &function,
                                        SmallVectorImpl<SILValue> &results) {
  SILFunctionConventions convs(function.getLoweredFunctionType(),
                               function.getModule());
  auto indResults = function.getIndirectResults();
  auto *retInst = cast<ReturnInst>(function.findReturnBB()->getTerminator());
  auto retVal = retInst->getOperand();
  SmallVector<SILValue, 8> dirResults;
  if (auto *tupleInst =
          dyn_cast_or_null<TupleInst>(retVal->getDefiningInstruction()))
    dirResults.append(tupleInst->getElements().begin(),
                      tupleInst->getElements().end());
  else
    dirResults.push_back(retVal);
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : convs.getResults())
    results.push_back(resInfo.isFormalDirect() ? dirResults[dirResIdx++]
                                               : indResults[indResIdx++]);
}

/// Given a function, gathers all of its direct results in an order defined by
/// its result type. Note that "formal results" refer to result values in the
/// body of the function, not at call sites.
void collectAllDirectResultsInTypeOrder(SILFunction &function,
                                        SmallVectorImpl<SILValue> &results) {
  SILFunctionConventions convs(function.getLoweredFunctionType(),
                               function.getModule());
  auto *retInst = cast<ReturnInst>(function.findReturnBB()->getTerminator());
  auto retVal = retInst->getOperand();
  if (auto *tupleInst = dyn_cast<TupleInst>(retVal))
    results.append(tupleInst->getElements().begin(),
                   tupleInst->getElements().end());
  else
    results.push_back(retVal);
}

/// Given a function call site, gathers all of its actual results (both direct
/// and indirect) in an order defined by its result type.
void collectAllActualResultsInTypeOrder(
    ApplyInst *ai, ArrayRef<SILValue> extractedDirectResults,
    SmallVectorImpl<SILValue> &results) {
  auto calleeConvs = ai->getSubstCalleeConv();
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : calleeConvs.getResults()) {
    results.push_back(resInfo.isFormalDirect()
                          ? extractedDirectResults[dirResIdx++]
                          : ai->getIndirectSILResults()[indResIdx++]);
  }
}

void collectMinimalIndicesForFunctionCall(
    ApplyInst *ai, SILAutoDiffIndices parentIndices,
    const DifferentiableActivityInfo &activityInfo,
    SmallVectorImpl<SILValue> &results, SmallVectorImpl<unsigned> &paramIndices,
    SmallVectorImpl<unsigned> &resultIndices) {
  auto calleeFnTy = ai->getSubstCalleeType();
  auto calleeConvs = ai->getSubstCalleeConv();
  // Parameter indices are indices (in the callee type signature) of parameter
  // arguments that are varied or are arguments.
  // Record all parameter indices in type order.
  unsigned currentParamIdx = 0;
  for (auto applyArg : ai->getArgumentsWithoutIndirectResults()) {
    if (activityInfo.isActive(applyArg, parentIndices))
      paramIndices.push_back(currentParamIdx);
    ++currentParamIdx;
  }
  // Result indices are indices (in the callee type signature) of results that
  // are useful.
  SmallVector<SILValue, 8> directResults;
  forEachApplyDirectResult(ai, [&](SILValue directResult) {
    directResults.push_back(directResult);
  });
  auto indirectResults = ai->getIndirectSILResults();
  // Record all results and result indices in type order.
  results.reserve(calleeFnTy->getNumResults());
  unsigned dirResIdx = 0;
  unsigned indResIdx = calleeConvs.getSILArgIndexOfFirstIndirectResult();
  for (auto &resAndIdx : enumerate(calleeConvs.getResults())) {
    auto &res = resAndIdx.value();
    unsigned idx = resAndIdx.index();
    if (res.isFormalDirect()) {
      results.push_back(directResults[dirResIdx]);
      if (auto dirRes = directResults[dirResIdx])
        if (dirRes && activityInfo.isActive(dirRes, parentIndices))
          resultIndices.push_back(idx);
      ++dirResIdx;
    } else {
      results.push_back(indirectResults[indResIdx]);
      if (activityInfo.isActive(indirectResults[indResIdx], parentIndices))
        resultIndices.push_back(idx);
      ++indResIdx;
    }
  }
  // Make sure the function call has active results.
  assert(results.size() == calleeFnTy->getNumResults());
  assert(llvm::any_of(results, [&](SILValue result) {
    return activityInfo.isActive(result, parentIndices);
  }));
}

void emitZeroIntoBuffer(SILBuilder &builder, CanType type,
                        SILValue bufferAccess, SILLocation loc) {
  auto &astCtx = builder.getASTContext();
  auto *swiftMod = builder.getModule().getSwiftModule();
  auto &typeConverter = builder.getModule().Types;
  // Look up conformance to `AdditiveArithmetic`.
  auto *additiveArithmeticProto =
      astCtx.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto confRef = swiftMod->lookupConformance(type, additiveArithmeticProto);
  assert(!confRef.isInvalid() && "Missing conformance to `AdditiveArithmetic`");
  // Look up `AdditiveArithmetic.zero.getter`.
  auto zeroDeclLookup = additiveArithmeticProto->lookupDirect(astCtx.Id_zero);
  auto *zeroDecl = cast<VarDecl>(zeroDeclLookup.front());
  assert(zeroDecl->isProtocolRequirement());
  auto *accessorDecl = zeroDecl->getAccessor(AccessorKind::Get);
  SILDeclRef accessorDeclRef(accessorDecl, SILDeclRef::Kind::Func);
  auto silFnType = typeConverter.getConstantType(
      TypeExpansionContext::minimal(), accessorDeclRef);
  // %wm = witness_method ...
  auto *getter = builder.createWitnessMethod(
      loc, type, confRef, accessorDeclRef, silFnType);
  // %metatype = metatype $T
  auto metatypeType = CanMetatypeType::get(
      type, MetatypeRepresentation::Thick);
  auto metatype = builder.createMetatype(
      loc, SILType::getPrimitiveObjectType(metatypeType));
  auto subMap = SubstitutionMap::getProtocolSubstitutions(
      additiveArithmeticProto, type, confRef);
  builder.createApply(loc, getter, subMap, {bufferAccess, metatype},
                      /*isNonThrowing*/ false);
  builder.emitDestroyValueOperation(loc, getter);
}

//===----------------------------------------------------------------------===//
// Thunk helpers
//===----------------------------------------------------------------------===//
// These helpers are copied/adapted from SILGen. They should be refactored and
// moved to a shared location.
//===----------------------------------------------------------------------===//

CanGenericSignature buildThunkSignature(SILFunction *fn, bool inheritGenericSig,
                                        OpenedArchetypeType *openedExistential,
                                        GenericEnvironment *&genericEnv,
                                        SubstitutionMap &contextSubs,
                                        SubstitutionMap &interfaceSubs,
                                        ArchetypeType *&newArchetype) {
  // If there's no opened existential, we just inherit the generic environment
  // from the parent function.
  if (openedExistential == nullptr) {
    auto genericSig = fn->getLoweredFunctionType()->getSubstGenericSignature();
    genericEnv = fn->getGenericEnvironment();
    interfaceSubs = fn->getForwardingSubstitutionMap();
    contextSubs = interfaceSubs;
    return genericSig;
  }

  auto &ctx = fn->getASTContext();
  GenericSignatureBuilder builder(ctx);

  // Add the existing generic signature.
  int depth = 0;
  if (inheritGenericSig) {
    if (auto genericSig =
            fn->getLoweredFunctionType()->getSubstGenericSignature()) {
      builder.addGenericSignature(genericSig);
      depth = genericSig->getGenericParams().back()->getDepth() + 1;
    }
  }

  // Add a new generic parameter to replace the opened existential.
  auto *newGenericParam = GenericTypeParamType::get(depth, 0, ctx);

  builder.addGenericParameter(newGenericParam);
  Requirement newRequirement(RequirementKind::Conformance, newGenericParam,
                             openedExistential->getOpenedExistentialType());
  auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  builder.addRequirement(newRequirement, source, nullptr);

  auto genericSig = std::move(builder).computeGenericSignature(
      SourceLoc(), /*allowConcreteGenericParams=*/true);
  genericEnv = genericSig->getGenericEnvironment();

  newArchetype = genericEnv->mapTypeIntoContext(newGenericParam)
      ->castTo<ArchetypeType>();

  // Calculate substitutions to map the caller's archetypes to the thunk's
  // archetypes.
  if (auto calleeGenericSig =
          fn->getLoweredFunctionType()->getSubstGenericSignature()) {
    contextSubs = SubstitutionMap::get(
        calleeGenericSig,
        [&](SubstitutableType *type) -> Type {
          return genericEnv->mapTypeIntoContext(type);
        },
        MakeAbstractConformanceForGenericType());
  }

  // Calculate substitutions to map interface types to the caller's archetypes.
  interfaceSubs = SubstitutionMap::get(
      genericSig,
      [&](SubstitutableType *type) -> Type {
        if (type->isEqual(newGenericParam))
          return openedExistential;
        return fn->mapTypeIntoContext(type);
      },
      MakeAbstractConformanceForGenericType());

  return genericSig->getCanonicalSignature();
}

CanSILFunctionType buildThunkType(SILFunction *fn,
                                  CanSILFunctionType &sourceType,
                                  CanSILFunctionType &expectedType,
                                  GenericEnvironment *&genericEnv,
                                  SubstitutionMap &interfaceSubs,
                                  bool withoutActuallyEscaping,
                                  DifferentiationThunkKind thunkKind) {
  assert(!expectedType->isPolymorphic());
  assert(!sourceType->isPolymorphic());

  auto &module = fn->getModule();
  auto origType = sourceType;

  // Cannot build a reabstraction thunk without context. Ownership semantics
  // on the result type are required.
  if (thunkKind == DifferentiationThunkKind::Reabstraction)
    assert(expectedType->getExtInfo().hasContext());

  // This may inherit @noescape from the expected type. The `@noescape`
  // attribute is only stripped when using this type to materialize a new decl.
  // Use `@convention(thin)` if:
  // - Building a reabstraction thunk type.
  // - Building an index subset thunk type, where the expected type has context
  //   (i.e. is `@convention(thick)`).
  auto extInfo = expectedType->getExtInfo();
  if (thunkKind == DifferentiationThunkKind::Reabstraction ||
      extInfo.hasContext()) {
    extInfo = extInfo.withRepresentation(
        SILFunctionType::Representation::Thin);
  }
  if (withoutActuallyEscaping)
    extInfo = extInfo.withNoEscape(false);

  // Does the thunk type involve archetypes other than opened existentials?
  bool hasArchetypes = false;
  // Does the thunk type involve an open existential type?
  CanOpenedArchetypeType openedExistential;
  auto archetypeVisitor = [&](CanType t) {
    if (auto archetypeTy = dyn_cast<OpenedArchetypeType>(t)) {
      if (auto opened = dyn_cast<OpenedArchetypeType>(archetypeTy)) {
        assert((openedExistential == CanArchetypeType() ||
                openedExistential == opened) &&
               "one too many open existentials");
        openedExistential = opened;
      } else {
        hasArchetypes = true;
      }
    }
  };

  // Use the generic signature from the context if the thunk involves
  // generic parameters.
  CanGenericSignature genericSig;
  SubstitutionMap contextSubs;
  ArchetypeType *newArchetype = nullptr;

  if (expectedType->hasArchetype() || sourceType->hasArchetype()) {
    expectedType.visit(archetypeVisitor);
    sourceType.visit(archetypeVisitor);
    genericSig = buildThunkSignature(
        fn, hasArchetypes, openedExistential, genericEnv, contextSubs,
        interfaceSubs, newArchetype);
  }

  // Utility function to apply contextSubs, and also replace the
  // opened existential with the new archetype.
  auto substIntoThunkContext = [&](CanType t) -> CanType {
    return t.subst(
        [&](SubstitutableType *type) -> Type {
          if (CanType(type) == openedExistential)
            return newArchetype;
          return Type(type).subst(contextSubs);
        },
        LookUpConformanceInSubstitutionMap(contextSubs),
        SubstFlags::AllowLoweredTypes)->getCanonicalType();
  };

  sourceType = cast<SILFunctionType>(substIntoThunkContext(sourceType));
  expectedType = cast<SILFunctionType>(substIntoThunkContext(expectedType));

  // If our parent function was pseudogeneric, this thunk must also be
  // pseudogeneric, since we have no way to pass generic parameters.
  if (genericSig)
    if (origType->isPseudogeneric())
      extInfo = extInfo.withIsPseudogeneric();

  // Add the function type as the parameter.
  auto contextConvention =
      SILType::getPrimitiveObjectType(sourceType).isTrivial(*fn)
          ? ParameterConvention::Direct_Unowned
          : ParameterConvention::Direct_Guaranteed;
  SmallVector<SILParameterInfo, 4> params;
  params.append(expectedType->getParameters().begin(),
                expectedType->getParameters().end());
  // Add reabstraction function parameter only if building a reabstraction thunk
  // type.
  if (thunkKind == DifferentiationThunkKind::Reabstraction)
    params.push_back({sourceType, sourceType->getExtInfo().hasContext()
                                      ? contextConvention
                                      : ParameterConvention::Direct_Unowned});

  // Map the parameter and expected types out of context to get the interface
  // type of the thunk.
  SmallVector<SILParameterInfo, 4> interfaceParams;
  interfaceParams.reserve(params.size());
  for (auto &param : params) {
    auto paramIfaceTy = param.getInterfaceType()->mapTypeOutOfContext();
    interfaceParams.push_back(SILParameterInfo(
        paramIfaceTy->getCanonicalType(genericSig), param.getConvention()));
  }

  SmallVector<SILYieldInfo, 4> interfaceYields;
  for (auto &yield : expectedType->getYields()) {
    auto yieldIfaceTy = yield.getInterfaceType()->mapTypeOutOfContext();
    auto interfaceYield =
        yield.getWithInterfaceType(yieldIfaceTy->getCanonicalType(genericSig));
    interfaceYields.push_back(interfaceYield);
  }

  SmallVector<SILResultInfo, 4> interfaceResults;
  for (auto &result : expectedType->getResults()) {
    auto resultIfaceTy = result.getInterfaceType()->mapTypeOutOfContext();
    auto interfaceResult =
        result.getWithInterfaceType(resultIfaceTy->getCanonicalType(genericSig));
    interfaceResults.push_back(interfaceResult);
  }

  Optional<SILResultInfo> interfaceErrorResult;
  if (expectedType->hasErrorResult()) {
    auto errorResult = expectedType->getErrorResult();
    auto errorIfaceTy = errorResult.getInterfaceType()->mapTypeOutOfContext();
    interfaceErrorResult =
        SILResultInfo(errorIfaceTy->getCanonicalType(genericSig),
                      expectedType->getErrorResult().getConvention());
  }

  // The type of the thunk function.
  return SILFunctionType::get(
      genericSig, extInfo, expectedType->getCoroutineKind(),
      ParameterConvention::Direct_Unowned, interfaceParams, interfaceYields,
      interfaceResults, interfaceErrorResult, {}, false, module.getASTContext());
}

SILFunction *getOrCreateReabstractionThunk(SILOptFunctionBuilder &fb,
                                           SILModule &module, SILLocation loc,
                                           SILFunction *caller,
                                           CanSILFunctionType fromType,
                                           CanSILFunctionType toType) {
  SubstitutionMap interfaceSubs;
  GenericEnvironment *genericEnv = nullptr;
  auto thunkType = buildThunkType(
      caller, fromType, toType, genericEnv, interfaceSubs,
      /*withoutActuallyEscaping*/ false,
      DifferentiationThunkKind::Reabstraction);
  auto thunkDeclType =
      thunkType->getWithExtInfo(thunkType->getExtInfo().withNoEscape(false));

  auto fromInterfaceType = fromType->mapTypeOutOfContext()->getCanonicalType();
  auto toInterfaceType = toType->mapTypeOutOfContext()->getCanonicalType();

  Mangle::ASTMangler mangler;
  std::string name = mangler.mangleReabstractionThunkHelper(
      thunkType, fromInterfaceType, toInterfaceType,
      Type(), module.getSwiftModule());

  // FIXME(TF-989): Mark reabstraction thunks as transparent. This requires
  // generating ossa reabstraction thunks so that they can be inlined during
  // mandatory inlining when `-enable-strip-ownership-after-serialization` is
  // true and ownership model eliminator is not run after differentiation.
  auto *thunk = fb.getOrCreateSharedFunction(
      loc, name, thunkDeclType, IsBare, IsNotTransparent, IsSerialized,
      ProfileCounter(), IsReabstractionThunk, IsNotDynamic);
  if (!thunk->empty())
    return thunk;

  thunk->setGenericEnvironment(genericEnv);
  thunk->setOwnershipEliminated();
  auto *entry = thunk->createBasicBlock();
  SILBuilder builder(entry);
  createEntryArguments(thunk);

  SILFunctionConventions fromConv(fromType, module);
  SILFunctionConventions toConv(toType, module);
  assert(toConv.useLoweredAddresses());

  auto *fnArg = thunk->getArgumentsWithoutIndirectResults().back();

  SmallVector<SILValue, 4> arguments;
  auto toArgIter = thunk->getArguments().begin();
  auto useNextArgument = [&]() {
    arguments.push_back(*toArgIter++);
  };

  SmallVector<AllocStackInst *, 4> localAllocations;
  auto createAllocStack = [&](SILType type) {
    auto *alloc = builder.createAllocStack(loc, type);
    localAllocations.push_back(alloc);
    return alloc;
  };

  // Handle indirect results.
  assert(fromType->getNumResults() == toType->getNumResults());
  for (unsigned resIdx : range(toType->getNumResults())) {
    auto fromRes = fromConv.getResults()[resIdx];
    auto toRes = toConv.getResults()[resIdx];
    // No abstraction mismatch.
    if (fromRes.isFormalIndirect() == toRes.isFormalIndirect()) {
      // If result types are indirect, directly pass as next argument.
      if (toRes.isFormalIndirect())
        useNextArgument();
      continue;
    }
    // Convert indirect result to direct result.
    if (fromRes.isFormalIndirect()) {
      SILType resultTy = fromConv.getSILType(fromRes);
      assert(resultTy.isAddress());
      auto *indRes = createAllocStack(resultTy);
      arguments.push_back(indRes);
      continue;
    }
    // Convert direct result to indirect result.
    // Increment thunk argument iterator; reabstraction handled later.
    toArgIter++;
  }

  // Reabstract parameters.
  assert(toType->getNumParameters() == fromType->getNumParameters());
  for (unsigned paramIdx : range(toType->getNumParameters())) {
    auto fromParam = fromConv.getParameters()[paramIdx];
    auto toParam = toConv.getParameters()[paramIdx];
    // No abstraction mismatch. Directly use next argument.
    if (fromParam.isFormalIndirect() == toParam.isFormalIndirect()) {
      useNextArgument();
      continue;
    }
    // Convert indirect parameter to direct parameter.
    if (fromParam.isFormalIndirect()) {
      auto paramTy = fromConv.getSILType(fromType->getParameters()[paramIdx]);
      if (!paramTy.hasArchetype())
        paramTy = thunk->mapTypeIntoContext(paramTy);
      assert(paramTy.isAddress());
      auto *toArg = *toArgIter++;
      auto *buf = createAllocStack(toArg->getType());
      builder.createStore(loc, toArg, buf,
                          StoreOwnershipQualifier::Unqualified);
      arguments.push_back(buf);
      continue;
    }
    // Convert direct parameter to indirect parameter.
    assert(toParam.isFormalIndirect());
    auto *toArg = *toArgIter++;
    auto *load = builder.createLoad(loc, toArg,
                                    LoadOwnershipQualifier::Unqualified);
    arguments.push_back(load);
  }

  auto *apply = builder.createApply(
      loc, fnArg, SubstitutionMap(), arguments, /*isNonThrowing*/ false);

  // Get return elements.
  SmallVector<SILValue, 4> results;
  // Extract all direct results.
  SmallVector<SILValue, 4> directResults;
  extractAllElements(apply, builder, directResults);

  auto fromDirResultsIter = directResults.begin();
  auto fromIndResultsIter = apply->getIndirectSILResults().begin();
  auto toIndResultsIter = thunk->getIndirectResults().begin();
  // Reabstract results.
  for (unsigned resIdx : range(toType->getNumResults())) {
    auto fromRes = fromConv.getResults()[resIdx];
    auto toRes = toConv.getResults()[resIdx];
    // No abstraction mismatch.
    if (fromRes.isFormalIndirect() == toRes.isFormalIndirect()) {
      // If result types are direct, add call result as direct thunk result.
      if (toRes.isFormalDirect())
        results.push_back(*fromDirResultsIter++);
      // If result types are indirect, increment indirect result iterators.
      else {
        ++fromIndResultsIter;
        ++toIndResultsIter;
      }
      continue;
    }
    // Load direct results from indirect results.
    if (fromRes.isFormalIndirect()) {
      auto indRes = *fromIndResultsIter++;
      auto *load = builder.createLoad(loc, indRes,
                                      LoadOwnershipQualifier::Unqualified);
      results.push_back(load);
      continue;
    }
    // Store direct results to indirect results.
    assert(toRes.isFormalIndirect());
    SILType resultTy = toConv.getSILType(toRes);
    assert(resultTy.isAddress());
    auto indRes = *toIndResultsIter++;
    builder.createStore(loc, *fromDirResultsIter++, indRes,
                        StoreOwnershipQualifier::Unqualified);
  }
  auto retVal = joinElements(results, builder, loc);

  // Deallocate local allocations.
  for (auto *alloc : llvm::reverse(localAllocations))
    builder.createDeallocStack(loc, alloc);

  // Create return.
  builder.createReturn(loc, retVal);

  LLVM_DEBUG(auto &s = getADDebugStream() << "Created reabstraction thunk.\n";
             s << "  From type: " << fromType << '\n';
             s << "  To type: " << toType << '\n';
             s << '\n' << *thunk);

  return thunk;
}

//===----------------------------------------------------------------------===//
// Code emission utilities
//===----------------------------------------------------------------------===//

SILValue joinElements(ArrayRef<SILValue> elements, SILBuilder &builder,
                      SILLocation loc) {
  if (elements.size() == 1)
    return elements.front();
  return builder.createTuple(loc, elements);
}

/// Given a value, extracts all elements to `results` from this value if it has
/// a tuple type. Otherwise, add this value directly to `results`.
void extractAllElements(SILValue value, SILBuilder &builder,
                        SmallVectorImpl<SILValue> &results) {
  auto tupleType = value->getType().getAs<TupleType>();
  if (!tupleType) {
    results.push_back(value);
    return;
  }
  if (builder.hasOwnership()) {
    auto *dti = builder.createDestructureTuple(value.getLoc(), value);
    results.append(dti->getResults().begin(), dti->getResults().end());
    return;
  }
  for (auto i : range(tupleType->getNumElements()))
    results.push_back(builder.createTupleExtract(value.getLoc(), value, i));
}

//===----------------------------------------------------------------------===//
// Utilities for looking up derivatives of functions
//===----------------------------------------------------------------------===//

/// Returns the AbstractFunctionDecl corresponding to `F`. If there isn't one,
/// returns `nullptr`.
static AbstractFunctionDecl *findAbstractFunctionDecl(SILFunction *F) {
  auto *DC = F->getDeclContext();
  if (!DC)
    return nullptr;
  auto *D = DC->getAsDecl();
  if (!D)
    return nullptr;
  return dyn_cast<AbstractFunctionDecl>(D);
}

SILDifferentiabilityWitness *
getExactDifferentiabilityWitness(SILModule &module, SILFunction *original,
                                 IndexSubset *parameterIndices,
                                 IndexSubset *resultIndices) {
  for (auto *w : module.lookUpDifferentiabilityWitnessesForFunction(
           original->getName())) {
    if (w->getParameterIndices() == parameterIndices &&
        w->getResultIndices() == resultIndices)
      return w;
  }
  return nullptr;
}

Optional<AutoDiffConfig>
findMinimalDerivativeConfiguration(AbstractFunctionDecl *original,
                                   IndexSubset *parameterIndices,
                                   IndexSubset *&minimalASTParameterIndices) {
  Optional<AutoDiffConfig> minimalConfig = None;
  auto configs = original->getDerivativeFunctionConfigurations();
  for (auto config : configs) {
    auto *silParameterIndices = autodiff::getLoweredParameterIndices(
        config.parameterIndices,
        original->getInterfaceType()->castTo<AnyFunctionType>());
    // If all indices in `parameterIndices` are in `daParameterIndices`, and
    // it has fewer indices than our current candidate and a primitive VJP,
    // then `attr` is our new candidate.
    //
    // NOTE(TF-642): `attr` may come from a un-partial-applied function and
    // have larger capacity than the desired indices. We expect this logic to
    // go away when `partial_apply` supports `@differentiable` callees.
    if (silParameterIndices->isSupersetOf(parameterIndices->extendingCapacity(
            original->getASTContext(), silParameterIndices->getCapacity())) &&
        // fewer parameters than before
        (!minimalConfig ||
         silParameterIndices->getNumIndices() <
             minimalConfig->parameterIndices->getNumIndices())) {
      minimalASTParameterIndices = config.parameterIndices;
      minimalConfig = AutoDiffConfig(silParameterIndices, config.resultIndices,
                                     config.derivativeGenericSignature);
    }
  }
  return minimalConfig;
}

SILDifferentiabilityWitness *getOrCreateMinimalASTDifferentiabilityWitness(
    SILModule &module, SILFunction *original, IndexSubset *parameterIndices,
    IndexSubset *resultIndices) {
  // AST differentiability witnesses always have a single result.
  if (resultIndices->getCapacity() != 1 || !resultIndices->contains(0))
    return nullptr;

  // Explicit differentiability witnesses only exist on SILFunctions that come
  // from AST functions.
  auto *originalAFD = findAbstractFunctionDecl(original);
  if (!originalAFD)
    return nullptr;

  IndexSubset *minimalASTParameterIndices = nullptr;
  auto minimalConfig = findMinimalDerivativeConfiguration(
      originalAFD, parameterIndices, minimalASTParameterIndices);
  if (!minimalConfig)
    return nullptr;

  auto *existingWitness = module.lookUpDifferentiabilityWitness(
      {original->getName(), *minimalConfig});
  if (existingWitness)
    return existingWitness;

  return SILDifferentiabilityWitness::createDeclaration(
      module, SILLinkage::PublicExternal, original,
      minimalConfig->parameterIndices, minimalConfig->resultIndices,
      minimalConfig->derivativeGenericSignature);
}


} // end namespace autodiff
} // end namespace swift
