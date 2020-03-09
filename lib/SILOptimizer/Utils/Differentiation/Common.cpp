//===--- Common.cpp - Automatic differentiation common utils --*- C++ -*---===//
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

#include "swift/SILOptimizer/Utils/Differentiation/Common.h"
#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"

namespace swift {
namespace autodiff {

raw_ostream &getADDebugStream() { return llvm::dbgs() << "[AD] "; }

bool isArrayLiteralIntrinsic(FullApplySite applySite) {
  return doesApplyCalleeHaveSemantics(applySite.getCalleeOrigin(),
                                      "array.uninitialized_intrinsic");
}

ApplyInst *getAllocateUninitializedArrayIntrinsic(SILValue v) {
  if (auto *ai = dyn_cast<ApplyInst>(v))
    if (isArrayLiteralIntrinsic(ai))
      return ai;
  return nullptr;
}

ApplyInst *getAllocateUninitializedArrayIntrinsicElementAddress(SILValue v) {
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
    FullApplySite applySite,
    llvm::function_ref<void(SILValue)> resultCallback) {
  switch (applySite.getKind()) {
  case FullApplySiteKind::ApplyInst: {
    auto *ai = cast<ApplyInst>(applySite.getInstruction());
    if (!ai->getType().is<TupleType>()) {
      resultCallback(ai);
      return;
    }
    if (auto *dti = getSingleDestructureTupleUser(ai))
      for (auto directResult : dti->getResults())
        resultCallback(directResult);
    break;
  }
  case FullApplySiteKind::BeginApplyInst: {
    auto *bai = cast<BeginApplyInst>(applySite.getInstruction());
    for (auto directResult : bai->getResults())
      resultCallback(directResult);
    break;
  }
  case FullApplySiteKind::TryApplyInst: {
    auto *tai = cast<TryApplyInst>(applySite.getInstruction());
    for (auto *succBB : tai->getSuccessorBlocks())
      for (auto *arg : succBB->getArguments())
        resultCallback(arg);
    break;
  }
  }
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
  // Treat `inout` parameters as semantic results.
  // Append `inout` parameters after formal results.
  for (auto i : range(convs.getNumParameters())) {
    auto paramInfo = convs.getParameters()[i];
    if (!paramInfo.isIndirectMutating())
      continue;
    auto *argument = function.getArgumentsWithoutIndirectResults()[i];
    results.push_back(argument);
  }
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
  // Record all `inout` parameters as results.
  auto inoutParamResultIndex = calleeFnTy->getNumResults();
  for (auto &paramAndIdx : enumerate(calleeConvs.getParameters())) {
    auto &param = paramAndIdx.value();
    if (!param.isIndirectMutating())
      continue;
    unsigned idx = paramAndIdx.index();
    auto inoutArg = ai->getArgument(idx);
    results.push_back(inoutArg);
    resultIndices.push_back(inoutParamResultIndex++);
  }
  // Make sure the function call has active results.
  auto numResults = calleeFnTy->getNumResults() +
                    calleeFnTy->getNumIndirectMutatingParameters();
  assert(results.size() == numResults);
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
  auto *getter = builder.createWitnessMethod(loc, type, confRef,
                                             accessorDeclRef, silFnType);
  // %metatype = metatype $T
  auto metatypeType = CanMetatypeType::get(type, MetatypeRepresentation::Thick);
  auto metatype = builder.createMetatype(
      loc, SILType::getPrimitiveObjectType(metatypeType));
  auto subMap = SubstitutionMap::getProtocolSubstitutions(
      additiveArithmeticProto, type, confRef);
  builder.createApply(loc, getter, subMap, {bufferAccess, metatype},
                      /*isNonThrowing*/ false);
  builder.emitDestroyValueOperation(loc, getter);
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

  // Explicit differentiability witnesses only exist on SIL functions that come
  // from AST functions.
  auto *originalAFD = findAbstractFunctionDecl(original);
  if (!originalAFD)
    return nullptr;

  IndexSubset *minimalASTParameterIndices = nullptr;
  auto minimalConfig = findMinimalDerivativeConfiguration(
      originalAFD, parameterIndices, minimalASTParameterIndices);
  if (!minimalConfig)
    return nullptr;

  std::string originalName = original->getName();
  // If original function requires a foreign entry point, use the foreign SIL
  // function to get or create the minimal differentiability witness.
  if (requiresForeignEntryPoint(originalAFD)) {
    originalName = SILDeclRef(originalAFD).asForeign().mangle();
    original = module.lookUpFunction(SILDeclRef(originalAFD).asForeign());
  }

  auto *existingWitness =
      module.lookUpDifferentiabilityWitness({originalName, *minimalConfig});
  if (existingWitness)
    return existingWitness;

  assert(original->isExternalDeclaration() &&
         "SILGen should create differentiability witnesses for all function "
         "definitions with explicit differentiable attributes");

  return SILDifferentiabilityWitness::createDeclaration(
      module, SILLinkage::PublicExternal, original,
      minimalConfig->parameterIndices, minimalConfig->resultIndices,
      minimalConfig->derivativeGenericSignature);
}

} // end namespace autodiff
} // end namespace swift
