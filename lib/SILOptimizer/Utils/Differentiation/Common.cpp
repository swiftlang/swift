//===--- Common.cpp - Automatic differentiation common utils --*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Automatic differentiation common utilities.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SILOptimizer/Utils/Differentiation/Common.h"

namespace swift {
namespace autodiff {

raw_ostream &getADDebugStream() { return llvm::dbgs() << "[AD] "; }

//===----------------------------------------------------------------------===//
// Code emission utilities
//===----------------------------------------------------------------------===//

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

SILValue joinElements(ArrayRef<SILValue> elements, SILBuilder &builder,
                      SILLocation loc) {
  if (elements.size() == 1)
    return elements.front();
  return builder.createTuple(loc, elements);
}

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
