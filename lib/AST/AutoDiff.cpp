//===--- AutoDiff.cpp - Swift automatic differentiation utilities ---------===//
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

#include "swift/AST/AutoDiff.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"

using namespace swift;

DifferentiabilityWitnessFunctionKind::DifferentiabilityWitnessFunctionKind(
    StringRef string) {
  Optional<innerty> result = llvm::StringSwitch<Optional<innerty>>(string)
                                 .Case("jvp", JVP)
                                 .Case("vjp", VJP)
                                 .Case("transpose", Transpose);
  assert(result && "Invalid string");
  rawValue = *result;
}

Optional<AutoDiffDerivativeFunctionKind>
DifferentiabilityWitnessFunctionKind::getAsDerivativeFunctionKind() const {
  switch (rawValue) {
  case JVP:
    return {AutoDiffDerivativeFunctionKind::JVP};
  case VJP:
    return {AutoDiffDerivativeFunctionKind::VJP};
  case Transpose:
    return None;
  }
}

void AutoDiffConfig::print(llvm::raw_ostream &s) const {
  s << "(parameters=";
  parameterIndices->print(s);
  s << " results=";
  resultIndices->print(s);
  if (derivativeGenericSignature) {
    s << " where=";
    derivativeGenericSignature->print(s);
  }
  s << ')';
}

// TODO(TF-874): This helper is inefficient and should be removed. Unwrapping at
// most once (for curried method types) is sufficient.
static void unwrapCurryLevels(AnyFunctionType *fnTy,
                              SmallVectorImpl<AnyFunctionType *> &results) {
  while (fnTy != nullptr) {
    results.push_back(fnTy);
    fnTy = fnTy->getResult()->getAs<AnyFunctionType>();
  }
}

static unsigned countNumFlattenedElementTypes(Type type) {
  if (auto *tupleTy = type->getCanonicalType()->getAs<TupleType>())
    return accumulate(tupleTy->getElementTypes(), 0,
                      [&](unsigned num, Type type) {
                        return num + countNumFlattenedElementTypes(type);
                      });
  return 1;
}

// TODO(TF-874): Simplify this helper and remove the `reverseCurryLevels` flag.
void AnyFunctionType::getSubsetParameters(
    IndexSubset *parameterIndices,
    SmallVectorImpl<AnyFunctionType::Param> &results, bool reverseCurryLevels) {
  SmallVector<AnyFunctionType *, 2> curryLevels;
  unwrapCurryLevels(this, curryLevels);

  SmallVector<unsigned, 2> curryLevelParameterIndexOffsets(curryLevels.size());
  unsigned currentOffset = 0;
  for (unsigned curryLevelIndex : llvm::reverse(indices(curryLevels))) {
    curryLevelParameterIndexOffsets[curryLevelIndex] = currentOffset;
    currentOffset += curryLevels[curryLevelIndex]->getNumParams();
  }

  // If `reverseCurryLevels` is true, reverse the curry levels and offsets.
  if (reverseCurryLevels) {
    std::reverse(curryLevels.begin(), curryLevels.end());
    std::reverse(curryLevelParameterIndexOffsets.begin(),
                 curryLevelParameterIndexOffsets.end());
  }

  for (unsigned curryLevelIndex : indices(curryLevels)) {
    auto *curryLevel = curryLevels[curryLevelIndex];
    unsigned parameterIndexOffset =
        curryLevelParameterIndexOffsets[curryLevelIndex];
    for (unsigned paramIndex : range(curryLevel->getNumParams()))
      if (parameterIndices->contains(parameterIndexOffset + paramIndex))
        results.push_back(curryLevel->getParams()[paramIndex]);
  }
}

SemanticFunctionResultType autodiff::getOriginalFunctionSemanticResultType(
    AnyFunctionType *functionType, Type originalFormalResultType,
    bool &hasMultipleOriginalSemanticResults,
    GenericEnvironment *derivativeGenEnv) {
  auto &ctx = functionType->getASTContext();
  hasMultipleOriginalSemanticResults = false;

  // Initialize original semantic result type as the original formal result
  // type, unless it is `Void`.
  Type originalResultType;
  bool isOriginalResultInout = false;
  if (!originalFormalResultType->isEqual(ctx.TheEmptyTupleType))
    originalResultType = originalFormalResultType;

  auto setOriginalSemanticResultType = [&](Type type) {
    // If original semantic result type has already been set, unset it and set
    // `hasMultipleOriginalSemanticResults` to true.
    if (originalResultType) {
      hasMultipleOriginalSemanticResults = true;
      originalResultType = Type();
      return;
    }
    originalResultType = type;
    isOriginalResultInout = true;
  };

  // Check for `inout` parameters.
  for (auto param : functionType->getParams())
    if (param.isInOut())
      setOriginalSemanticResultType(param.getPlainType());
  if (auto *resultFunctionType =
          functionType->getResult()->getAs<AnyFunctionType>()) {
    for (auto param : resultFunctionType->getParams())
      if (param.isInOut())
        setOriginalSemanticResultType(param.getPlainType());
  }

  // Map original semantic result type into derivative generic environment.
  if (originalResultType && derivativeGenEnv)
    originalResultType =
        derivativeGenEnv->mapTypeIntoContext(originalResultType);
  return {originalResultType, isOriginalResultInout};
}

GenericSignature autodiff::getConstrainedDerivativeGenericSignature(
    SILFunctionType *originalFnTy, IndexSubset *diffParamIndices,
    GenericSignature derivativeGenSig, LookupConformanceFn lookupConformance,
    bool isTranspose) {
  if (!derivativeGenSig)
    derivativeGenSig = originalFnTy->getSubstGenericSignature();
  if (!derivativeGenSig)
    return nullptr;
  auto &ctx = originalFnTy->getASTContext();
  auto *diffableProto = ctx.getProtocol(KnownProtocolKind::Differentiable);
  SmallVector<Requirement, 4> requirements;
  for (unsigned paramIdx : diffParamIndices->getIndices()) {
    // Require differentiability parameters to conform to `Differentiable`.
    auto paramType = originalFnTy->getParameters()[paramIdx].getInterfaceType();
    Requirement req(RequirementKind::Conformance, paramType,
                    diffableProto->getDeclaredType());
    requirements.push_back(req);
    if (isTranspose) {
      // Require linearity parameters to additionally satisfy
      // `Self == Self.TangentVector`.
      auto tanSpace = paramType->getAutoDiffTangentSpace(lookupConformance);
      auto paramTanType = tanSpace->getCanonicalType();
      Requirement req(RequirementKind::SameType, paramType, paramTanType);
      requirements.push_back(req);
    }
  }
  return evaluateOrDefault(
      ctx.evaluator,
      AbstractGenericSignatureRequest{derivativeGenSig.getPointer(),
                                      /*addedGenericParams*/ {},
                                      std::move(requirements)},
      nullptr);
}

Type TangentSpace::getType() const {
  switch (kind) {
  case Kind::TangentVector:
    return value.tangentVectorType;
  case Kind::Tuple:
    return value.tupleType;
  }
}

CanType TangentSpace::getCanonicalType() const {
  return getType()->getCanonicalType();
}

NominalTypeDecl *TangentSpace::getNominal() const {
  assert(isTangentVector());
  return getTangentVector()->getNominalOrBoundGenericNominal();
}
