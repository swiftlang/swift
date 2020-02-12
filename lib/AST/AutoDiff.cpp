//===--- AutoDiff.cpp - Swift automatic differentiation utilities ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AutoDiff.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"

using namespace swift;

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
// See TF-874 for WIP.
void autodiff::getSubsetParameterTypes(IndexSubset *subset,
                                       AnyFunctionType *type,
                                       SmallVectorImpl<Type> &results,
                                       bool reverseCurryLevels) {
  SmallVector<AnyFunctionType *, 2> curryLevels;
  unwrapCurryLevels(type, curryLevels);

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
      if (subset->contains(parameterIndexOffset + paramIndex))
        results.push_back(curryLevel->getParams()[paramIndex].getOldType());
  }
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
