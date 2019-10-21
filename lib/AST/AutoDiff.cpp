//===--------- AutoDiff.cpp - Swift Differentiable Programming ------------===//
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

#include "swift/AST/AutoDiff.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILLinkage.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

bool SILAutoDiffIndices::operator==(const SILAutoDiffIndices &other) const {
  return source == other.source && parameters == other.parameters;
}

AutoDiffDerivativeFunctionKind::
AutoDiffDerivativeFunctionKind(StringRef string) {
  Optional<innerty> result =
      llvm::StringSwitch<Optional<innerty>>(string)
          .Case("jvp", JVP).Case("vjp", VJP);
  assert(result && "Invalid string");
  rawValue = *result;
}

DifferentiabilityWitnessFunctionKind::
DifferentiabilityWitnessFunctionKind(StringRef string) {
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
  case JVP: return {AutoDiffDerivativeFunctionKind::JVP};
  case VJP: return {AutoDiffDerivativeFunctionKind::VJP};
  case Transpose: return None;
  }
}

NormalDifferentiableFunctionTypeComponent::
NormalDifferentiableFunctionTypeComponent(AutoDiffDerivativeFunctionKind kind) {
  switch (kind) {
  case AutoDiffDerivativeFunctionKind::JVP: rawValue = JVP; return;
  case AutoDiffDerivativeFunctionKind::VJP: rawValue = VJP; return;
  }
}

NormalDifferentiableFunctionTypeComponent::
NormalDifferentiableFunctionTypeComponent(StringRef string) {
  Optional<innerty> result = llvm::StringSwitch<Optional<innerty>>(string)
      .Case("original", Original)
      .Case("jvp", JVP)
      .Case("vjp", VJP);
  assert(result && "Invalid string");
  rawValue = *result;
}

Optional<AutoDiffDerivativeFunctionKind>
NormalDifferentiableFunctionTypeComponent::getAsDerivativeFunctionKind() const {
  switch (rawValue) {
  case Original: return None;
  case JVP: return {AutoDiffDerivativeFunctionKind::JVP};
  case VJP: return {AutoDiffDerivativeFunctionKind::VJP};
  }
}

LinearDifferentiableFunctionTypeComponent::
LinearDifferentiableFunctionTypeComponent(StringRef string) {
  Optional<innerty> result =
      llvm::StringSwitch<Optional<innerty>>(string)
          .Case("original", Original)
          .Case("transpose", Transpose);
  assert(result && "Invalid string");
  rawValue = *result;
}

void SILAutoDiffIndices::print(llvm::raw_ostream &s) const {
  s << "(source=" << source << " parameters=(";
  interleave(parameters->getIndices(),
             [&s](unsigned p) { s << p; }, [&s]{ s << ' '; });
  s << "))";
}

void SILAutoDiffIndices::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
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

void AutoDiffConfig::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

// TODO(TF-874): This helper is inefficient and should be removed. Unwrapping at
// most once (for curried method types) is sufficient.
static void unwrapCurryLevels(AnyFunctionType *fnTy,
                              SmallVectorImpl<AnyFunctionType *> &result) {
  while (fnTy != nullptr) {
    result.push_back(fnTy);
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

// TODO(TF-874): Simplify this helper. See TF-874 for WIP.
IndexSubset *
autodiff::getLoweredParameterIndices(IndexSubset *indices,
                                     AnyFunctionType *type) {
  SmallVector<AnyFunctionType *, 2> curryLevels;
  unwrapCurryLevels(type, curryLevels);

  // Calculate the lowered sizes of all the parameters.
  SmallVector<unsigned, 8> paramLoweredSizes;
  unsigned totalLoweredSize = 0;
  auto addLoweredParamInfo = [&](Type type) {
    unsigned paramLoweredSize = countNumFlattenedElementTypes(type);
    paramLoweredSizes.push_back(paramLoweredSize);
    totalLoweredSize += paramLoweredSize;
  };
  for (auto *curryLevel : llvm::reverse(curryLevels))
    for (auto &param : curryLevel->getParams())
      addLoweredParamInfo(param.getPlainType());

  // Construct the result by setting each range of bits that corresponds to each
  // "on" parameter.
  llvm::SmallVector<unsigned, 8> loweredIndices;
  unsigned currentBitIndex = 0;
  for (unsigned i : range(indices->getCapacity())) {
    auto paramLoweredSize = paramLoweredSizes[i];
    if (indices->contains(i)) {
      auto indices = range(currentBitIndex, currentBitIndex + paramLoweredSize);
      loweredIndices.append(indices.begin(), indices.end());
    }
    currentBitIndex += paramLoweredSize;
  }

  return IndexSubset::get(
      type->getASTContext(), totalLoweredSize, loweredIndices);
}

// TODO(TF-874): Simplify this helper and remove the `reverseCurryLevels` flag.
// See TF-874 for WIP.
void autodiff::getSubsetParameterTypes(IndexSubset *subset,
                                       AnyFunctionType *type,
                                       SmallVectorImpl<Type> &result,
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
        result.push_back(
            curryLevel->getParams()[paramIndex].getOldType());
  }
}

bool autodiff::getBuiltinAutoDiffApplyConfig(
    StringRef operationName, AutoDiffDerivativeFunctionKind &kind,
    unsigned &arity, bool &rethrows) {
  if (!operationName.startswith("autodiffApply_"))
    return false;
  operationName = operationName.drop_front(strlen("autodiffApply_"));
  // Parse 'jvp' or 'vjp'.
  if (operationName.startswith("jvp"))
    kind = AutoDiffDerivativeFunctionKind::JVP;
  else if (operationName.startswith("vjp"))
    kind = AutoDiffDerivativeFunctionKind::VJP;
  operationName = operationName.drop_front(3);
  // Parse '_arity'.
  if (operationName.startswith("_arity")) {
    operationName = operationName.drop_front(strlen("_arity"));
    auto arityStr = operationName.take_while(llvm::isDigit);
    operationName = operationName.drop_front(arityStr.size());
    auto converted = llvm::to_integer(arityStr, arity);
    assert(converted); (void)converted;
    assert(arity > 0);
  } else {
    arity = 1;
  }
  // Parse '_rethrows'.
  if (operationName.startswith("_rethrows")) {
    operationName = operationName.drop_front(strlen("_rethrows"));
    rethrows = true;
  } else {
    rethrows = false;
  }
  return operationName.empty();
}

SILLinkage autodiff::getAutoDiffDerivativeFunctionLinkage(
    SILLinkage originalLinkage, bool isDerivativeFnExported) {
  // If the original is defined externally, then the AD pass is just generating
  // derivative functions for use in the current module and therefore these
  // derivative functions should not be visible outside the module.
  if (isAvailableExternally(originalLinkage))
    return SILLinkage::Hidden;

  // If the original is public, then external modules may need to link the
  // derivative function. Return the linkage of the original function, unless
  // the derivative function is not exported (i.e. differentiation is not
  // explicitly requested via a `[differentiable]` attribute on the original
  // function).
  if (originalLinkage == SILLinkage::Public ||
      originalLinkage == SILLinkage::PublicNonABI ||
      originalLinkage == SILLinkage::Shared)
    return isDerivativeFnExported ? originalLinkage : SILLinkage::Hidden;

  // Otherwise, the original function is defined and used only in the current
  // module, so external modules will never try to access the associated
  // function. Make the derivative function hidden.
  return SILLinkage::Hidden;
}

Type VectorSpace::getType() const {
  switch (kind) {
  case Kind::Vector:
    return value.vectorType;
  case Kind::Tuple:
    return value.tupleType;
  case Kind::Function:
    return value.functionType;
  }
}

CanType VectorSpace::getCanonicalType() const {
  return getType()->getCanonicalType();
}

NominalTypeDecl *VectorSpace::getNominal() const {
  return getVector()->getNominalOrBoundGenericNominal();
}
