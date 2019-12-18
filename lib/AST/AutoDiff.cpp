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
#include "swift/AST/Types.h"

using namespace swift;

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

// SWIFT_ENABLE_TENSORFLOW
// Not-yet-upstreamed `tensorflow` branch additions are below.

#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
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

SILAutoDiffIndices AutoDiffConfig::getSILAutoDiffIndices() const {
  assert(resultIndices->getNumIndices() == 1);
  return SILAutoDiffIndices(*resultIndices->begin(), parameterIndices);
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

// Given the rest of a `Builtin.applyDerivative_{jvp|vjp}` or
// `Builtin.applyTranspose` operation name, attempts to parse the arity and
// throwing-ness from the operation name. Modifies the operation name argument
// in place as substrings get dropped.
static void parseAutoDiffBuiltinCommonConfig(
    StringRef &operationName, unsigned &arity, bool &throws) {
  // Parse '_arity'.
  constexpr char arityPrefix[] = "_arity";
  if (operationName.startswith(arityPrefix)) {
    operationName = operationName.drop_front(sizeof(arityPrefix) - 1);
    auto arityStr = operationName.take_while(llvm::isDigit);
    operationName = operationName.drop_front(arityStr.size());
    auto converted = llvm::to_integer(arityStr, arity);
    assert(converted); (void)converted;
    assert(arity > 0);
  } else {
    arity = 1;
  }
  // Parse '_throws'.
  constexpr char throwsPrefix[] = "_throws";
  if (operationName.startswith(throwsPrefix)) {
    operationName = operationName.drop_front(sizeof(throwsPrefix) - 1);
    throws = true;
  } else {
    throws = false;
  }
}

bool autodiff::getBuiltinApplyDerivativeConfig(
    StringRef operationName, AutoDiffDerivativeFunctionKind &kind,
    unsigned &arity, bool &throws) {
  constexpr char prefix[] = "applyDerivative";
  if (!operationName.startswith(prefix))
    return false;
  operationName = operationName.drop_front(sizeof(prefix) - 1);
  // Parse 'jvp' or 'vjp'.
  constexpr char jvpPrefix[] = "_jvp";
  constexpr char vjpPrefix[] = "_vjp";
  if (operationName.startswith(jvpPrefix))
    kind = AutoDiffDerivativeFunctionKind::JVP;
  else if (operationName.startswith(vjpPrefix))
    kind = AutoDiffDerivativeFunctionKind::VJP;
  operationName = operationName.drop_front(sizeof(jvpPrefix) - 1);
  parseAutoDiffBuiltinCommonConfig(operationName, arity, throws);
  return operationName.empty();
}

bool autodiff::getBuiltinApplyTransposeConfig(
    StringRef operationName, unsigned &arity, bool &throws) {
  constexpr char prefix[] = "applyTranspose";
  if (!operationName.startswith(prefix))
    return false;
  operationName = operationName.drop_front(sizeof(prefix) - 1);
  parseAutoDiffBuiltinCommonConfig(operationName, arity, throws);
  return operationName.empty();
}

bool autodiff::getBuiltinDifferentiableOrLinearFunctionConfig(
    StringRef operationName, unsigned &arity, bool &throws) {
  constexpr char differentiablePrefix[] = "differentiableFunction";
  constexpr char linearPrefix[] = "linearFunction";
  if (operationName.startswith(differentiablePrefix))
    operationName = operationName.drop_front(sizeof(differentiablePrefix) - 1);
  else if (operationName.startswith(linearPrefix))
    operationName = operationName.drop_front(sizeof(linearPrefix) - 1);
  else
    return false;
  parseAutoDiffBuiltinCommonConfig(operationName, arity, throws);
  return operationName.empty();
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
// SWIFT_ENABLE_TENSORFLOW END
