//===--- AutoDiff.cpp - Swift Differentiable Programming ------------------===//
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
  for (auto *curryLevel : reversed(curryLevels))
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
  for (unsigned curryLevelIndex : reversed(indices(curryLevels))) {
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

IndexSubset *
IndexSubset::getFromString(ASTContext &ctx, StringRef string) {
  if (string.size() < 0) return nullptr;
  unsigned capacity = string.size();
  llvm::SmallBitVector indices(capacity);
  for (unsigned i : range(capacity)) {
    if (string[i] == 'S')
      indices.set(i);
    else if (string[i] != 'U')
      return nullptr;
  }
  return get(ctx, indices);
}

std::string IndexSubset::getString() const {
  std::string result;
  result.reserve(capacity);
  for (unsigned i : range(capacity))
    result += contains(i) ? 'S' : 'U';
  return result;
}

bool IndexSubset::isSubsetOf(IndexSubset *other) const {
  assert(capacity == other->capacity);
  for (auto index : range(numBitWords))
    if (getBitWord(index) & ~other->getBitWord(index))
      return false;
  return true;
}

bool IndexSubset::isSupersetOf(IndexSubset *other) const {
  assert(capacity == other->capacity);
  for (auto index : range(numBitWords))
    if (~getBitWord(index) & other->getBitWord(index))
      return false;
  return true;
}

IndexSubset *IndexSubset::adding(unsigned index,
                                                 ASTContext &ctx) const {
  assert(index < getCapacity());
  if (contains(index))
    return const_cast<IndexSubset *>(this);
  SmallBitVector newIndices(capacity);
  bool inserted = false;
  for (auto curIndex : getIndices()) {
    if (!inserted && curIndex > index) {
      newIndices.set(index);
      inserted = true;
    }
    newIndices.set(curIndex);
  }
  return get(ctx, newIndices);
}

IndexSubset *IndexSubset::extendingCapacity(
    ASTContext &ctx, unsigned newCapacity) const {
  assert(newCapacity >= capacity);
  if (newCapacity == capacity)
    return const_cast<IndexSubset *>(this);
  SmallBitVector indices(newCapacity);
  for (auto index : getIndices())
    indices.set(index);
  return IndexSubset::get(ctx, indices);
}

int IndexSubset::findNext(int startIndex) const {
  assert(startIndex < (int)capacity && "Start index cannot be past the end");
  unsigned bitWordIndex = 0, offset = 0;
  if (startIndex >= 0) {
    auto indexAndOffset = getBitWordIndexAndOffset(startIndex);
    bitWordIndex = indexAndOffset.first;
    offset = indexAndOffset.second + 1;
  }
  for (; bitWordIndex < numBitWords; ++bitWordIndex, offset = 0) {
    for (; offset < numBitsPerBitWord; ++offset) {
      auto index = bitWordIndex * numBitsPerBitWord + offset;
      auto bitWord = getBitWord(bitWordIndex);
      if (!bitWord)
        break;
      if (index >= capacity)
        return capacity;
      if (bitWord & ((BitWord)1 << offset))
        return index;
    }
  }
  return capacity;
}

int IndexSubset::findPrevious(int endIndex) const {
  assert(endIndex >= 0 && "End index cannot be before the start");
  int bitWordIndex = numBitWords - 1, offset = numBitsPerBitWord - 1;
  if (endIndex < (int)capacity) {
    auto indexAndOffset = getBitWordIndexAndOffset(endIndex);
    bitWordIndex = (int)indexAndOffset.first;
    offset = (int)indexAndOffset.second - 1;
  }
  for (; bitWordIndex >= 0; --bitWordIndex, offset = numBitsPerBitWord - 1) {
    for (; offset < (int)numBitsPerBitWord; --offset) {
      auto index = bitWordIndex * (int)numBitsPerBitWord + offset;
      auto bitWord = getBitWord(bitWordIndex);
      if (!bitWord)
        break;
      if (index < 0)
        return -1;
      if (bitWord & ((BitWord)1 << offset))
        return index;
    }
  }
  return -1;
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
