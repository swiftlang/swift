//===-------- AutoDiff.cpp - Routines for USR generation ------------------===//
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
#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

SILAutoDiffIndices::SILAutoDiffIndices(
    unsigned source, ArrayRef<unsigned> parameters) : source(source) {
  if (parameters.empty())
    return;

  auto max = *std::max_element(parameters.begin(), parameters.end());
  this->parameters.resize(max + 1);
  int last = -1;
  for (auto paramIdx : parameters) {
    assert((int)paramIdx > last && "Parameter indices must be ascending");
    last = paramIdx;
    this->parameters.set(paramIdx);
  }
}

bool SILAutoDiffIndices::operator==(
    const SILAutoDiffIndices &other) const {
  if (source != other.source)
    return false;

  // The parameters are the same when they have exactly the same set bit
  // indices, even if they have different sizes.
  llvm::SmallBitVector buffer(std::max(parameters.size(),
                                       other.parameters.size()));
  buffer ^= parameters;
  buffer ^= other.parameters;
  return buffer.none();
}

Differentiability::Differentiability(AutoDiffMode mode,
                                     bool wrtSelf,
                                     llvm::SmallBitVector parameterIndices,
                                     llvm::SmallBitVector resultIndices)
    : mode(mode), wrtSelf(wrtSelf), parameterIndices(parameterIndices),
      resultIndices(resultIndices) {
}

Differentiability::Differentiability(AutoDiffMode mode,
                                     AnyFunctionType *type)
    : mode(mode), wrtSelf(type->getExtInfo().hasSelfParam()),
      // For now, we assume exactly one result until we figure out how to
      // model result selection.
      resultIndices(1) {
  // If function has self, it must be a curried method type.
  if (wrtSelf) {
    auto methodTy = type->getResult()->castTo<AnyFunctionType>();
    parameterIndices = llvm::SmallBitVector(methodTy->getNumParams());
  } else {
    parameterIndices = llvm::SmallBitVector(type->getNumParams());
  }
  parameterIndices.set();
  resultIndices.set();
}

unsigned autodiff::getOffsetForAutoDiffAssociatedFunction(
    unsigned order, SILAutoDiffAssociatedFunctionKind kind) {
  unsigned offset;
  switch (kind) {
  case SILAutoDiffAssociatedFunctionKind::LegacyPrimal:
    offset = 0;
    break;
  case SILAutoDiffAssociatedFunctionKind::LegacyAdjoint:
    offset = 1;
    break;
  case SILAutoDiffAssociatedFunctionKind::JVP:
    offset = 0;
    break;
  case SILAutoDiffAssociatedFunctionKind::VJP:
    offset = 1;
    break;
  }
  return (order - 1) * 2 + offset;
}
