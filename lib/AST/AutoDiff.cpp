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
#include "swift/Basic/LLVM.h"

using namespace swift;

SILReverseAutoDiffIndices::SILReverseAutoDiffIndices(
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

bool SILReverseAutoDiffIndices::operator==(
    const SILReverseAutoDiffIndices &other) const {
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
