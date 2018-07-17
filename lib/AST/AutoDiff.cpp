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
  int last = -1;
  unsigned maxIdx = 0;
  for (auto paramIdx : parameters) {
    assert((int)paramIdx > last && "Parameter indices must be ascending");
    last = paramIdx;
    if (paramIdx > maxIdx) {
      maxIdx = paramIdx;
      this->parameters.resize(maxIdx + 1);
    }
    this->parameters.set(paramIdx);
  }
}
