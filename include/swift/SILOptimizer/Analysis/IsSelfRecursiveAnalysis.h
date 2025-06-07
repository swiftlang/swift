//===--- IsSelfRecursiveAnalysis.h ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ISSELFRECURSIVEANALYSIS_H
#define SWIFT_SILOPTIMIZER_ISSELFRECURSIVEANALYSIS_H

#include "swift/SILOptimizer/Analysis/Analysis.h"

namespace swift {
class SILFunction;

class IsSelfRecursive {
  const SILFunction *f;
  bool didComputeValue = false;
  bool isSelfRecursive = false;

  void compute();

public:
  IsSelfRecursive(const SILFunction *f) : f(f) {}

  ~IsSelfRecursive();

  bool isComputed() const { return didComputeValue; }

  bool get() {
    if (!didComputeValue) {
      compute();
      didComputeValue = true;
    }
    return isSelfRecursive;
  }

  SILFunction *getFunction() { return const_cast<SILFunction *>(f); }
};

class IsSelfRecursiveAnalysis final
    : public FunctionAnalysisBase<IsSelfRecursive> {
public:
  IsSelfRecursiveAnalysis()
      : FunctionAnalysisBase<IsSelfRecursive>(
            SILAnalysisKind::IsSelfRecursive) {}

  IsSelfRecursiveAnalysis(const IsSelfRecursiveAnalysis &) = delete;
  IsSelfRecursiveAnalysis &operator=(const IsSelfRecursiveAnalysis &) = delete;

  static SILAnalysisKind getAnalysisKind() {
    return SILAnalysisKind::IsSelfRecursive;
  }

  static bool classof(const SILAnalysis *s) {
    return s->getKind() == SILAnalysisKind::IsSelfRecursive;
  }

  std::unique_ptr<IsSelfRecursive> newFunctionAnalysis(SILFunction *f) override {
    return std::make_unique<IsSelfRecursive>(f);
  }

  bool shouldInvalidate(SILAnalysis::InvalidationKind k) override {
    return k & InvalidationKind::Calls;
  }
};

} // namespace swift

#endif
