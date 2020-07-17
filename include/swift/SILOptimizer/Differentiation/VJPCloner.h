//===--- VJPCloner.h - VJP function generation ----------------*- C++ -*---===//
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
// This file defines a helper class for generating VJP functions for automatic
// differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_VJPCLONER_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_VJPCLONER_H

#include "swift/SILOptimizer/Analysis/DifferentiableActivityAnalysis.h"
#include "swift/SILOptimizer/Differentiation/DifferentiationInvoker.h"
#include "swift/SILOptimizer/Differentiation/LinearMapInfo.h"

namespace swift {
namespace autodiff {

class ADContext;
class PullbackCloner;

/// A helper class for generating VJP functions.
class VJPCloner final {
  class Implementation;
  Implementation &impl;

public:
  /// Creates a VJP cloner.
  ///
  /// The parent VJP cloner stores the original function and an empty
  /// to-be-generated pullback function.
  explicit VJPCloner(ADContext &context, SILFunction *original,
                     SILDifferentiabilityWitness *witness, SILFunction *vjp,
                     DifferentiationInvoker invoker);
  ~VJPCloner();

  ADContext &getContext() const;
  SILModule &getModule() const;
  SILFunction &getOriginal() const;
  SILFunction &getVJP() const;
  SILFunction &getPullback() const;
  SILDifferentiabilityWitness *getWitness() const;
  const SILAutoDiffIndices getIndices() const;
  DifferentiationInvoker getInvoker() const;
  LinearMapInfo &getPullbackInfo() const;
  const DifferentiableActivityInfo &getActivityInfo() const;

  /// Performs VJP generation on the empty VJP function. Returns true if any
  /// error occurs.
  bool run();
};

} // end namespace autodiff
} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_VJPCLONER_H
