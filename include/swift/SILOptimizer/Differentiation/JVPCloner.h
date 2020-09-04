//===--- JVPCloner.h - JVP function generation ----------------*- C++ -*---===//
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
// This file defines a helper class for generating JVP functions for automatic
// differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_JVPCLONER_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_JVPCLONER_H

#include "swift/SILOptimizer/Differentiation/DifferentiationInvoker.h"

namespace swift {

class SILFunction;
class SILDifferentiabilityWitness;

namespace autodiff {

class ADContext;

/// A helper class for generating JVP functions.
class JVPCloner final {
  class Implementation;
  Implementation &impl;

public:
  /// Creates a JVP cloner.
  ///
  /// The parent JVP cloner stores the original function and an empty
  /// to-be-generated pullback function.
  explicit JVPCloner(ADContext &context, SILFunction *original,
                     SILDifferentiabilityWitness *witness, SILFunction *jvp,
                     DifferentiationInvoker invoker);
  ~JVPCloner();

  /// Performs JVP generation on the empty JVP function. Returns true if any
  /// error occurs.
  bool run();
};

} // end namespace autodiff
} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_JVPCLONER_H
