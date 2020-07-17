//===--- PullbackCloner.h - Pullback function generation -----*- C++ -*----===//
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
// This file defines a helper class for generating pullback functions for
// automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKCLONER_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKCLONER_H

namespace swift {
namespace autodiff {

class VJPCloner;

/// A helper class for generating pullback functions.
class PullbackCloner final {
  class Implementation;
  Implementation &impl;

public:
  /// Creates a pullback cloner from a parent VJP cloner.
  ///
  /// The parent VJP cloner stores the original function and an empty
  /// to-be-generated pullback function.
  explicit PullbackCloner(VJPCloner &vjpCloner);
  ~PullbackCloner();

  /// Performs pullback generation on the empty pullback function. Returns true
  /// if any error occurs.
  bool run();
};

} // end namespace autodiff
} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_PULLBACKCLONER_H
