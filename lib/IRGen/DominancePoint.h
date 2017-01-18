//===--- DominancePoint.h - Dominance points --------------------*- C++ -*-===//
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
//
//  This file defines types relating to local dominance calculations
//  during the emission of a function.
//
//  During the emission of a function, the LLVM IR is not well-formed enough
//  to do accurate dominance computations.  For example, a basic block may
//  appear to have a single predecessor, but that may be because a different
//  predecessor has not yet been added.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_DOMINANCEPOINT_H
#define SWIFT_IRGEN_DOMINANCEPOINT_H

#include <cassert>
#include <cstdint>

namespace swift {
namespace irgen {
  class IRGenFunction;

/// An opaque class for storing keys for the dominance callback.  The
/// key is assumed to be something like a (uniqued) pointer, and a
/// null pointer is assumed to mean a non-dominating point.
class DominancePoint {
  uintptr_t Value;
  enum : uintptr_t {
    Universal = 0,
  };
  explicit DominancePoint(uintptr_t value) : Value(value) {}
public:
  explicit DominancePoint(void *value)
      : Value(reinterpret_cast<uintptr_t>(value)) {
    assert(isOrdinary());
  }

  /// Something about the definition is known to dominate all possible
  /// places that will use it.
  static DominancePoint universal() { return DominancePoint(Universal); }

  bool isOrdinary() const {
    return Value != Universal;
  }
  bool isUniversal() const {
    return Value == Universal;
  }

  template <class T> T* as() const {
    assert(isOrdinary());
    return reinterpret_cast<T*>(Value);
  }
  bool operator==(DominancePoint other) const { return Value == other.Value; }
};

/// A dominance resolver is a function that answers the question of
/// whether one dominance point dominates another.
///
/// It will only be asked this question with ordinary dominance points.
using DominanceResolverFunction = bool(*)(IRGenFunction &IGF,
                                          DominancePoint curPoint,
                                          DominancePoint definingPoint);

}
}

#endif
