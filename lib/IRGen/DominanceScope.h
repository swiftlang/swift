//===--- DominanceScope.h - Dominance scoping -------------------*- C++ -*-===//
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

#include <stdint.h>

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
    Unknown = 1,
  };
  explicit DominancePoint(uintptr_t value) : Value(value) {}
public:
  explicit DominancePoint(void *value)
      : Value(reinterpret_cast<uintptr_t>(value)) {
    assert(isOrdinary());
  }

  /// Something about the definition is known to dominate all possible
  /// places that will use it.
  static DominancePoint universal() { return DominanceKey(Universal); }

  /// This definition point has non-obvious dominance rules; don't put
  /// anything here and assume it'll dominate other things.  This should be
  /// used when IRGen adds its own control flow that might interact awkwardly
  /// with dominance.
  static DominancePoint unknown() { return DominanceKey(Unknown); }

  bool isOrdinary() {
    return Value > Uncacheable;
  }
  bool isUniversal() {
    return Value == Universal;
  }
  bool isUnknown() {
    return Value == Unknown;
  }

  template <class T> T* as() const {
    assert(isOrdinary());
    return reinterpret_cast<T*>(Value);
  }
  bool operator==(DominancePoint other) const { return Value == other.Value; }
};

using DominanceResolverFunction = bool(*)(IRGenFunction &IGF,
                                          DominancePoint curPoint,
                                          DominancePoint definingPoint);

}
}

#endif
