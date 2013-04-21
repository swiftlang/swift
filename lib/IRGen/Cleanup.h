//===--- Cleanup.h - Declarations for Cleanup IR Generation -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Cleanup type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLEANUP_H
#define SWIFT_IRGEN_CLEANUP_H

#include "IRGenFunction.h"

namespace swift {
namespace irgen {


/// A Cleanup is an object placed on IRGenFunction's cleanups stack to
/// cause something to occur when a scope or full-expression is
/// concluded.
class Cleanup {
  unsigned AllocatedSize;

 protected:
  Cleanup() {}
  virtual ~Cleanup() {}

public:
  /// Return the allocated size of this object.  This is required by
  /// DiverseStack for iteration.
  size_t allocated_size() const { return AllocatedSize; }

private:
  virtual void _anchor();
};

} // end namespace irgen
} // end namespace swift

#endif
