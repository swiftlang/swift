//===--- ManagedValue.h - Exploded R-Value Representation -------*- C++ -*-===//
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
// A storage structure for holding an rvalue with an optional cleanup.
// Ownership of the rvalue can be "forward" to disable the associated cleanup.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_MANAGEDVALUE_H
#define SWIFT_LOWERING_MANAGEDVALUE_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "SILGen.h"

namespace swift {
  class Value;
  
namespace Lowering {

/// ManagedValue - represents a SIL rvalue. It constists of a Value and an
/// optional cleanup. Ownership of the ManagedValue can be "forwarded" to
/// disable its cleanup when the rvalue is consumed. Read-only addresses may
/// also be stored in a ManagedValue, but for general lvalues, the LValue type
/// must be used instead, which also handles writeback through logical
/// properties.
class ManagedValue {
  Value value;
  CleanupsDepth cleanup;

public:
  ManagedValue() = default;
  explicit ManagedValue(Value value)
    : value(value), cleanup(CleanupsDepth::invalid()) {}
  ManagedValue(Value value, CleanupsDepth cleanup)
    : value(value), cleanup(cleanup) {}

  Value getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  Value getValue() const { return value; }

  bool hasCleanup() const { return cleanup.isValid(); }
  CleanupsDepth getCleanup() const { return cleanup; }

  /// Forward this value, deactivating the cleanup and returning the
  /// underlying value.
  Value forward(SILGenFunction &gen) {
    if (hasCleanup())
      gen.Cleanups.setCleanupState(getCleanup(), CleanupState::Dead);
    return getValue();
  }

  /// Split this value into its underlying value and, if present, its cleanup.
  Value split(llvm::SmallVectorImpl<CleanupsDepth> &cleanups) {
    if (hasCleanup()) cleanups.push_back(getCleanup());
    return getValue();
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
