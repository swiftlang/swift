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
  /// The value (or address of an address-only value) being managed.
  Value value;
  /// A handle to the cleanup that destroys this value, or
  /// CleanupsDepth::invalid if the value has no cleanup.
  CleanupsDepth cleanup;
  /// True if this ManagedValue represents an address-only value.
  bool addressOnlyValue : 1;

public:
  ManagedValue() = default;
  explicit ManagedValue(Value value, bool addressOnlyValue = false)
    : value(value), cleanup(CleanupsDepth::invalid()),
      addressOnlyValue(addressOnlyValue)
    {}
  ManagedValue(Value value, CleanupsDepth cleanup,
               bool isAddressOnlyValue = false)
    : value(value), cleanup(cleanup), addressOnlyValue(isAddressOnlyValue)
    {}

  Value getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  Value getValue() const { return value; }
  
  SILType getType() const { return value.getType(); }
  
  bool isAddressOnlyValue() const { return addressOnlyValue; }

  bool hasCleanup() const { return cleanup.isValid(); }
  CleanupsDepth getCleanup() const { return cleanup; }

  /// Forward this value, deactivating the cleanup and returning the
  /// underlying value. Not valid for address-only values.
  Value forward(SILGenFunction &gen) {
    assert(!addressOnlyValue &&
           "must forward an address-only value using forwardInto");
    if (hasCleanup())
      gen.Cleanups.setCleanupState(getCleanup(), CleanupState::Dead);
    return getValue();
  }
  
  /// Forward this value into memory by storing it to the given address.
  /// Currently only implemented for address-only values.
  ///
  /// \param gen - The SILGenFunction.
  /// \param loc - the AST location to associate with emitted instructions.
  /// \param address - the address to store to.
  /// \param isInitialize - True if the address references uninitialized memory.
  ///                       False if the address currently contains a valid
  ///                       value.
  void forwardInto(SILGenFunction &gen, SILLocation loc,
                   Value address, bool isInitialize) {
    assert(addressOnlyValue &&
           "must forward loadable value using forward");
    // If we own a cleanup for this value, we can "take" the value and disable
    // the cleanup.
    bool canTake = hasCleanup();
    if (canTake) {
      gen.Cleanups.setCleanupState(getCleanup(), CleanupState::Dead);
    }
    gen.B.createCopyAddr(loc, value, address, canTake, isInitialize);
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
