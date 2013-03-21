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

/// ManagedValue - represents a SIL rvalue. It consists of a Value and an
/// optional cleanup. Ownership of the ManagedValue can be "forwarded" to
/// disable its cleanup when the rvalue is consumed. Read-only addresses may
/// also be stored in a ManagedValue, but for general lvalues, the LValue type
/// must be used instead, which also handles writeback through logical
/// properties.
class ManagedValue {
  /// The value (or address of an address-only value) being managed, and
  /// whether it represents an address-only value.
  llvm::PointerIntPair<Value, 1, bool> valueAndIsAddressOnlyValue;
  /// A handle to the cleanup that destroys this value, or
  /// CleanupsDepth::invalid if the value has no cleanup.
  CleanupsDepth cleanup;

public:
  ManagedValue() = default;
  explicit ManagedValue(Value value, bool addressOnlyValue = false)
    : valueAndIsAddressOnlyValue(value, addressOnlyValue),
      cleanup(CleanupsDepth::invalid())
  {}
  ManagedValue(Value value, CleanupsDepth cleanup,
               bool isAddressOnlyValue = false)
    : valueAndIsAddressOnlyValue(value, isAddressOnlyValue),
      cleanup(cleanup)
  {}

  Value getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  Value getValue() const { return valueAndIsAddressOnlyValue.getPointer(); }
  
  SILType getType() const { return getValue().getType(); }
  
  bool isAddressOnlyValue() const { return valueAndIsAddressOnlyValue.getInt(); }

  bool hasCleanup() const { return cleanup.isValid(); }
  CleanupsDepth getCleanup() const { return cleanup; }

  /// Disable the cleanup for this value.
  void forwardCleanup(SILGenFunction &gen) {
    assert(hasCleanup() && "value doesn't have cleanup!");
    gen.Cleanups.setCleanupState(getCleanup(), CleanupState::Dead);
  }
  
  /// Forward this value, deactivating the cleanup and returning the
  /// underlying value. Not valid for address-only values.
  Value forward(SILGenFunction &gen) {
    assert(!isAddressOnlyValue() &&
           "must forward an address-only value using forwardInto");
    if (hasCleanup())
      forwardCleanup(gen);
    return getValue();
  }
  
  /// Forward this value if it's loadable, or create a temporary copy if
  /// it's address-only, per the argument passing convention.
  Value forwardArgument(SILGenFunction &gen, SILLocation loc) {
    if (isAddressOnlyValue()) {
      // If the value is already take-able, we don't need to make another copy.
      // Just pass cleanup responsibility to the callee.
      if (hasCleanup()) {
        forwardCleanup(gen);
        return getValue();
      }
      
      // Make a copy of the address-only value for the callee to consume.
      Value copy = gen.emitTemporaryAllocation(loc, getType());
      forwardInto(gen, loc, copy, /*isInitialize*/ true);
      return copy;
    } else if (getType().isAddress()) {
      // Simply pass other addresses--they should correspond to byref args.
      return getValue();
    } else {
      // Forward loadable values.
      Value v = forward(gen);
      // Thicken thin function values.
      // FIXME: Swift type-checking should do this.
      if (v.getType().is<AnyFunctionType>() &&
          v.getType().castTo<AnyFunctionType>()->isThin()) {
        v = gen.emitThickenFunction(loc, v);
      }
      return v;
    }
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
  ///
  /// FIXME: Ideally we would initialize directly into destinations and not need
  /// this method.
  void forwardInto(SILGenFunction &gen, SILLocation loc,
                   Value address, bool isInitialize) {
    assert(isAddressOnlyValue() &&
           "must forward loadable value using forward");
    // If we own a cleanup for this value, we can "take" the value and disable
    // the cleanup.
    bool canTake = hasCleanup();
    if (canTake) {
      forwardCleanup(gen);
    }
    gen.B.createCopyAddr(loc, getValue(), address, canTake, isInitialize);
  }

  /// Returns true if this value corresponds to an lvalue in Swift.
  bool isLValue() const {
    return getType().isAddress() && !isAddressOnlyValue();
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
