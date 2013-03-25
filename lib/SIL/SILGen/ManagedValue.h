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
  /// whether it represents an lvalue.
  llvm::PointerIntPair<Value, 1, bool> valueAndIsLValue;
  /// A handle to the cleanup that destroys this value, or
  /// CleanupsDepth::invalid if the value has no cleanup.
  CleanupsDepth cleanup;

public:
  enum Unmanaged_t { Unmanaged };
  enum LValue_t { LValue };
  
  ManagedValue() = default;
  explicit ManagedValue(Value value, LValue_t)
    : valueAndIsLValue(value, true),
      cleanup(CleanupsDepth::invalid())
  {}
  explicit ManagedValue(Value value, Unmanaged_t)
    : valueAndIsLValue(value, false),
      cleanup(CleanupsDepth::invalid())
  {}
  ManagedValue(Value value, CleanupsDepth cleanup)
    : valueAndIsLValue(value, false),
      cleanup(cleanup)
  {}

  Value getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  Value getValue() const { return valueAndIsLValue.getPointer(); }
  
  SILType getType() const { return getValue().getType(); }
  
  bool isLValue() const { return valueAndIsLValue.getInt(); }

  bool hasCleanup() const { return cleanup.isValid(); }
  CleanupsDepth getCleanup() const { return cleanup; }

  /// Disable the cleanup for this value.
  void forwardCleanup(SILGenFunction &gen) {
    assert(hasCleanup() && "value doesn't have cleanup!");
    gen.Cleanups.setCleanupState(getCleanup(), CleanupState::Dead);
  }
  
  /// Forward this value, deactivating the cleanup and returning the
  /// underlying value.
  Value forward(SILGenFunction &gen) {
    if (hasCleanup())
      forwardCleanup(gen);
    return getValue();
  }
  
  /// Forward this value as an argument.
  Value forwardArgument(SILGenFunction &gen, SILLocation loc) {
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
    assert(getType().isAddressOnly() &&
           "must forward loadable value using forward");
    // If we own a cleanup for this value, we can "take" the value and disable
    // the cleanup.
    bool canTake = hasCleanup();
    if (canTake) {
      forwardCleanup(gen);
    }
    gen.B.createCopyAddr(loc, getValue(), address, canTake, isInitialize);
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
