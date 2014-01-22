//===--- RValue.h - Exploded RValue Representation --------------*- C++ -*-===//
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
// A storage structure for holding a destructured rvalue with an optional
// cleanup(s).
// Ownership of the rvalue can be "forwarded" to disable the associated
// cleanup(s).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_MANAGEDVALUE_H
#define SWIFT_LOWERING_MANAGEDVALUE_H

#include "Cleanup.h"
#include "llvm/ADT/PointerIntPair.h"
#include "swift/SIL/SILValue.h"


namespace swift {
  class SILValue;
  
namespace Lowering {

/// ManagedValue - represents a SIL rvalue. It consists of a SILValue and an
/// optional cleanup. Ownership of the ManagedValue can be "forwarded" to
/// disable its cleanup when the rvalue is consumed. A ManagedValue can also
/// represent an LValue used as a value, such as a @inout function argument.
class ManagedValue {
  /// The value (or address of an address-only value) being managed, and
  /// whether it represents an lvalue.
  llvm::PointerIntPair<SILValue, 1, bool> valueAndIsLValue;
  /// A handle to the cleanup that destroys this value, or
  /// CleanupHandle::invalid() if the value has no cleanup.
  CleanupHandle cleanup;

public:
  enum Unmanaged_t { Unmanaged };
  enum LValue_t { LValue };
  
  ManagedValue() = default;
  explicit ManagedValue(SILValue value, LValue_t)
    : valueAndIsLValue(value, true),
      cleanup(CleanupHandle::invalid())
  {}
  explicit ManagedValue(SILValue value, Unmanaged_t)
    : valueAndIsLValue(value, false),
      cleanup(CleanupHandle::invalid())
  {}
  ManagedValue(SILValue value, CleanupHandle cleanup)
    : valueAndIsLValue(value, false),
      cleanup(cleanup)
  {}

  static ManagedValue forUnmanaged(SILValue value) {
    return ManagedValue(value, Unmanaged);
  }
  static ManagedValue forLValue(SILValue value) {
    return ManagedValue(value, LValue);
  }

  SILValue getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  SILValue getValue() const { return valueAndIsLValue.getPointer(); }
  
  SILType getType() const { return getValue().getType(); }
  
  bool isLValue() const { return valueAndIsLValue.getInt(); }

  CanType getSwiftType() const {
    return isLValue()
      ? getType().getSwiftType()
      : getType().getSwiftRValueType();
  }
  
  /// Emit a copy of this value with independent ownership.
  ManagedValue copy(SILGenFunction &gen, SILLocation l);
  
  /// Store a copy of this value with independent ownership into the given
  /// uninitialized address.
  void copyInto(SILGenFunction &gen, SILValue dest, SILLocation L);
  
  /// This is the same operation as 'copy', but works on +0 values that don't
  /// have cleanups.  It returns a +1 value with one.
  ManagedValue copyUnmanaged(SILGenFunction &gen, SILLocation loc);
  
  bool hasCleanup() const { return cleanup.isValid(); }
  CleanupHandle getCleanup() const { return cleanup; }

  /// Disable the cleanup for this value.
  void forwardCleanup(SILGenFunction &gen) const;
  
  /// Forward this value, deactivating the cleanup and returning the
  /// underlying value.
  SILValue forward(SILGenFunction &gen) const;
  
  /// Forward this value as an argument.
  SILValue forwardArgument(SILGenFunction &gen, SILLocation loc,
                           AbstractCC cc, CanType origNativeTy,
                           CanType substNativeTy, CanType bridgedTy);
  
  /// Get this value as an argument without consuming it.
  SILValue getArgumentValue(SILGenFunction &gen, SILLocation loc,
                            AbstractCC cc,
                            CanType origNativeTy, CanType substNativeTy,
                            CanType bridgedTy);
  
  /// Forward this value into memory by storing it to the given address.
  ///
  /// \param gen - The SILGenFunction.
  /// \param loc - the AST location to associate with emitted instructions.
  /// \param address - the address to assign to.
  void forwardInto(SILGenFunction &gen, SILLocation loc, SILValue address);
  
  /// Assign this value into memory, destroying the existing
  /// value at the destination address.
  ///
  /// \param gen - The SILGenFunction.
  /// \param loc - the AST location to associate with emitted instructions.
  /// \param address - the address to assign to.
  void assignInto(SILGenFunction &gen, SILLocation loc, SILValue address);
  
  explicit operator bool() const {
    return bool(getValue());
  }
};

  
} // end namespace Lowering
} // end namespace swift

#endif
