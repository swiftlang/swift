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
namespace Lowering {

/// ManagedValue - represents a singular SIL value and an optional cleanup.
/// Ownership of the ManagedValue can be "forwarded" to disable its cleanup when
/// the rvalue is consumed. A ManagedValue can also represent an LValue used as
/// a value, such as an inout function argument, and can be null.
///
/// Interesting relevant cases include:
///   LValue: the SILValue will always have an isAddress() SILType. LValues
///           never have an associated cleanup.
///   RValue, isAddress() type: an address-only RValue.
///   RValue, !isAddress() type: a loadable RValue.
///   "InContext": Represented with the lvalue flag set but with no SILValue,
///                this represents a value that was emitted directly into an
///                initialization stored by an SGFContext.
///
/// The RValue cases may or may not have a cleanup associated with the value.
/// A cleanup is associated with +1 values of non-trivial type.
///
class ManagedValue {
  /// The value (or address of an address-only value) being managed, and
  /// whether it represents an lvalue.  InContext is represented with the lvalue
  /// flag set but with a null SILValue.
  llvm::PointerIntPair<SILValue, 1, bool> valueAndFlag;
  
  /// A handle to the cleanup that destroys this value, or
  /// CleanupHandle::invalid() if the value has no cleanup.
  CleanupHandle cleanup;

  explicit ManagedValue(SILValue value, bool isLValue, CleanupHandle cleanup)
    : valueAndFlag(value, isLValue), cleanup(cleanup) {
  }

public:
  
  ManagedValue() = default;
  
  /// Create a managed value for a +1 rvalue.
  ManagedValue(SILValue value, CleanupHandle cleanup)
    : valueAndFlag(value, false), cleanup(cleanup) {
    assert(value && "No value specified");
  }

  /// Create a managed value for a +0 rvalue.
  static ManagedValue forUnmanaged(SILValue value) {
    assert(value && "No value specified");
    return ManagedValue(value, false, CleanupHandle::invalid());
  }
  /// Create a managed value for an l-value.
  static ManagedValue forLValue(SILValue value) {
    assert(value && "No value specified");
    assert(value.getType().isAddress() &&
           "lvalues always have isAddress() type");
    return ManagedValue(value, true, CleanupHandle::invalid());
  }
  
  /// Create a managed value that indicates that the value you're looking for
  /// got stored into an initialization specified by an SGFContext, instead of
  /// being represented by this ManagedValue.
  static ManagedValue forInContext() {
    return ManagedValue(SILValue(), true, CleanupHandle::invalid());
  }

  bool isLValue() const {
    return valueAndFlag.getInt() && valueAndFlag.getPointer();
  }
  bool isInContext() const {
    return valueAndFlag.getInt() && !valueAndFlag.getPointer();
  }

  /// Return true if this is an +0 rvalue, or has trivial type.
  bool isPlusZeroRValueOrTrivial() const {
    // If this is an lvalue or isInContext() then it is not an RValue.
    if (isLValue() || isInContext()) return false;
    
    // If this has a cleanup attached, then it is +1 rvalue.  If not, it is
    // either +0 or trivial (in which case +0 vs +1 doesn't matter).
    return !hasCleanup();
  }
  
  SILValue getLValueAddress() const {
    assert(isLValue() && "This isn't an lvalue");
    return getValue();
  }
  
  SILValue getUnmanagedValue() const {
    assert(!hasCleanup());
    return getValue();
  }
  SILValue getValue() const { return valueAndFlag.getPointer(); }
  
  SILType getType() const { return getValue().getType(); }
  

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
    // "InContext" is not considered false.
    return bool(getValue()) || valueAndFlag.getInt();
  }
};

  
} // end namespace Lowering
} // end namespace swift

#endif
