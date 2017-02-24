//===--- ManagedValue.cpp - Value with cleanup ----------------------------===//
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
// A storage structure for holding a destructured rvalue with an optional
// cleanup(s).
// Ownership of the rvalue can be "forwarded" to disable the associated
// cleanup(s).
//
//===----------------------------------------------------------------------===//

#include "ManagedValue.h"
#include "SILGenFunction.h"
using namespace swift;
using namespace Lowering;

/// Emit a copy of this value with independent ownership.
ManagedValue ManagedValue::copy(SILGenFunction &gen, SILLocation loc) {
  auto &lowering = gen.getTypeLowering(getType());
  if (lowering.isTrivial())
    return *this;

  if (getType().isObject()) {
    return gen.B.createCopyValue(loc, *this, lowering);
  }

  SILValue buf = gen.emitTemporaryAllocation(loc, getType());
  gen.B.createCopyAddr(loc, getValue(), buf, IsNotTake, IsInitialization);
  return gen.emitManagedRValueWithCleanup(buf, lowering);
}

/// Emit a copy of this value with independent ownership.
ManagedValue ManagedValue::formalAccessCopy(SILGenFunction &gen,
                                            SILLocation loc) {
  assert(gen.InWritebackScope && "Can only perform a formal access copy in a "
                                 "formal evaluation scope");
  auto &lowering = gen.getTypeLowering(getType());
  if (lowering.isTrivial())
    return *this;

  if (getType().isObject()) {
    return gen.B.createFormalAccessCopyValue(loc, *this);
  }

  SILValue buf = gen.emitTemporaryAllocation(loc, getType());
  return gen.B.createFormalAccessCopyAddr(loc, *this, buf, IsNotTake,
                                          IsInitialization);
}

/// Store a copy of this value with independent ownership into the given
/// uninitialized address.
void ManagedValue::copyInto(SILGenFunction &gen, SILValue dest,
                            SILLocation loc) {
  auto &lowering = gen.getTypeLowering(getType());
  if (lowering.isAddressOnly()) {
    gen.B.createCopyAddr(loc, getValue(), dest, IsNotTake, IsInitialization);
    return;
  }

  SILValue copy = lowering.emitCopyValue(gen.B, loc, getValue());
  lowering.emitStoreOfCopy(gen.B, loc, copy, dest, IsInitialization);
}

/// This is the same operation as 'copy', but works on +0 values that don't
/// have cleanups.  It returns a +1 value with one.
ManagedValue ManagedValue::copyUnmanaged(SILGenFunction &gen, SILLocation loc) {
  if (getType().isObject()) {
    return gen.B.createCopyValue(loc, *this);
  }

  SILValue result = gen.emitTemporaryAllocation(loc, getType());
  gen.B.createCopyAddr(loc, getValue(), result, IsNotTake, IsInitialization);
  return gen.emitManagedRValueWithCleanup(result);
}

/// This is the same operation as 'copy', but works on +0 values that don't
/// have cleanups.  It returns a +1 value with one.
ManagedValue ManagedValue::formalAccessCopyUnmanaged(SILGenFunction &gen,
                                                     SILLocation loc) {
  if (getType().isObject()) {
    return gen.B.createFormalAccessCopyValue(loc, *this);
  }

  SILValue result = gen.emitTemporaryAllocation(loc, getType());
  return gen.B.createFormalAccessCopyAddr(loc, *this, result, IsNotTake,
                                          IsInitialization);
}

/// Disable the cleanup for this value.
void ManagedValue::forwardCleanup(SILGenFunction &gen) const {
  assert(hasCleanup() && "value doesn't have cleanup!");
  gen.Cleanups.forwardCleanup(getCleanup());
}

/// Forward this value, deactivating the cleanup and returning the
/// underlying value.
SILValue ManagedValue::forward(SILGenFunction &gen) const {
  if (hasCleanup())
    forwardCleanup(gen);
  return getValue();
}

void ManagedValue::forwardInto(SILGenFunction &gen, SILLocation loc,
                               SILValue address) {
  if (hasCleanup())
    forwardCleanup(gen);
  auto &addrTL = gen.getTypeLowering(address->getType());
  gen.emitSemanticStore(loc, getValue(), address, addrTL, IsInitialization);
}

void ManagedValue::assignInto(SILGenFunction &gen, SILLocation loc,
                              SILValue address) {
  if (hasCleanup())
    forwardCleanup(gen);
  
  auto &addrTL = gen.getTypeLowering(address->getType());
  gen.emitSemanticStore(loc, getValue(), address, addrTL,
                        IsNotInitialization);
}

ManagedValue ManagedValue::borrow(SILGenFunction &gen, SILLocation loc) const {
  assert(getValue() && "cannot borrow an invalid or in-context value");
  if (isLValue())
    return *this;
  if (getType().isAddress())
    return ManagedValue::forUnmanaged(getValue());
  return gen.emitManagedBeginBorrow(loc, getValue());
}

ManagedValue ManagedValue::formalAccessBorrow(SILGenFunction &gen,
                                              SILLocation loc) const {
  assert(getValue() && "cannot borrow an invalid or in-context value");
  if (isLValue())
    return *this;
  if (getType().isAddress())
    return ManagedValue::forUnmanaged(getValue());
  return gen.emitFormalEvaluationManagedBeginBorrow(loc, getValue());
}
