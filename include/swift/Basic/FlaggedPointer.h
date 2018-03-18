//===--- FlaggedPointer.h - Explicit pointer tagging container --*- C++ -*-===//
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
// This file defines the FlaggedPointer class.
//
//===----------------------------------------------------------------------===//
//
#ifndef SWIFT_BASIC_FLAGGEDPOINTER_H
#define SWIFT_BASIC_FLAGGEDPOINTER_H

#include <cassert>

#include "llvm/Support/Compiler.h"
#include "llvm/Support/PointerLikeTypeTraits.h"

#include "Algorithm.h"

namespace swift {

/// This class implements a pair of a pointer and boolean flag.
/// Like PointerIntPair, it represents this by mangling a bit into the low part
/// of the pointer, taking advantage of pointer alignment. Unlike
/// PointerIntPair, you must specify the bit position explicitly, instead of
/// automatically placing an integer into the highest bits possible.
///
/// Composing this with `PointerIntPair` is not allowed.
template <typename PointerTy,
          unsigned BitPosition,
          typename PtrTraits = llvm::PointerLikeTypeTraits<PointerTy>>
class FlaggedPointer {
  intptr_t Value;
  static_assert(PtrTraits::NumLowBitsAvailable > 0,
                "Not enough bits to store flag at this position");
  enum : uintptr_t {
    FlagMask = (uintptr_t)1 << BitPosition,
    PointerBitMask = ~FlagMask
  };
public:
  FlaggedPointer() : Value(0) {}
  FlaggedPointer(PointerTy PtrVal, bool FlagVal) {
    setPointerAndFlag(PtrVal, FlagVal);
  }
  explicit FlaggedPointer(PointerTy PtrVal) {
    initWithPointer(PtrVal);
  }

  /// Returns the underlying pointer with the flag bit masked out.
  PointerTy getPointer() const {
    return PtrTraits::getFromVoidPointer(
      reinterpret_cast<void*>(Value & PointerBitMask));
  }

  void setPointer(PointerTy PtrVal) {
    intptr_t PtrWord = reinterpret_cast<intptr_t>(
      PtrTraits::getAsVoidPointer(PtrVal));
    assert((PtrWord & ~PointerBitMask) == 0 &&
           "Pointer is not sufficiently aligned");
    Value = PtrWord | (Value & ~PointerBitMask);
  }

  bool getFlag() const {
    return (bool)(Value & FlagMask);
  }

  void setFlag(bool FlagVal) {
    intptr_t FlagWord = static_cast<intptr_t>(FlagVal);

    Value &= ~FlagMask;
    Value |= FlagWord << BitPosition;
  }

  /// Set the pointer value and assert if it overlaps with
  /// the flag's bit position.
  void initWithPointer(PointerTy PtrVal) {
    intptr_t PtrWord = reinterpret_cast<intptr_t>(
      PtrTraits::getAsVoidPointer(PtrVal));
    assert((PtrWord & ~PointerBitMask) == 0 &&
      "Pointer is not sufficiently aligned");
    Value = PtrWord;
  }

  /// Set the pointer value, set the flag, and assert
  /// if the pointer's value would overlap with the flag's
  /// bit position.
  void setPointerAndFlag(PointerTy PtrVal, bool FlagVal) {
    intptr_t PtrWord = reinterpret_cast<intptr_t>(
      PtrTraits::getAsVoidPointer(PtrVal));
    assert((PtrWord & ~PointerBitMask) == 0 &&
      "Pointer is not sufficiently aligned");
    intptr_t FlagWord = static_cast<intptr_t>(FlagVal);

    Value = PtrWord | (FlagWord << BitPosition);
  }

  PointerTy const *getAddrOfPointer() const {
    return const_cast<FlaggedPointer *>(this)->getAddrOfPointer();
  }

  PointerTy *getAddrOfPointer() {
    assert(Value == reinterpret_cast<intptr_t>(getPointer()) &&
      "Can only return the address if IntBits is cleared and "
      "PtrTraits doesn't change the pointer");
    return reinterpret_cast<PointerTy *>(&Value);
  }

  /// Get the raw pointer value for the underlying pointer
  /// including its flag value.
  void *getOpaqueValue() const {
    return reinterpret_cast<void*>(Value);
  }

  void setFromOpaqueValue(void *Val) {
    Value = reinterpret_cast<intptr_t>(Val);
  }

  static FlaggedPointer getFromOpaqueValue(const void *V) {
    FlaggedPointer P;
    P.setFromOpaqueValue(const_cast<void *>(V));
    return P;
  }

  bool operator==(const FlaggedPointer &RHS) const {
    return Value == RHS.Value;
  }
  bool operator!=(const FlaggedPointer &RHS) const {
    return Value != RHS.Value;
  }
  bool operator<(const FlaggedPointer &RHS) const {
    return Value < RHS.Value;
  }
  bool operator>(const FlaggedPointer &RHS) const {
    return Value > RHS.Value;
  }
  bool operator<=(const FlaggedPointer &RHS) const {
    return Value <= RHS.Value;
  }
  bool operator>=(const FlaggedPointer &RHS) const {
    return Value >= RHS.Value;
  }
};

} // end namespace swift

// Teach SmallPtrSet that FlaggedPointer is "basically a pointer".
template <typename PointerTy, unsigned BitPosition, typename PtrTraits>
struct llvm::PointerLikeTypeTraits<
  swift::FlaggedPointer<PointerTy, BitPosition, PtrTraits>> {
public:
  static inline void *
    getAsVoidPointer(const swift::FlaggedPointer<PointerTy, BitPosition> &P) {
      return P.getOpaqueValue();
  }
  static inline swift::FlaggedPointer<PointerTy, BitPosition>
    getFromVoidPointer(void *P) {
      return swift::FlaggedPointer<PointerTy, BitPosition>::getFromOpaqueValue(P);
  }
  static inline swift::FlaggedPointer<PointerTy, BitPosition>
    getFromVoidPointer(const void *P) {
      return swift::FlaggedPointer<PointerTy, BitPosition>::getFromOpaqueValue(P);
  }
  enum {
    NumLowBitsAvailable = (BitPosition >= PtrTraits::NumLowBitsAvailable)
      ? PtrTraits::NumLowBitsAvailable
      : (swift::min(int(BitPosition + 1),
        int(PtrTraits::NumLowBitsAvailable)) - 1)
  };
};

#endif // SWIFT_BASIC_FLAGGEDPOINTER_H
