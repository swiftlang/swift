//===--- NullablePtr.h - A pointer that allows null -------------*- C++ -*-===//
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
// This file defines and implements the NullablePtr class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_NULLABLEPTR_H
#define SWIFT_BASIC_NULLABLEPTR_H

#include <cassert>
#include <cstddef>
#include <type_traits>
#include "llvm/Support/PointerLikeTypeTraits.h"

namespace swift {
/// NullablePtr pointer wrapper - NullablePtr is used for APIs where a
/// potentially-null pointer gets passed around that must be explicitly handled
/// in lots of places.  By putting a wrapper around the null pointer, it makes
/// it more likely that the null pointer case will be handled correctly.
template<class T>
class NullablePtr {
  T *Ptr;
  struct PlaceHolder {};

public:
  NullablePtr(T *P = 0) : Ptr(P) {}

  template<typename OtherT>
  NullablePtr(NullablePtr<OtherT> Other,
              typename std::enable_if<
                std::is_convertible<OtherT *, T *>::value,
                PlaceHolder
              >::type = PlaceHolder()) : Ptr(Other.getPtrOrNull()) {}

  bool isNull() const { return Ptr == 0; }
  bool isNonNull() const { return Ptr != 0; }

  /// get - Return the pointer if it is non-null.
  const T *get() const {
    assert(Ptr && "Pointer wasn't checked for null!");
    return Ptr;
  }

  /// get - Return the pointer if it is non-null.
  T *get() {
    assert(Ptr && "Pointer wasn't checked for null!");
    return Ptr;
  }

  T *getPtrOrNull() { return getPtrOr(nullptr); }
  const T *getPtrOrNull() const { return getPtrOr(nullptr); }

  T *getPtrOr(T *defaultValue) { return Ptr ? Ptr : defaultValue; }
  const T *getPtrOr(const T *defaultValue) const {
    return Ptr ? Ptr : defaultValue;
  }

  explicit operator bool() const { return Ptr; }

  bool operator==(const NullablePtr<T> &other) const {
    return other.Ptr == Ptr;
  }

  bool operator!=(const NullablePtr<T> &other) const {
    return !(*this == other);
  }

  bool operator==(const T *other) const { return other == Ptr; }

  bool operator!=(const T *other) const { return !(*this == other); }
};

} // end namespace swift

namespace llvm {
template <typename T> struct PointerLikeTypeTraits;
template <typename T> struct PointerLikeTypeTraits<swift::NullablePtr<T>> {
public:
  static inline void *getAsVoidPointer(swift::NullablePtr<T> ptr) {
    return static_cast<void *>(ptr.getPtrOrNull());
  }
  static inline swift::NullablePtr<T> getFromVoidPointer(void *ptr) {
    return swift::NullablePtr<T>(static_cast<T*>(ptr));
  }
  enum { NumLowBitsAvailable = PointerLikeTypeTraits<T *>::NumLowBitsAvailable };
};

}

#endif // SWIFT_BASIC_NULLABLEPTR_H
