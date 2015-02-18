//===- NullablePtr.h - A pointer that allows null ---------------*- C++ -*-===//
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
// This file defines and implements the NullablePtr class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_NULLABLEPTR_H
#define SWIFT_BASIC_NULLABLEPTR_H

#include <cassert>
#include <cstddef>
#include <type_traits>

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
                std::is_convertible<OtherT, T>::value,
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
  
  T *getPtrOrNull() { return Ptr; }
  const T *getPtrOrNull() const { return Ptr; }

  explicit operator bool() const { return Ptr; }
};
  
} // end namespace swift

#endif
