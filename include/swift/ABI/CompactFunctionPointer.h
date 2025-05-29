//===--- CompactFunctionPointer.h - Compact Function Pointers ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift's runtime structures often use relative function pointers to reduce the
// size of metadata and also to minimize load-time overhead in PIC.
// This file defines pointer types whose size and interface are compatible with
// the relative pointer types for targets that do not support relative references
// to code from data.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_COMPACTFUNCTIONPOINTER_H
#define SWIFT_ABI_COMPACTFUNCTIONPOINTER_H

namespace swift {

/// A compact unconditional absolute function pointer that can fit in a 32-bit
/// integer.
/// As a trade-off compared to relative pointers, this has load-time overhead in PIC
/// and is only available on 32-bit targets.
template <typename T, bool Nullable = false, typename Offset = int32_t>
class AbsoluteFunctionPointer {
  T *Pointer;
  static_assert(sizeof(T *) == sizeof(int32_t),
                "Function pointer must be 32-bit when using compact absolute pointer");

public:
  using PointerTy = T *;

  PointerTy get() const & { return Pointer; }

  operator PointerTy() const & { return this->get(); }

  bool isNull() const & { return Pointer == nullptr; }

  /// Resolve a pointer from a `base` pointer and a value loaded from `base`.
  template <typename BasePtrTy, typename Value>
  static PointerTy resolve(BasePtrTy *base, Value value) {
    return reinterpret_cast<PointerTy>(value);
  }

  template <typename... ArgTy>
  typename std::invoke_result<T *(ArgTy...)>::type operator()(ArgTy... arg) const {
    static_assert(std::is_function<T>::value,
                  "T must be an explicit function type");
    return this->get()(std::forward<ArgTy>(arg)...);
  }
};

// TODO(katei): Add another pointer structure for 64-bit targets and for efficiency on PIC

} // namespace swift

#endif // SWIFT_ABI_COMPACTFUNCTIONPOINTER_H
