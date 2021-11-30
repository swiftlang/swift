//===- Optional.h - Simple variant for passing optional values --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file provides Optional, a template class modeled in the spirit of
//  OCaml's 'opt' variant.  The idea is to strongly type whether or not
//  a value can be optional.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_OPTIONAL_H
#define LLVM_ADT_OPTIONAL_H

#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/type_traits.h"
#include <cassert>
#include <memory>
#include <new>
#include <utility>

namespace llvm {

class raw_ostream;

namespace optional_detail {

/// Storage for any type.
//
// The specialization condition intentionally uses
// llvm::is_trivially_copy_constructible instead of
// std::is_trivially_copy_constructible.  GCC versions prior to 7.4 may
// instantiate the copy constructor of `T` when
// std::is_trivially_copy_constructible is instantiated.  This causes
// compilation to fail if we query the trivially copy constructible property of
// a class which is not copy constructible.
//
// The current implementation of OptionalStorage insists that in order to use
// the trivial specialization, the value_type must be trivially copy
// constructible and trivially copy assignable due to =default implementations
// of the copy/move constructor/assignment.  It does not follow that this is
// necessarily the case std::is_trivially_copyable is true (hence the expanded
// specialization condition).
//
// The move constructible / assignable conditions emulate the remaining behavior
// of std::is_trivially_copyable.
template <typename T, bool = (llvm::is_trivially_copy_constructible<T>::value &&
                              std::is_trivially_copy_assignable<T>::value &&
                              (std::is_trivially_move_constructible<T>::value ||
                               !std::is_move_constructible<T>::value) &&
                              (std::is_trivially_move_assignable<T>::value ||
                               !std::is_move_assignable<T>::value))>
class OptionalStorage {
  union {
    char empty;
    T value;
  };
  bool hasVal;

public:
  ~OptionalStorage() { reset(); }

  constexpr OptionalStorage() noexcept : empty(), hasVal(false) {}

  constexpr OptionalStorage(OptionalStorage const &other) : OptionalStorage() {
    if (other.hasValue()) {
      emplace(other.value);
    }
  }
  constexpr OptionalStorage(OptionalStorage &&other) : OptionalStorage() {
    if (other.hasValue()) {
      emplace(std::move(other.value));
    }
  }

  template <class... Args>
  constexpr explicit OptionalStorage(in_place_t, Args &&... args)
      : value(std::forward<Args>(args)...), hasVal(true) {}

  void reset() noexcept {
    if (hasVal) {
      value.~T();
      hasVal = false;
    }
  }

  constexpr bool hasValue() const noexcept { return hasVal; }

  T &getValue() LLVM_LVALUE_FUNCTION noexcept {
    assert(hasVal);
    return value;
  }
  constexpr T const &getValue() const LLVM_LVALUE_FUNCTION noexcept {
    assert(hasVal);
    return value;
  }
#if LLVM_HAS_RVALUE_REFERENCE_THIS
  T &&getValue() && noexcept {
    assert(hasVal);
    return std::move(value);
  }
#endif

  template <class... Args> void emplace(Args &&... args) {
    reset();
    ::new ((void *)std::addressof(value)) T(std::forward<Args>(args)...);
    hasVal = true;
  }

  OptionalStorage &operator=(T const &y) {
    if (hasValue()) {
      value = y;
    } else {
      ::new ((void *)std::addressof(value)) T(y);
      hasVal = true;
    }
    return *this;
  }
  OptionalStorage &operator=(T &&y) {
    if (hasValue()) {
      value = std::move(y);
    } else {
      ::new ((void *)std::addressof(value)) T(std::move(y));
      hasVal = true;
    }
    return *this;
  }

  OptionalStorage &operator=(OptionalStorage const &other) {
    if (other.hasValue()) {
      if (hasValue()) {
        value = other.value;
      } else {
        ::new ((void *)std::addressof(value)) T(other.value);
        hasVal = true;
      }
    } else {
      reset();
    }
    return *this;
  }

  OptionalStorage &operator=(OptionalStorage &&other) {
    if (other.hasValue()) {
      if (hasValue()) {
        value = std::move(other.value);
      } else {
        ::new ((void *)std::addressof(value)) T(std::move(other.value));
        hasVal = true;
      }
    } else {
      reset();
    }
    return *this;
  }
};

template <typename T> class OptionalStorage<T, true> {
  union {
    char empty;
    T value;
  };
  bool hasVal = false;

public:
  ~OptionalStorage() = default;

  constexpr OptionalStorage() noexcept : empty{} {}

  constexpr OptionalStorage(OptionalStorage const &other) = default;
  constexpr OptionalStorage(OptionalStorage &&other) = default;

  OptionalStorage &operator=(OptionalStorage const &other) = default;
  OptionalStorage &operator=(OptionalStorage &&other) = default;

  template <class... Args>
  constexpr explicit OptionalStorage(in_place_t, Args &&... args)
      : value(std::forward<Args>(args)...), hasVal(true) {}

  void reset() noexcept {
    if (hasVal) {
      value.~T();
      hasVal = false;
    }
  }

  constexpr bool hasValue() const noexcept { return hasVal; }

  T &getValue() LLVM_LVALUE_FUNCTION noexcept {
    assert(hasVal);
    return value;
  }
  constexpr T const &getValue() const LLVM_LVALUE_FUNCTION noexcept {
    assert(hasVal);
    return value;
  }
#if LLVM_HAS_RVALUE_REFERENCE_THIS
  T &&getValue() && noexcept {
    assert(hasVal);
    return std::move(value);
  }
#endif

  template <class... Args> void emplace(Args &&... args) {
    reset();
    ::new ((void *)std::addressof(value)) T(std::forward<Args>(args)...);
    hasVal = true;
  }

  OptionalStorage &operator=(T const &y) {
    if (hasValue()) {
      value = y;
    } else {
      ::new ((void *)std::addressof(value)) T(y);
      hasVal = true;
    }
    return *this;
  }
  OptionalStorage &operator=(T &&y) {
    if (hasValue()) {
      value = std::move(y);
    } else {
      ::new ((void *)std::addressof(value)) T(std::move(y));
      hasVal = true;
    }
    return *this;
  }
};

} // namespace optional_detail

template <typename T> class Optional {
  optional_detail::OptionalStorage<T> Storage;

public:
  using value_type = T;

  constexpr Optional() {}
  constexpr Optional(NoneType) {}

  constexpr Optional(const T &y) : Storage(in_place, y) {}
  constexpr Optional(const Optional &O) = default;

  constexpr Optional(T &&y) : Storage(in_place, std::move(y)) {}
  constexpr Optional(Optional &&O) = default;

  template <typename... ArgTypes>
  constexpr Optional(in_place_t, ArgTypes &&...Args)
      : Storage(in_place, std::forward<ArgTypes>(Args)...) {}

  Optional &operator=(T &&y) {
    Storage = std::move(y);
    return *this;
  }
  Optional &operator=(Optional &&O) = default;

  /// Create a new object by constructing it in place with the given arguments.
  template <typename... ArgTypes> void emplace(ArgTypes &&... Args) {
    Storage.emplace(std::forward<ArgTypes>(Args)...);
  }

  static constexpr Optional create(const T *y) {
    return y ? Optional(*y) : Optional();
  }

  Optional &operator=(const T &y) {
    Storage = y;
    return *this;
  }
  Optional &operator=(const Optional &O) = default;

  void reset() { Storage.reset(); }

  constexpr const T *getPointer() const { return &Storage.getValue(); }
  T *getPointer() { return &Storage.getValue(); }
  constexpr const T &getValue() const LLVM_LVALUE_FUNCTION {
    return Storage.getValue();
  }
  T &getValue() LLVM_LVALUE_FUNCTION { return Storage.getValue(); }

  constexpr explicit operator bool() const { return hasValue(); }
  constexpr bool hasValue() const { return Storage.hasValue(); }
  constexpr const T *operator->() const { return getPointer(); }
  T *operator->() { return getPointer(); }
  constexpr const T &operator*() const LLVM_LVALUE_FUNCTION {
    return getValue();
  }
  T &operator*() LLVM_LVALUE_FUNCTION { return getValue(); }

  template <typename U>
  constexpr T getValueOr(U &&value) const LLVM_LVALUE_FUNCTION {
    return hasValue() ? getValue() : std::forward<U>(value);
  }

  /// Apply a function to the value if present; otherwise return None.
  template <class Function>
  auto map(const Function &F) const LLVM_LVALUE_FUNCTION
      -> Optional<decltype(F(getValue()))> {
    if (*this) return F(getValue());
    return None;
  }

#if LLVM_HAS_RVALUE_REFERENCE_THIS
  T &&getValue() && { return std::move(Storage.getValue()); }
  T &&operator*() && { return std::move(Storage.getValue()); }

  template <typename U>
  T getValueOr(U &&value) && {
    return hasValue() ? std::move(getValue()) : std::forward<U>(value);
  }

  /// Apply a function to the value if present; otherwise return None.
  template <class Function>
  auto map(const Function &F) &&
      -> Optional<decltype(F(std::move(*this).getValue()))> {
    if (*this) return F(std::move(*this).getValue());
    return None;
  }
#endif
};

template <class T> llvm::hash_code hash_value(const Optional<T> &O) {
  return O ? hash_combine(true, *O) : hash_value(false);
}

template <typename T, typename U>
constexpr bool operator==(const Optional<T> &X, const Optional<U> &Y) {
  if (X && Y)
    return *X == *Y;
  return X.hasValue() == Y.hasValue();
}

template <typename T, typename U>
constexpr bool operator!=(const Optional<T> &X, const Optional<U> &Y) {
  return !(X == Y);
}

template <typename T, typename U>
constexpr bool operator<(const Optional<T> &X, const Optional<U> &Y) {
  if (X && Y)
    return *X < *Y;
  return X.hasValue() < Y.hasValue();
}

template <typename T, typename U>
constexpr bool operator<=(const Optional<T> &X, const Optional<U> &Y) {
  return !(Y < X);
}

template <typename T, typename U>
constexpr bool operator>(const Optional<T> &X, const Optional<U> &Y) {
  return Y < X;
}

template <typename T, typename U>
constexpr bool operator>=(const Optional<T> &X, const Optional<U> &Y) {
  return !(X < Y);
}

template <typename T>
constexpr bool operator==(const Optional<T> &X, NoneType) {
  return !X;
}

template <typename T>
constexpr bool operator==(NoneType, const Optional<T> &X) {
  return X == None;
}

template <typename T>
constexpr bool operator!=(const Optional<T> &X, NoneType) {
  return !(X == None);
}

template <typename T>
constexpr bool operator!=(NoneType, const Optional<T> &X) {
  return X != None;
}

template <typename T> constexpr bool operator<(const Optional<T> &, NoneType) {
  return false;
}

template <typename T> constexpr bool operator<(NoneType, const Optional<T> &X) {
  return X.hasValue();
}

template <typename T>
constexpr bool operator<=(const Optional<T> &X, NoneType) {
  return !(None < X);
}

template <typename T>
constexpr bool operator<=(NoneType, const Optional<T> &X) {
  return !(X < None);
}

template <typename T> constexpr bool operator>(const Optional<T> &X, NoneType) {
  return None < X;
}

template <typename T> constexpr bool operator>(NoneType, const Optional<T> &X) {
  return X < None;
}

template <typename T>
constexpr bool operator>=(const Optional<T> &X, NoneType) {
  return None <= X;
}

template <typename T>
constexpr bool operator>=(NoneType, const Optional<T> &X) {
  return X <= None;
}

template <typename T>
constexpr bool operator==(const Optional<T> &X, const T &Y) {
  return X && *X == Y;
}

template <typename T>
constexpr bool operator==(const T &X, const Optional<T> &Y) {
  return Y && X == *Y;
}

template <typename T>
constexpr bool operator!=(const Optional<T> &X, const T &Y) {
  return !(X == Y);
}

template <typename T>
constexpr bool operator!=(const T &X, const Optional<T> &Y) {
  return !(X == Y);
}

template <typename T>
constexpr bool operator<(const Optional<T> &X, const T &Y) {
  return !X || *X < Y;
}

template <typename T>
constexpr bool operator<(const T &X, const Optional<T> &Y) {
  return Y && X < *Y;
}

template <typename T>
constexpr bool operator<=(const Optional<T> &X, const T &Y) {
  return !(Y < X);
}

template <typename T>
constexpr bool operator<=(const T &X, const Optional<T> &Y) {
  return !(Y < X);
}

template <typename T>
constexpr bool operator>(const Optional<T> &X, const T &Y) {
  return Y < X;
}

template <typename T>
constexpr bool operator>(const T &X, const Optional<T> &Y) {
  return Y < X;
}

template <typename T>
constexpr bool operator>=(const Optional<T> &X, const T &Y) {
  return !(X < Y);
}

template <typename T>
constexpr bool operator>=(const T &X, const Optional<T> &Y) {
  return !(X < Y);
}

raw_ostream &operator<<(raw_ostream &OS, NoneType);

template <typename T, typename = decltype(std::declval<raw_ostream &>()
                                          << std::declval<const T &>())>
raw_ostream &operator<<(raw_ostream &OS, const Optional<T> &O) {
  if (O)
    OS << *O;
  else
    OS << None;
  return OS;
}

} // end namespace llvm

#endif // LLVM_ADT_OPTIONAL_H
