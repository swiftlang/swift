//===--- PossiblyUniquePtr.h - A dynamic smart pointer ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
///  \file
///
///  This file defines PossiblyUniquePtr, a smart pointer template that
///  typically owns its pointee but can dynamically just contain an unowned
///  pointer.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_POSSIBLY_UNIQUE_PTR_H
#define SWIFT_BASIC_POSSIBLY_UNIQUE_PTR_H

#include "llvm/ADT/PointerIntPair.h"
#include "swift/Basic/Compiler.h"

namespace swift {

enum PointerIsOwned_t: bool {
  PointerIsNotOwned = false,
  PointerIsOwned = true
};

/// Essentially std::unique_ptr except it dynamically tracks whether the
/// pointer is actually owned.
///
/// The operations which possibly transfer ownership into a PossiblyUniquePtr
/// (the constructor and `reset`) privilege the owning case. This allows
/// PossiblyUniquePtr<T> to serve as a drop-in replacement for
/// std::unique_ptr<T>.
template <class T, class PtrTraits = llvm::PointerLikeTypeTraits<T*>>
class SWIFT_TRIVIAL_ABI PossiblyUniquePtr {
  llvm::PointerIntPair<T*, 1, bool, PtrTraits> Value;

  // Befriend all other specializations of this class, for the use of the
  // converting constructor and assignment operator.
  template <class, class>
  friend class PossiblyUniquePtr;

public:
  PossiblyUniquePtr() {}

  // Allow implicit conversion from a null pointer.
  PossiblyUniquePtr(std::nullptr_t) {}

  // Require conversion from any other pointer to be explicit.
  explicit PossiblyUniquePtr(T *pointer,
                             PointerIsOwned_t owned = PointerIsOwned)
      : Value(pointer, owned) {
    assert((pointer != nullptr || !owned) && "claiming ownership of null pointer");
  }

  PossiblyUniquePtr(PossiblyUniquePtr &&other)
      : Value(other.Value) {
    other.dropValue();
  }

  PossiblyUniquePtr &operator=(PossiblyUniquePtr &&other) {
    destroyValue();
    Value = other.Value;
    other.dropValue();
    return *this;
  }

  ~PossiblyUniquePtr() {
    destroyValue();
  }

  // Converting constructor.
  template <class U, class UTraits>
  PossiblyUniquePtr(PossiblyUniquePtr<U,UTraits> &&other)
      : Value(other.Value.getPointer(), other.Value.getInt()) {
    other.dropValue();
  }

  // Converting assignment operator.
  template <class U, class UTraits>
  PossiblyUniquePtr &operator=(PossiblyUniquePtr<U,UTraits> &&other) {
    destroyValue();
    Value.setPointerAndInt(other.Value.getPointer(),
                           other.Value.getInt());
    other.dropValue();
    return *this;
  }

  template <class ObjectType, class... Args>
  static PossiblyUniquePtr make(Args &&...args) {
    return PossiblyUniquePtr(new ObjectType(std::forward<Args>(args)...));
  }

  /// As a unique pointer, PossiblyUniquePtr is non-copyable.
  PossiblyUniquePtr(const PossiblyUniquePtr &other) = delete;
  PossiblyUniquePtr &operator=(const PossiblyUniquePtr &other) = delete;

  explicit operator bool() const {
    return Value.getPointer() != nullptr;
  }

  T &operator*() const {
    return *get_nonnull();
  }
  T *operator->() const {
    return get_nonnull();
  }

  /// Reset the value of this object to the given pointer.
  ///
  /// Destroys the current value, if any.
  void reset(T *pointer = nullptr, PointerIsOwned_t owned = PointerIsOwned) & {
    destroyValue();
    Value.setPointerAndInt(pointer, owned);
  }

  /// Borrow the current pointer value, leaving ownership (if any) with this object.
  T *get() const {
    return Value.getPointer();
  }

  /// Borrow the current pointer value, leaving ownership (if any) with this object.
  /// Asserts that the pointer is non-null.
  T *get_nonnull() const {
    auto value = get();
    assert(value);
    return value;
  }

  /// Determines whether this object owns its pointer value.
  PointerIsOwned_t isOwned() const {
    return PointerIsOwned_t(Value.getInt());
  }

  /// Release ownership of the pointer, making the caller responsible for it.
  /// This can only be called on an owned pointer.
  T *releaseOwned() {
    assert(isOwned());
    auto value = get();
    dropValue();
    return value;
  }

private:
  /// Destroy the value, if we own it.
  void destroyValue() {
    if (Value.getInt()) {
      delete Value.getPointer();
    }
  }

  /// Clear the stored value without asserting ownership.
  void dropValue() & {
    Value.setPointerAndInt(nullptr, 0);
  }
};

template <class ObjectType, class... As>
PossiblyUniquePtr<ObjectType> make_possibly_unique(As &&...args) {
  return PossiblyUniquePtr<ObjectType>::
      template make<ObjectType>(std::forward<As>(args)...);
}

} // end namespace swift

#endif
