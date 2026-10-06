//===--- SignedPointerUnion.h - Pointer-authenticated tagged union --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A two-case tagged pointer union whose entire encoded word -- the selected
// pointer together with its one-bit discriminator tag -- is signed with
// pointer authentication, address-discriminated against its own storage slot.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_SIGNEDPOINTERUNION_H
#define SWIFT_RUNTIME_SIGNEDPOINTERUNION_H

#include "swift/Runtime/Config.h"

#include <cassert>
#include <cstdint>
#include <type_traits>

namespace swift {

/// A tagged union of two pointer types, `FirstTy` or `SecondTy`, whose whole
/// encoded word -- the held pointer ORed with its one-bit discriminator tag --
/// is signed with ptrauth and address-discriminated against the address of its
/// own storage.
///
/// Compared to signing only the pointee pointer, signing the entire encoded
/// word also covers the discriminator tag, and the address discrimination binds
/// the signature to the slot. An attacker with a memory-write primitive can
/// therefore neither forge a value, relocate an otherwise legitimately signed
/// word in from another slot, nor flip the tag, without invalidating the
/// signature.
///
/// Both pointee types must be at least 2-byte aligned: one low bit is stolen
/// for the tag, the same assumption llvm::PointerUnion makes.
///
/// `Discriminator` is the ptrauth discriminator (blended with the storage
/// address). Each distinct use should pass a distinct discriminator so a signed
/// word cannot be substituted between unrelated unions.
template <class FirstTy, class SecondTy, unsigned Discriminator>
class SignedPointerUnion {
  static_assert(std::is_pointer<FirstTy>::value &&
                    std::is_pointer<SecondTy>::value,
                "SignedPointerUnion members must be pointer types");

  enum : uintptr_t { TagMask = 1, FirstTag = 0, SecondTag = 1 };

  /// The encoded (pointer | tag) word, signed in place and
  /// address-discriminated against `&Value`. A zero word (the empty /
  /// first-null state) is left unsigned.
  uintptr_t Value = 0;

  uintptr_t signTo(uintptr_t encoded) const {
#if SWIFT_PTRAUTH
    return reinterpret_cast<uintptr_t>(ptrauth_sign_unauthenticated(
        reinterpret_cast<void *>(encoded), ptrauth_key_process_dependent_data,
        ptrauth_blend_discriminator(&Value, Discriminator)));
#else
    return encoded;
#endif
  }

  uintptr_t authed() const {
#if SWIFT_PTRAUTH
    return reinterpret_cast<uintptr_t>(ptrauth_auth_data(
        reinterpret_cast<void *>(Value), ptrauth_key_process_dependent_data,
        ptrauth_blend_discriminator(&Value, Discriminator)));
#else
    return Value;
#endif
  }

  void store(const void *ptr, uintptr_t tag) {
    auto bits = reinterpret_cast<uintptr_t>(ptr);
    assert((bits & TagMask) == 0 && "pointer is not sufficiently aligned");
    Value = signTo(bits | tag);
  }

  template <class T>
  static constexpr uintptr_t tagFor() {
    static_assert(std::is_same<T, FirstTy>::value ||
                      std::is_same<T, SecondTy>::value,
                  "querying a type not in the union");
    return std::is_same<T, FirstTy>::value ? FirstTag : SecondTag;
  }

public:
  SignedPointerUnion() = default;

  // The copy operations must resign: the stored word is address-discriminated,
  // so a plain bit copy would leave the signature bound to the source slot and
  // fail to authenticate at the destination. Authenticate against the source
  // slot, then re-sign against this slot. (This is also what lets a
  // SignedPointerUnion survive being relocated, e.g. when it is stored inline
  // in a ConcurrentReadableHashMap entry that is copied on resize.)
  SignedPointerUnion(const SignedPointerUnion &other) {
    Value = signTo(other.authed());
  }
  SignedPointerUnion &operator=(const SignedPointerUnion &other) {
    if (this != &other)
      Value = signTo(other.authed());
    return *this;
  }

  SignedPointerUnion(FirstTy ptr) { store(ptr, FirstTag); }
  SignedPointerUnion(SecondTy ptr) { store(ptr, SecondTag); }

  SignedPointerUnion &operator=(FirstTy ptr) {
    store(ptr, FirstTag);
    return *this;
  }
  SignedPointerUnion &operator=(SecondTy ptr) {
    store(ptr, SecondTag);
    return *this;
  }

  /// True if the union currently holds a `T` (regardless of whether it is
  /// null). A default-constructed union holds a null `FirstTy`.
  template <class T>
  bool is() const {
    return (authed() & TagMask) == tagFor<T>();
  }

  /// The held pointer if the union holds a `T`, otherwise null.
  template <class T>
  T dyn_cast() const {
    uintptr_t v = authed();
    if ((v & TagMask) != tagFor<T>())
      return T{};
    return reinterpret_cast<T>(v & ~TagMask);
  }

  /// The held pointer; the union must hold a `T`.
  template <class T>
  T get() const {
    uintptr_t v = authed();
    assert((v & TagMask) == tagFor<T>());
    return reinterpret_cast<T>(v & ~TagMask);
  }

  /// True if the held pointer is non-null.
  explicit operator bool() const { return (authed() & ~TagMask) != 0; }
};

} // namespace swift

#endif // SWIFT_RUNTIME_SIGNEDPOINTERUNION_H
