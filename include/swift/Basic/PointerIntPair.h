//===--- PointerIntPair.h - Pointer/int pair with 32-bit fallback -*- C++ -*-=//
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

#ifndef SWIFT_BASIC_POINTERINTPAIR_H
#define SWIFT_BASIC_POINTERINTPAIR_H

#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include "swift/Runtime/Config.h"
#include <cstdint>
#include <type_traits>

namespace swift {
namespace pointer_int_pair_detail {

/// Same value interface as llvm::PointerIntPair, but with the pointer and the
/// integer in separate fields, used when the pointer type has too few spare
/// low bits to pack IntBits into.
template <typename PointerTy, typename IntType>
class SeparateStorage {
  PointerTy Ptr{};
  IntType Int{};

public:
  constexpr SeparateStorage() = default;
  SeparateStorage(PointerTy P, IntType I) : Ptr(P), Int(I) {}
  explicit SeparateStorage(PointerTy P) : Ptr(P) {}

  PointerTy getPointer() const { return Ptr; }
  IntType getInt() const { return Int; }
  void setPointer(PointerTy P) & { Ptr = P; }
  void setInt(IntType I) & { Int = I; }
  void initWithPointer(PointerTy P) & { Ptr = P; Int = IntType{}; }
  void setPointerAndInt(PointerTy P, IntType I) & { Ptr = P; Int = I; }

  bool operator==(const SeparateStorage &RHS) const {
    return Ptr == RHS.Ptr && Int == RHS.Int;
  }
  bool operator!=(const SeparateStorage &RHS) const { return !(*this == RHS); }
};

/// The default authentication policy: no pointer authentication at all.
/// `sign`/`authenticate` are identity functions, so a PointerIntPair that
/// doesn't opt into a real Auth policy compiles down to exactly the same
/// code as before this policy parameter existed.
struct Unauthenticated {
  static uintptr_t sign(uintptr_t value, const void *) { return value; }
  static uintptr_t authenticate(uintptr_t value, const void *) { return value; }
};

} // namespace pointer_int_pair_detail

/// An authentication policy that signs/authenticates a data pointer,
/// diversified on the address of the object doing the signing (typically
/// `this` from inside a `PointerIntPair`-family class) blended with a static
/// discriminator. On platforms without pointer authentication, degenerates
/// to a no-op.
///
/// IMPORTANT: this policy requires that the `PointerIntPair` live at a
/// fixed address.  If the address changes, it will invalidate the stored
/// value.
#if SWIFT_PTRAUTH
template <unsigned Discriminator,
          ptrauth_key Key = ptrauth_key_process_independent_data>
struct AddressDiversifiedPointerAuth {
  static uintptr_t sign(uintptr_t value, const void *addr) {
    if (!value)
      return value;
    return reinterpret_cast<uintptr_t>(ptrauth_sign_unauthenticated(
        reinterpret_cast<void *>(value), Key,
        ptrauth_blend_discriminator(addr, Discriminator)));
  }
  static uintptr_t authenticate(uintptr_t value, const void *addr) {
    if (!value)
      return value;
    return reinterpret_cast<uintptr_t>(ptrauth_auth_data(
        reinterpret_cast<void *>(value), Key,
        ptrauth_blend_discriminator(addr, Discriminator)));
  }
};
#else
template <unsigned Discriminator, unsigned Key = 0>
struct AddressDiversifiedPointerAuth {
  static uintptr_t sign(uintptr_t value, const void *) { return value; }
  static uintptr_t authenticate(uintptr_t value, const void *) { return value; }
};
#endif

namespace pointer_int_pair_detail {

/// Similar to llvm::PointerIntPair, but the packed pointer+int word is
/// signed as a whole using `Auth`. The int bits are folded into the
/// pointer's spare low bits *before* signing, and the full word is
/// authenticated *before* the int bits are masked back out. This way
/// tampering with either the pointer or the packed int invalidates the
/// signature.
template <typename PointerTy, unsigned IntBits, typename IntType,
          typename PtrTraits, typename Auth>
class AuthenticatedPacked {
  using Info = llvm::PointerIntPairInfo<PointerTy, IntBits, PtrTraits>;

  uintptr_t Value = 0;

  uintptr_t logical() const {
    return Auth::authenticate(Value, this);
  }
  void setLogical(uintptr_t word) & { Value = Auth::sign(word, this); }

public:
  constexpr AuthenticatedPacked() = default;
  AuthenticatedPacked(PointerTy P, IntType I) { setPointerAndInt(P, I); }
  explicit AuthenticatedPacked(PointerTy P) { initWithPointer(P); }

  // Signing is diversified on this object's own address, so copying the raw
  // signed word bitwise (the implicit copy ctor/assignment) would leave the
  // destination holding a signature computed for the source's address.
  // Re-derive and re-sign instead.
  AuthenticatedPacked(const AuthenticatedPacked &Other)
      : AuthenticatedPacked(Other.getPointer(), Other.getInt()) {}
  AuthenticatedPacked &operator=(const AuthenticatedPacked &Other) & {
    setPointerAndInt(Other.getPointer(), Other.getInt());
    return *this;
  }

  PointerTy getPointer() const {
    return Info::getPointer(static_cast<intptr_t>(logical()));
  }
  IntType getInt() const {
    return (IntType)Info::getInt(static_cast<intptr_t>(logical()));
  }

  void setPointer(PointerTy P) & {
    setLogical(static_cast<uintptr_t>(
        Info::updatePointer(static_cast<intptr_t>(logical()), P)));
  }
  void setInt(IntType I) & {
    setLogical(static_cast<uintptr_t>(Info::updateInt(
        static_cast<intptr_t>(logical()), static_cast<intptr_t>(I))));
  }
  void initWithPointer(PointerTy P) & {
    setLogical(static_cast<uintptr_t>(Info::updatePointer(0, P)));
  }
  void setPointerAndInt(PointerTy P, IntType I) & {
    intptr_t Word = Info::updatePointer(0, P);
    Word = Info::updateInt(Word, static_cast<intptr_t>(I));
    setLogical(static_cast<uintptr_t>(Word));
  }

  // Compares the authenticated logical value, not the raw signed bits:
  // address-diversified signing means two objects holding the same
  // logical pointer+int at different addresses have different raw bits.
  bool operator==(const AuthenticatedPacked &RHS) const {
    return logical() == RHS.logical();
  }
  bool operator!=(const AuthenticatedPacked &RHS) const {
    return !(*this == RHS);
  }
};

/// Same value interface as SeparateStorage, but the pointer field is signed
/// and authenticated using `Auth`.
template <typename PointerTy, typename IntType, typename Auth>
class AuthenticatedSeparateStorage {
  using PtrTraits = llvm::PointerLikeTypeTraits<PointerTy>;

  uintptr_t SignedPtr = 0;
  IntType Int{};

public:
  constexpr AuthenticatedSeparateStorage() = default;
  AuthenticatedSeparateStorage(PointerTy P, IntType I) {
    setPointerAndInt(P, I);
  }
  explicit AuthenticatedSeparateStorage(PointerTy P) { initWithPointer(P); }

  // See AuthenticatedPacked's copy ctor/assignment for why a bitwise copy
  // of the raw signed word would be wrong here.
  AuthenticatedSeparateStorage(const AuthenticatedSeparateStorage &Other)
      : AuthenticatedSeparateStorage(Other.getPointer(), Other.getInt()) {}
  AuthenticatedSeparateStorage &
  operator=(const AuthenticatedSeparateStorage &Other) & {
    setPointerAndInt(Other.getPointer(), Other.getInt());
    return *this;
  }

  PointerTy getPointer() const {
    uintptr_t Word = Auth::authenticate(SignedPtr, this);
    return PtrTraits::getFromVoidPointer(reinterpret_cast<void *>(Word));
  }
  IntType getInt() const { return Int; }

  void setPointer(PointerTy P) & {
    uintptr_t Word =
        reinterpret_cast<uintptr_t>(PtrTraits::getAsVoidPointer(P));
    SignedPtr = Auth::sign(Word, this);
  }
  void setInt(IntType I) & { Int = I; }
  void initWithPointer(PointerTy P) & {
    setPointer(P);
    Int = IntType{};
  }
  void setPointerAndInt(PointerTy P, IntType I) & {
    setPointer(P);
    Int = I;
  }

  // See AuthenticatedPacked::operator== for why this compares the
  // authenticated pointer rather than the raw signed bits.
  bool operator==(const AuthenticatedSeparateStorage &RHS) const {
    return getPointer() == RHS.getPointer() && Int == RHS.Int;
  }
  bool operator!=(const AuthenticatedSeparateStorage &RHS) const {
    return !(*this == RHS);
  }
};

template <bool HasEnoughBits, bool IsUnauthenticated, typename PointerTy,
          unsigned IntBits, typename IntType, typename PtrTraits,
          typename Auth>
struct Selector2;

template <typename PointerTy, unsigned IntBits, typename IntType,
          typename PtrTraits, typename Auth>
struct Selector2<true, true, PointerTy, IntBits, IntType, PtrTraits, Auth> {
  using type = llvm::PointerIntPair<PointerTy, IntBits, IntType, PtrTraits>;
};

template <typename PointerTy, unsigned IntBits, typename IntType,
          typename PtrTraits, typename Auth>
struct Selector2<false, true, PointerTy, IntBits, IntType, PtrTraits, Auth> {
  using type = SeparateStorage<PointerTy, IntType>;
};

template <typename PointerTy, unsigned IntBits, typename IntType,
          typename PtrTraits, typename Auth>
struct Selector2<true, false, PointerTy, IntBits, IntType, PtrTraits, Auth> {
  using type =
      AuthenticatedPacked<PointerTy, IntBits, IntType, PtrTraits, Auth>;
};

template <typename PointerTy, unsigned IntBits, typename IntType,
          typename PtrTraits, typename Auth>
struct Selector2<false, false, PointerTy, IntBits, IntType, PtrTraits, Auth> {
  using type = AuthenticatedSeparateStorage<PointerTy, IntType, Auth>;
};

template <bool HasEnoughBits, typename PointerTy, unsigned IntBits,
          typename IntType, typename PtrTraits, typename Auth>
struct Selector {
  using type =
      typename Selector2<HasEnoughBits, std::is_same<Auth, Unauthenticated>::value,
                          PointerTy, IntBits, IntType, PtrTraits, Auth>::type;
};

} // namespace pointer_int_pair_detail

template <typename PointerTy, unsigned IntBits, typename IntType = unsigned,
          typename PtrTraits = llvm::PointerLikeTypeTraits<PointerTy>,
          typename Auth = pointer_int_pair_detail::Unauthenticated>
using PointerIntPair = typename pointer_int_pair_detail::Selector<
    (PtrTraits::NumLowBitsAvailable >= IntBits), PointerTy, IntBits, IntType,
    PtrTraits, Auth>::type;

} // namespace swift

#endif // SWIFT_BASIC_POINTERINTPAIR_H
