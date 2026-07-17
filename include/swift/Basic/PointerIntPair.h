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

template <bool HasEnoughBits, typename PointerTy, unsigned IntBits,
          typename IntType, typename PtrTraits>
struct Selector {
  using type = llvm::PointerIntPair<PointerTy, IntBits, IntType, PtrTraits>;
};

template <typename PointerTy, unsigned IntBits, typename IntType,
          typename PtrTraits>
struct Selector<false, PointerTy, IntBits, IntType, PtrTraits> {
  using type = SeparateStorage<PointerTy, IntType>;
};

} // namespace pointer_int_pair_detail

template <typename PointerTy, unsigned IntBits, typename IntType = unsigned,
          typename PtrTraits = llvm::PointerLikeTypeTraits<PointerTy>>
using PointerIntPair = typename pointer_int_pair_detail::Selector<
    (PtrTraits::NumLowBitsAvailable >= IntBits), PointerTy, IntBits, IntType,
    PtrTraits>::type;

} // namespace swift

#endif // SWIFT_BASIC_POINTERINTPAIR_H
