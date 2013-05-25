//===- PointerIntUnion.h - Discriminated union of ptr and int ---*- C++ -*-===//
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
/// \file Like PointerUnion, PointerIntUnion provides a discriminated union of
/// a pointer type and an integer type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_POINTERINTUNION_H
#define SWIFT_BASIC_POINTERINTUNION_H

#include "llvm/Support/PointerLikeTypeTraits.h"

namespace swift {
  /// This implements a discriminated union of a pointer and an integer,
  /// and keeps the discriminator bit-mangled into the low bits of the
  /// underlying storage. This allows the implementation to be extremely
  /// efficient in space, but permits a very natural and type-safe API.
  ///
  /// \tparam PointerTy The pointer type to be stored in the union.
  /// \tparam IntTy The integer type to be stored in the union.
  /// \tparam DataTy The storage type, which must be large enough to store both
  ///                \c PointerTy and \c IntTy values. Set this explicitly to
  ///                a larger type if you need to store values in the full
  ///                range of \c IntTy.
  template <typename PointerTy, typename IntTy = uintptr_t,
            typename DataTy = typename std::common_type<intptr_t, IntTy>::type>
  class PointerIntUnion {
    using PointerTraits = llvm::PointerLikeTypeTraits<PointerTy>;

    static_assert(sizeof(PointerTy) <= sizeof(DataTy), "pointer is too large");
    static_assert(sizeof(IntTy) <= sizeof(DataTy), "integer is too large");
    static_assert(PointerTraits::NumLowBitsAvailable >= 1,
                  "no room for a discriminator bit");

    DataTy Data;

  public:
    /*implicit*/ PointerIntUnion(IntTy val)
      : Data(1 | (static_cast<DataTy>(val) << 1)) {
      assert(!isPointer() && "sanity check");
      assert(getInt() == val && "integer requires too many bits");
    }
    /*implicit*/ PointerIntUnion(PointerTy val)
      : Data(reinterpret_cast<DataTy>(PointerTraits::getAsVoidPointer(val))) {
      assert(isPointer() && "pointer is not sufficiently aligned");
    }

    bool isPointer() const {
      return !(Data & 1);
    }

    PointerTy getPointer() const {
      assert(isPointer());
      return PointerTraits::getFromVoidPointer(reinterpret_cast<void *>(Data));
    }

    IntTy getInt() const {
      assert(!isPointer());
      return static_cast<IntTy>(Data >> 1);
    }
  };
}

#endif
