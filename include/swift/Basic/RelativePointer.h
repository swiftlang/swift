//===--- RelativePointer.h - Relative Pointer Support -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Some data structures emitted by the Swift compiler use relative indirect
// addresses in order to minimize startup cost for a process. By referring to
// the offset of the global offset table entry for a symbol, instead of directly
// referring to the symbol, compiler-emitted data structures avoid requiring
// unnecessary relocation at dynamic linking time. This header contains types
// to help dereference these relative addresses.
//
//===----------------------------------------------------------------------===//

#include <cstdint>

namespace swift {

/// A relative reference to an object stored in memory. The reference may be
/// direct or indirect, and uses the low bit of the (assumed at least
/// 2-byte-aligned) pointer to differentiate.
template<typename ValueTy>
class RelativeIndirectablePointer {
private:
  /// The relative offset of the pointer's memory from the `this` pointer.
  /// If the low bit is clear, this is a direct reference; otherwise, it is
  /// an indirect reference.
  int32_t RelativeOffset;

  /// RelativePointers should appear in statically-generated metadata. They
  /// shouldn't be constructed or copied.
  RelativeIndirectablePointer() = delete;
  RelativeIndirectablePointer(RelativeIndirectablePointer &&) = delete;
  RelativeIndirectablePointer(const RelativeIndirectablePointer &) = delete;
  RelativeIndirectablePointer &operator=(RelativeIndirectablePointer &&)
    = delete;
  RelativeIndirectablePointer &operator=(const RelativeIndirectablePointer &)
    = delete;

  const ValueTy *get() const & {
    // The pointer is offset relative to `this`.
    auto base = reinterpret_cast<intptr_t>(this);
    intptr_t address = base + (RelativeOffset & ~1);

    // If the low bit is set, then this is an indirect address. Otherwise,
    // it's direct.
    if (RelativeOffset & 1) {
      return *reinterpret_cast<const ValueTy * const *>(address);
    } else {
      return reinterpret_cast<const ValueTy *>(address);
    }
  }

public:
  operator const ValueTy* () const & {
    return get();
  }

  const ValueTy &operator*() const & {
    return *get();
  }

  const ValueTy *operator->() const & {
    return get();
  }
};

/// A relative reference to a function, intended to reference private metadata
/// functions for the current executable or dynamic library image from
/// position-independent constant data.
template<typename T>
class RelativeDirectPointerImpl {
private:
  /// The relative offset of the function's entry point from *this.
  int32_t RelativeOffset;

  /// RelativePointers should appear in statically-generated metadata. They
  /// shouldn't be constructed or copied.
  RelativeDirectPointerImpl() = delete;
  RelativeDirectPointerImpl(RelativeDirectPointerImpl &&) = delete;
  RelativeDirectPointerImpl(const RelativeDirectPointerImpl &) = delete;
  RelativeDirectPointerImpl &operator=(RelativeDirectPointerImpl &&)
    = delete;
  RelativeDirectPointerImpl &operator=(const RelativeDirectPointerImpl&)
    = delete;

public:
  using ValueTy = T;
  using PointerTy = T*;

  PointerTy get() const & {
    // The function entry point is addressed relative to `this`.
    auto base = reinterpret_cast<intptr_t>(this);
    intptr_t absolute = base + RelativeOffset;
    return reinterpret_cast<PointerTy>(absolute);
  }

  /// A zero relative offset encodes a null reference.
  bool isNull() const & {
    return RelativeOffset == 0;
  }
};

/// A direct relative reference to an object.
template<typename T>
class RelativeDirectPointer :
  private RelativeDirectPointerImpl<T>
{
  using super = RelativeDirectPointerImpl<T>;
public:
  operator typename super::PointerTy() const & {
    return this->get();
  }

  const typename super::ValueTy &operator*() const & {
    return *this->get();
  }

  const typename super::ValueTy *operator->() const & {
    return this->get();
  }

  using super::isNull;
};

/// A specialization of RelativeDirectPointer for function pointers,
/// allowing for calls.
template<typename RetTy, typename...ArgTy>
class RelativeDirectPointer<RetTy (ArgTy...)> :
  private RelativeDirectPointerImpl<RetTy (ArgTy...)>
{
  using super = RelativeDirectPointerImpl<RetTy (ArgTy...)>;
public:
  operator typename super::PointerTy() const & {
    return this->get();
  }

  RetTy operator()(ArgTy...arg) {
    return this->get()(std::forward<ArgTy>(arg)...);
  }

  using super::isNull;
};

}

