//===-- RelativePointer.h - Relative Pointer Support ------------*- C++ -*-===//
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

/// A double-indirect relative reference to an address stored in memory,
/// intended to reference GOT entries in the current executable or dynamic
/// library image from position-independent constant data.
template<typename ValueTy>
class RelativePointer {
private:
  /// The relative offset of the pointer's memory from the `this` pointer.
  /// If the low bit is clear, this is a direct reference; otherwise, it is
  /// an indirect reference.
  int32_t RelativeOffset;

  /// RelativePointers should appear in statically-generated metadata. They
  /// shouldn't be constructed or copied.
  RelativePointer() = delete;
  RelativePointer(RelativePointer &&) = delete;
  RelativePointer(const RelativePointer &) = delete;
  RelativePointer &operator=(RelativePointer &&) = delete;
  RelativePointer &operator=(const RelativePointer &) = delete;

  const ValueTy *get() const & {
    // The pointer is offset relative to `this`.
    auto base = reinterpret_cast<uintptr_t>(this);
    uintptr_t address = base + (RelativeOffset & ~1u);

    // If the low bit is set, then this is an indirect address. Otherwise,
    // it's direct.
    if (RelativeOffset & 1u) {
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
template<typename FnTy>
class RelativeFunctionPointer;

template<typename RetTy, typename...ArgTy>
class RelativeFunctionPointer<RetTy (ArgTy...)> {
private:
  /// The relative offset of the function's entry point from *this.
  int32_t RelativeOffset;

  /// RelativePointers should appear in statically-generated metadata. They
  /// shouldn't be constructed or copied.
  RelativeFunctionPointer() = delete;
  RelativeFunctionPointer(RelativeFunctionPointer &&) = delete;
  RelativeFunctionPointer(const RelativeFunctionPointer &) = delete;
  RelativeFunctionPointer &operator=(RelativeFunctionPointer &&) = delete;
  RelativeFunctionPointer &operator=(const RelativeFunctionPointer &) = delete;

  using FunctionPointerTy = RetTy (*)(ArgTy...);

  FunctionPointerTy get() const & {
    // The function entry point is addressed relative to `this`.
    auto base = reinterpret_cast<uintptr_t>(this);
    uintptr_t entryPoint = base + RelativeOffset;
    return *reinterpret_cast<FunctionPointerTy>(entryPoint);
  }
public:
  operator FunctionPointerTy() const & {
    return get();
  }

  RetTy operator()(ArgTy...arg) {
    return get()(std::forward<ArgTy>(arg)...);
  }
};

}

