//===--- _SwiftStlibCxxOverlay.h - Additions for Stdlib ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef __cplusplus
#error "no C++"
#endif

#ifdef SWIFT_CXX_INTEROP_OPTIONAL_MIXIN

/// True when the Optional has a value.
SWIFT_INLINE_THUNK operator bool() const noexcept { return *this != none; }

/// Returns the value stored in the Optional.
///
/// The returned value is copied using the appropriate Swift / C++ copy
/// semantics.
SWIFT_INLINE_THUNK T_0_0 get() const
    noexcept(noexcept(getUnsafelyUnwrapped())) {
  // FIXME: Fail with source location.
  return getUnsafelyUnwrapped();
}

#undef SWIFT_CXX_INTEROP_OPTIONAL_MIXIN

#else
// out-of-class overlay for Swift standard library.

static_assert(sizeof(_impl::_impl_String) >= 0,
              "included outside of stdlib bindings");

namespace cxxOverlay {

class IterationEndSentinel;

/// Abstract Swift collection iterator.
template <class Collection, class T> class CollectionIterator {
public:
  using Index =
      decltype(reinterpret_cast<Collection *>(0x123)->getStartIndex());

  SWIFT_INLINE_THUNK CollectionIterator(const Collection &collection)
      : collection(collection) {
    index = collection.getStartIndex();
    endIndex = collection.getEndIndex();
    // FIXME: Begin read access.
  }

  SWIFT_INLINE_THUNK ~CollectionIterator() {
    // FIXME: End read access.
  }

  SWIFT_INLINE_THUNK T operator*() const { return collection[index]; }
  SWIFT_INLINE_THUNK void operator++() {
    ++index;
    // FIXME: assert(index <= endIndex); // No need to go past the end.
  }

  SWIFT_INLINE_THUNK bool operator!=(const IterationEndSentinel &) const {
    return index != endIndex;
  }

private:
  Index index, endIndex;
  const Collection &collection;
};

class IterationEndSentinel {};

template <class T> using ArrayIterator = CollectionIterator<Array<T>, T>;

} // namespace cxxOverlay

// FIXME: This should apply to more than the Array type.
template <class T>
SWIFT_INLINE_THUNK cxxOverlay::ArrayIterator<T> begin(const Array<T> &array
                                          [[clang::lifetimebound]]) {
  return cxxOverlay::ArrayIterator<T>(array);
}

template <class T>
SWIFT_INLINE_THUNK cxxOverlay::IterationEndSentinel end(const Array<T> &) {
  return {};
}

extern "C" void *_Nonnull swift_errorRetain(void *_Nonnull swiftError) noexcept;

extern "C" void swift_errorRelease(void *_Nonnull swiftError) noexcept;

extern "C" int $ss5ErrorMp; // external global %swift.protocol, align 4

extern "C" const void *_Nullable swift_getTypeByMangledNameInContext(
    const char *_Nullable typeNameStart, size_t typeNameLength,
    const void *_Nullable context,
    const void *_Nullable const *_Nullable genericArgs) SWIFT_CALL;

extern "C" bool swift_dynamicCast(void *_Nullable dest, void *_Nullable src,
                                  const void *_Nullable srcType,
                                  const void *_Nullable targetType,
                                  uint32_t flags);

struct SymbolicP {
  alignas(2) uint8_t _1;
  uint32_t _2;
  uint8_t _3[2];
  uint8_t _4;
} __attribute__((packed));

inline const void *_Nullable getErrorMetadata() {
  static SymbolicP errorSymbol;
  static int *_Nonnull got_ss5ErrorMp = &$ss5ErrorMp;
  errorSymbol._1 = 2;
  errorSymbol._2 =
      static_cast<uint32_t>(reinterpret_cast<uintptr_t>(&got_ss5ErrorMp) -
                            reinterpret_cast<uintptr_t>(&errorSymbol._2));
  errorSymbol._3[0] = '_';
  errorSymbol._3[1] = 'p';
  errorSymbol._4 = 0;
  static_assert(sizeof(errorSymbol) == 8, "");
  auto charErrorSymbol = reinterpret_cast<const char *>(&errorSymbol);

  const void *ptr2 = swift_getTypeByMangledNameInContext(
      charErrorSymbol, sizeof(errorSymbol) - 1, nullptr, nullptr);
  return ptr2;
}

class Error {
public:
  Error() {}
  Error(void *_Nonnull swiftError) { opaqueValue = swiftError; }
  ~Error() {
    if (opaqueValue)
      swift_errorRelease(opaqueValue);
  }
  void *_Nonnull getPointerToOpaquePointer() { return opaqueValue; }
  Error(Error &&other) : opaqueValue(other.opaqueValue) {
    other.opaqueValue = nullptr;
  }
  Error(const Error &other) {
    if (other.opaqueValue)
      swift_errorRetain(other.opaqueValue);
    opaqueValue = other.opaqueValue;
  }

  template <class T>
  Swift::Optional<T> as() {
    alignas(alignof(T)) char buffer[sizeof(T)];
    const void *em = getErrorMetadata();
    void *ep = getPointerToOpaquePointer();
    auto metadata = swift::TypeMetadataTrait<T>::getTypeMetadata();

    // Dynamic cast will release the error, so we need to retain it.
    swift_errorRetain(ep);
    bool dynamicCast =
        swift_dynamicCast(buffer, &ep, em, metadata,
                          /*take on success  destroy on failure*/ 6);

    if (dynamicCast) {
      auto result = swift::_impl::implClassFor<T>::type::returnNewValue(
          [&](char *dest) {
            swift::_impl::implClassFor<T>::type::initializeWithTake(dest,
                                                                    buffer);
          });
      return Swift::Optional<T>::init(result);
    }

    return  Swift::Optional<T>::none();
  }

private:
  void *_Nonnull opaqueValue = nullptr;
};

#endif
