//===--- _SwiftStdlibCxxOverlay.h - Additions for Stdlib --------*- C++ -*-===//
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

#elif defined(SWIFT_CXX_INTEROP_STRING_MIXIN)

#ifndef SWIFT_CXX_INTEROP_HIDE_STL_OVERLAY

/// Constructs a Swift string from a C string.
SWIFT_INLINE_THUNK String(const char *cString) noexcept {
  if (!cString) {
#ifdef __EmbeddedSwift__
    auto res = _impl::$eS2SycfC();
#else
    auto res = _impl::$sS2SycfC();
#endif
    memcpy(_getOpaquePointer(), &res, sizeof(res));
    return;
  }
#ifdef __EmbeddedSwift__
  auto res = _impl::$eSS7cStringSSSPys4Int8VG_tcfC(cString);
#else
  auto res = _impl::$sSS7cStringSSSPys4Int8VG_tcfC(cString);
#endif
  memcpy(_getOpaquePointer(), &res, sizeof(res));
}

/// Constructs a Swift string from a C++ string.
SWIFT_INLINE_THUNK String(const std::string &str) noexcept {
#ifdef __EmbeddedSwift__
  auto res = _impl::$eSS7cStringSSSPys4Int8VG_tcfC(str.c_str());
#else
  auto res = _impl::$sSS7cStringSSSPys4Int8VG_tcfC(str.c_str());
#endif
  memcpy(_getOpaquePointer(), &res, sizeof(res));
}

/// Casts the Swift String value to a C++ std::string.
SWIFT_INLINE_THUNK operator std::string() const;

#endif // SWIFT_CXX_INTEROP_HIDE_STL_OVERLAY

#undef SWIFT_CXX_INTEROP_STRING_MIXIN

#else
// out-of-class overlay for Swift standard library.

static_assert(sizeof(_impl::_impl_String) >= 0,
              "included outside of stdlib bindings");

#ifndef SWIFT_CXX_INTEROP_HIDE_STL_OVERLAY

SWIFT_INLINE_THUNK String::operator std::string() const {
  auto u = getUtf8();
  std::string result;
  result.reserve(u.getCount());

  auto end_offset = u.getEndIndex().getEncodedOffset();
  for (auto idx = u.getStartIndex(); idx.getEncodedOffset() < end_offset;
       idx = u.indexAfter(idx)) {
    result.push_back(static_cast<char>(u[idx]));
  }

  return result;
}

#endif // SWIFT_CXX_INTEROP_HIDE_STL_OVERLAY

namespace cxxOverlay {

class IterationEndSentinel;

/// Abstract Swift collection iterator.
template <class Collection, class T> class CollectionIterator {
public:
  using Index =
      decltype(reinterpret_cast<Collection *>(0x123)->getStartIndex());

  SWIFT_INLINE_THUNK CollectionIterator(const Collection &c) noexcept(
      noexcept(c.getStartIndex()) &&noexcept(c.getEndIndex()))
      : collection(c) {
    index = collection.getStartIndex();
    endIndex = collection.getEndIndex();
    // FIXME: Begin read access.
  }

  SWIFT_INLINE_THUNK ~CollectionIterator() noexcept {
    // FIXME: End read access.
  }

  SWIFT_INLINE_THUNK T operator*() const noexcept {
    return collection[index];
  }
  SWIFT_INLINE_THUNK void operator++() noexcept {
    ++index;
    // FIXME: assert(index <= endIndex); // No need to go past the end.
  }

  SWIFT_INLINE_THUNK bool
  operator!=(const IterationEndSentinel &) const noexcept {
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

#if defined(SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR) &&                     \
    !defined(__EmbeddedSwift__)

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

SWIFT_INLINE_THUNK const void *_Nullable getErrorMetadata() {
// We do not care about these symbols being duplicated across multiple shared
// libraries for now.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunique-object-duplication"
  static SymbolicP errorSymbol;
  static int *_Nonnull got_ss5ErrorMp = &$ss5ErrorMp;
#pragma clang diagnostic pop
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

#ifndef SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR

// Swift.String.init<A>(describing: A) -> Swift.String
// The argument is passed indirectly and is consumed at +1; the trailing
// parameter is the type metadata for A.
extern "C" _impl::swift_interop_stub_Swift_String
$sSS10describingSSx_tclufC(const void *_Nonnull value,
                           const void *_Nonnull metadata) SWIFT_NOEXCEPT
    SWIFT_CALL;

#pragma clang diagnostic push
// swift::Error defines all of its virtual methods inline, so it has no key
// function and its vtable is emitted weakly in each translation unit.
#pragma clang diagnostic ignored "-Wweak-vtables"

class Error : public std::exception {
public:
  SWIFT_INLINE_THUNK Error() noexcept {}
  SWIFT_INLINE_THUNK Error(void *_Nonnull swiftError) noexcept {
    opaqueValue = swiftError;
  }
  ~Error() noexcept override {
    if (opaqueValue)
      swift_errorRelease(opaqueValue);
    if (whatBuffer)
      _impl::opaqueFree(static_cast<char *_Nonnull>(whatBuffer));
  }
  SWIFT_INLINE_THUNK void *_Nonnull getPointerToOpaquePointer() noexcept {
    return opaqueValue;
  }
  SWIFT_INLINE_THUNK Error(Error &&other) noexcept
      : opaqueValue(other.opaqueValue), whatBuffer(other.whatBuffer) {
    other.opaqueValue = nullptr;
    other.whatBuffer = nullptr;
  }
  SWIFT_INLINE_THUNK Error(const Error &other) noexcept
      : std::exception(other) {
    if (other.opaqueValue)
      swift_errorRetain(other.opaqueValue);
    opaqueValue = other.opaqueValue;
    // The cached `what()` buffer is intentionally not copied; the copy
    // recomputes it on demand.
  }
  SWIFT_INLINE_THUNK Error &operator=(const Error &other) noexcept {
    if (this == &other)
      return *this;
    std::exception::operator=(other);
    if (other.opaqueValue)
      swift_errorRetain(other.opaqueValue);
    if (opaqueValue)
      swift_errorRelease(opaqueValue);
    opaqueValue = other.opaqueValue;
    if (whatBuffer) {
      _impl::opaqueFree(static_cast<char *_Nonnull>(whatBuffer));
      whatBuffer = nullptr;
    }
    return *this;
  }
  SWIFT_INLINE_THUNK Error &operator=(Error &&other) noexcept {
    void *otherOpaqueValue = other.opaqueValue;
    other.opaqueValue = opaqueValue;
    opaqueValue = otherOpaqueValue;
    char *otherWhatBuffer = other.whatBuffer;
    other.whatBuffer = whatBuffer;
    whatBuffer = otherWhatBuffer;
    return *this;
  }

  /// Returns the UTF-8 encoded result of `String(describing:)` for the
  /// thrown Swift error value. The result is computed lazily and cached in
  /// this object.
  const char *_Nonnull what() const noexcept override {
    char *cached = __atomic_load_n(&whatBuffer, __ATOMIC_ACQUIRE);
    if (cached)
      return cached;
    if (!opaqueValue)
      return "swift::Error: no error value";
    const void *errorMetadata = getErrorMetadata();
    if (!errorMetadata)
      return "swift::Error: error metadata not available";
    // `String.init(describing:)` consumes its argument at +1, so hand the
    // callee its own retained reference to the error value.
    swift_errorRetain(opaqueValue);
    void *errorValue = opaqueValue;
    auto stringStub = $sSS10describingSSx_tclufC(&errorValue, errorMetadata);
    auto description = _impl::_impl_String::returnNewValue(
        [&](char *_Nonnull dest) SWIFT_INLINE_THUNK_ATTRIBUTES {
          memcpy(dest, &stringStub, sizeof(stringStub));
        });
    auto utf8 = description.getUtf8();
    size_t length = static_cast<size_t>(utf8.getCount());
    char *buffer =
        static_cast<char *>(_impl::opaqueAlloc(length + 1, alignof(char)));
    if (!buffer)
      return "swift::Error: failed to allocate description buffer";
    size_t i = 0;
    auto endOffset = utf8.getEndIndex().getEncodedOffset();
    for (auto index = utf8.getStartIndex();
         index.getEncodedOffset() < endOffset && i < length;
         index = utf8.indexAfter(index))
      buffer[i++] = static_cast<char>(utf8[index]);
    buffer[i] = '\0';
    char *expected = nullptr;
    if (!__atomic_compare_exchange_n(&whatBuffer, &expected, buffer,
                                     /*weak=*/false, __ATOMIC_ACQ_REL,
                                     __ATOMIC_ACQUIRE)) {
      // Another thread cached its buffer first; use that one.
      _impl::opaqueFree(buffer);
      return expected;
    }
    return buffer;
  }

  template <class T> SWIFT_INLINE_THUNK swift::Optional<T> as() const {
    alignas(alignof(T)) char buffer[sizeof(T)];
    const void *em = getErrorMetadata();
    void *ep = opaqueValue;
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
      return swift::Optional<T>::init(result);
    }

    return swift::Optional<T>::none();
  }

private:
  void *_Nonnull opaqueValue = nullptr;
  mutable char *_Nullable whatBuffer = nullptr;
};

#pragma clang diagnostic pop

namespace _impl {

constexpr inline std::size_t max(std::size_t a, std::size_t b) {
  return a > b ? a : b;
}

} // namespace _impl

/// The Expected class has either an error or an value.
template<class T>
class Expected {
public:

  /// Default
  constexpr Expected() noexcept {
    new (&buffer) Error();
    has_val = false;
  }

  constexpr Expected(const swift::Error &error_val) noexcept {
    new (&buffer) Error(error_val);
    has_val = false;
  }

  constexpr Expected(const T &val) noexcept {
    new (&buffer) T(val);
    has_val = true;
  }

  /// Copy
  constexpr Expected(Expected const& other) noexcept {
    if (other.has_value())
      new (&buffer) T(other.value());
    else
      new (&buffer) Error(other.error());

    has_val = other.has_value();
  }

  /// Move
  // FIXME: Implement move semantics when move Swift values is possible
  constexpr Expected(Expected&&) noexcept { abort(); }

  ~Expected() noexcept {
    if (has_value())
      reinterpret_cast<const T *>(buffer)->~T();
    else
      reinterpret_cast<swift::Error *>(buffer)->~Error();
  }

  /// assignment
  constexpr auto operator=(Expected&& other) noexcept = delete;
  constexpr auto operator=(Expected&) noexcept = delete;

  /// For accessing T's members
  constexpr T const *_Nonnull operator->() const noexcept {
    if (!has_value())
      abort();
    return reinterpret_cast<const T *>(buffer);
  }

  constexpr T *_Nonnull operator->() noexcept {
    if (!has_value())
      abort();
    return reinterpret_cast<T *>(buffer);
  }

  /// Getting reference to T
  constexpr T const &operator*() const & noexcept {
    if (!has_value())
      abort();
    return reinterpret_cast<const T &>(buffer);
  }

  constexpr T &operator*() & noexcept {
    if (!has_value())
      abort();
    return reinterpret_cast<T &>(buffer);
  }

  constexpr explicit operator bool() const noexcept { return has_value(); }

  // Get value, if not exists abort
  constexpr T const& value() const& {
    if (!has_value())
      abort();
    return *reinterpret_cast<const T *>(buffer);
  }

  constexpr T& value() & {
    if (!has_value())
      abort();
    return *reinterpret_cast<T *>(buffer);
  }

  // Get error
  constexpr swift::Error const &error() const & {
    if (has_value())
      abort();
    return reinterpret_cast<const swift::Error &>(buffer);
  }

  constexpr swift::Error &error() & {
    if (has_value())
      abort();
    return reinterpret_cast<swift::Error &>(buffer);
  }

  constexpr bool has_value() const noexcept { return has_val; }

private:
  alignas(_impl::max(alignof(T), alignof(swift::Error))) char buffer[_impl::max(
      sizeof(T), sizeof(swift::Error))];
  bool has_val;
};

template<>
class Expected<void> {
public:
  /// Default: a successful `void` result.
  Expected() noexcept {
    has_val = true;
  }

  Expected(const swift::Error &error_val) noexcept {
    new (&buffer) Error(error_val);
    has_val = false;
  }

  /// Copy
  Expected(Expected const& other) noexcept {
    if (!other.has_value())
      new (&buffer) Error(other.error());

    has_val = other.has_value();
  }

  /// Move
  // FIXME: Implement move semantics when move swift values is possible
  [[noreturn]] Expected(Expected&&) noexcept { abort(); }

  ~Expected() noexcept {
    if (!has_value())
      reinterpret_cast<swift::Error *>(buffer)->~Error();
  }

  /// assignment
  constexpr auto operator=(Expected&& other) noexcept = delete;
  constexpr auto operator=(Expected&) noexcept = delete;


  constexpr explicit operator bool() const noexcept { return has_value(); }

  // Get error
  constexpr swift::Error const &error() const & {
    if (has_value())
      abort();
    return reinterpret_cast<const swift::Error &>(buffer);
  }

  constexpr swift::Error &error() & {
    if (has_value())
      abort();
    return reinterpret_cast<swift::Error &>(buffer);
  }

  constexpr bool has_value() const noexcept { return has_val; }
private:
  alignas(alignof(swift::Error)) char buffer[sizeof(swift::Error)];
  bool has_val;
};

#ifdef __cpp_exceptions

template<class T>
using ThrowingResult = T;

#define SWIFT_RETURN_THUNK(T, v) v
#define SWIFT_NORETURN_EXCEPT_ERRORS SWIFT_NORETURN

#else

template <class T> using ThrowingResult = swift::Expected<T>;

#define SWIFT_RETURN_THUNK(T, v) swift::Expected<T>(v)
#define SWIFT_NORETURN_EXCEPT_ERRORS

#endif

#endif // SWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR
#endif // SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR && !__EmbeddedSwift__

#endif
