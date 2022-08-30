//===--- _SwiftCxxInteroperability.h - C++ Interop support ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines types and support functions required by C++ bindings generated
//  by the Swift compiler that allow C++ code to call Swift APIs.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CXX_INTEROPERABILITY_H
#define SWIFT_CXX_INTEROPERABILITY_H

#ifdef __cplusplus

#include <cstdint>
#include <stdlib.h>
#if defined(_WIN32)
#include <malloc.h>
#endif

namespace swift {
namespace _impl {

extern "C" void *_Nonnull swift_retain(void *_Nonnull) noexcept;

extern "C" void swift_release(void *_Nonnull) noexcept;

extern "C" void *_Nonnull swift_errorRetain(void *_Nonnull swiftError) noexcept;

extern "C" void swift_errorRelease(void *_Nonnull swiftError) noexcept;

inline void *_Nonnull opaqueAlloc(size_t size, size_t align) noexcept {
#if defined(_WIN32)
  void *r = _aligned_malloc(size, align);
#else
  if (align < sizeof(void *))
    align = sizeof(void *);
  void *r = nullptr;
  int res = posix_memalign(&r, align, size);
  (void)res;
#endif
  return r;
}

inline void opaqueFree(void *_Nonnull p) noexcept {
#if defined(_WIN32)
  _aligned_free(p);
#else
  free(p);
#endif
}

/// Base class for a container for an opaque Swift value, like resilient struct.
class OpaqueStorage {
public:
  inline OpaqueStorage() noexcept : storage(nullptr) {}
  inline OpaqueStorage(size_t size, size_t alignment) noexcept
      : storage(reinterpret_cast<char *>(opaqueAlloc(size, alignment))) {}
  inline OpaqueStorage(OpaqueStorage &&other) noexcept
      : storage(other.storage) {
    other.storage = nullptr;
  }
  inline OpaqueStorage(const OpaqueStorage &) noexcept = delete;

  inline ~OpaqueStorage() noexcept {
    if (storage) {
      opaqueFree(static_cast<char *_Nonnull>(storage));
    }
  }

  void operator=(OpaqueStorage &&other) noexcept {
    auto temp = storage;
    storage = other.storage;
    other.storage = temp;
  }
  void operator=(const OpaqueStorage &) noexcept = delete;

  inline char *_Nonnull getOpaquePointer() noexcept {
    return static_cast<char *_Nonnull>(storage);
  }
  inline const char *_Nonnull getOpaquePointer() const noexcept {
    return static_cast<char *_Nonnull>(storage);
  }

private:
  char *_Nullable storage;
};

/// Base class for a Swift reference counted class value.
class RefCountedClass {
public:
  inline ~RefCountedClass() { swift_release(_opaquePointer); }
  inline RefCountedClass(const RefCountedClass &other) noexcept
      : _opaquePointer(other._opaquePointer) {
    swift_retain(_opaquePointer);
  }
  inline RefCountedClass &operator=(const RefCountedClass &other) noexcept {
    swift_retain(other._opaquePointer);
    swift_release(_opaquePointer);
    _opaquePointer = other._opaquePointer;
    return *this;
  }
  // FIXME: What to do in 'move'?
  inline RefCountedClass(RefCountedClass &&) noexcept = default;

protected:
  inline RefCountedClass(void *_Nonnull ptr) noexcept : _opaquePointer(ptr) {}

private:
  void *_Nonnull _opaquePointer;
  friend class _impl_RefCountedClass;
};

class _impl_RefCountedClass {
public:
  static inline void *_Nonnull getOpaquePointer(const RefCountedClass &object) {
    return object._opaquePointer;
  }
  static inline void *_Nonnull &getOpaquePointerRef(RefCountedClass &object) {
    return object._opaquePointer;
  }
};

} // namespace _impl

/// Swift's Int type.
using Int = ptrdiff_t;

/// Swift's UInt type.
using UInt = size_t;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wc++17-extensions"

/// True if the given type is a Swift type that can be used in a generic context
/// in Swift.
template <class T>
static inline const constexpr bool isUsableInGenericContext = false;

/// Returns the type metadat for the given Swift type T.
template <class T> inline void *_Nonnull getTypeMetadata();

namespace _impl {

/// Type trait that returns the `_impl::_impl_<T>` class type for the given
/// class T.
template <class T> struct implClassFor {
  // using type = ...;
};

/// True if the given type is a Swift value type.
template <class T> static inline const constexpr bool isValueType = false;

/// True if the given type is a Swift value type with opaque layout that can be
/// boxed.
template <class T> static inline const constexpr bool isOpaqueLayout = false;

/// Returns the opaque pointer to the given value.
template <class T>
inline const void *_Nonnull getOpaquePointer(const T &value) {
  if constexpr (isOpaqueLayout<T>)
    return reinterpret_cast<const OpaqueStorage &>(value).getOpaquePointer();
  return reinterpret_cast<const void *>(&value);
}

template <class T> inline void *_Nonnull getOpaquePointer(T &value) {
  if constexpr (isOpaqueLayout<T>)
    return reinterpret_cast<OpaqueStorage &>(value).getOpaquePointer();
  return reinterpret_cast<void *>(&value);
}

namespace swift {
class Error {
public:
  Error() {}
  Error(void* _Nonnull swiftError) { opaqueValue = swiftError; }
  ~Error() {
    if (opaqueValue)
      swift_errorRelease(opaqueValue);
  }
  void* _Nonnull getPointerToOpaquePointer() { return opaqueValue; }
  Error(Error &&other) : opaqueValue(other.opaqueValue) {
    other.opaqueValue = nullptr;
  }
  Error(const Error &other) {
    if (other.opaqueValue)
      swift_errorRetain(other.opaqueValue);
    opaqueValue = other.opaqueValue;
  }

private:
  void * _Nonnull opaqueValue = nullptr;
};
}

} // namespace _impl

#pragma clang diagnostic pop

} // namespace swift
#endif

#endif // SWIFT_CXX_INTEROPERABILITY_H
