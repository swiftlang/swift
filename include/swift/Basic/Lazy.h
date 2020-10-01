//===--- Lazy.h - A lazily-initialized object -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_LAZY_H
#define SWIFT_BASIC_LAZY_H

#include <memory>
#ifdef SWIFT_STDLIB_SINGLE_THREADED_RUNTIME
// No dependencies on single-threaded environments.
#elif defined(__APPLE__)
#include <dispatch/dispatch.h>
#elif defined(__wasi__)
// No pthread on wasi, see https://bugs.swift.org/browse/SR-12097 for more details.
#else
#include <mutex>
#endif
#include "swift/Basic/Malloc.h"
#include "swift/Basic/type_traits.h"

#if defined(__wasi__)
// Temporary single-threaded stub. Should be replaced with a thread-safe version
// as soon as the WASI SDK allows it. See https://bugs.swift.org/browse/SR-12766.
inline void wasi_call_once(int *flag, void *context, void (*func)(void *)) {
  switch (*flag) {
  case 0:
    *flag = 1;
    func(context);
    return;
  case 1:
    return;
  default:
    assert(false && "wasi_call_once got invalid flag");
    abort();
  }
}
#endif

namespace swift {

#ifdef SWIFT_STDLIB_SINGLE_THREADED_RUNTIME
  using OnceToken_t = bool;
# define SWIFT_ONCE_F(TOKEN, FUNC, CONTEXT) \
  if (!TOKEN) { TOKEN = true; (FUNC)(CONTEXT); }
#elif defined(__APPLE__)
  using OnceToken_t = dispatch_once_t;
# define SWIFT_ONCE_F(TOKEN, FUNC, CONTEXT) \
  ::dispatch_once_f(&TOKEN, CONTEXT, FUNC)
#elif defined(__CYGWIN__)
  // _swift_once_f() is declared in Private.h.
  // This prototype is copied instead including the header file.
  void _swift_once_f(uintptr_t *predicate, void *context,
                     void (*function)(void *));
  using OnceToken_t = unsigned long;
# define SWIFT_ONCE_F(TOKEN, FUNC, CONTEXT) \
  _swift_once_f(&TOKEN, CONTEXT, FUNC)
#elif defined(__wasi__)
  using OnceToken_t = int;
# define SWIFT_ONCE_F(TOKEN, FUNC, CONTEXT) \
  ::wasi_call_once(&TOKEN, CONTEXT, FUNC)
#else
  using OnceToken_t = std::once_flag;
# define SWIFT_ONCE_F(TOKEN, FUNC, CONTEXT) \
  ::std::call_once(TOKEN, FUNC, CONTEXT)
#endif

/// A template for lazily-constructed, zero-initialized, leaked-on-exit
/// global objects.
template <class T> class Lazy {
  alignas(T) char Value[sizeof(T)] = { 0 };

  OnceToken_t OnceToken = {};

  static void defaultInitCallback(void *ValueAddr) {
    ::new (ValueAddr) T();
  }
  
public:
  using Type = T;
  
  T &get(void (*initCallback)(void *) = defaultInitCallback);

  template<typename Arg1>
  T &getWithInit(Arg1 &&arg1);

  /// Get the value, assuming it must have already been initialized by this
  /// point.
  T &unsafeGetAlreadyInitialized() { return *reinterpret_cast<T *>(&Value); }

  constexpr Lazy() = default;

  T *operator->() { return &get(); }
  T &operator*() { return get(); }

private:
  Lazy(const Lazy &) = delete;
  Lazy &operator=(const Lazy &) = delete;
};

template <typename T> inline T &Lazy<T>::get(void (*initCallback)(void*)) {
  static_assert(std::is_literal_type<Lazy<T>>::value,
                "Lazy<T> must be a literal type");

  SWIFT_ONCE_F(OnceToken, initCallback, &Value);
  return unsafeGetAlreadyInitialized();
}

template <typename T>
template <typename Arg1> inline T &Lazy<T>::getWithInit(Arg1 &&arg1) {
  struct Data {
    void *address;
    Arg1 &&arg1;

    static void init(void *context) {
      Data *data = static_cast<Data *>(context);
      ::new (data->address) T(static_cast<Arg1&&>(data->arg1));
    }
  } data{&Value, static_cast<Arg1&&>(arg1)};

  SWIFT_ONCE_F(OnceToken, &Data::init, &data);
  return unsafeGetAlreadyInitialized();
}

} // end namespace swift

#define SWIFT_LAZY_CONSTANT(INITIAL_VALUE) \
  ([]{ \
    using T = ::std::remove_reference<decltype(INITIAL_VALUE)>::type; \
    static ::swift::Lazy<T> TheLazy; \
    return TheLazy.get([](void *ValueAddr){ ::new(ValueAddr) T{INITIAL_VALUE}; });\
  }())

#endif // SWIFT_BASIC_LAZY_H
