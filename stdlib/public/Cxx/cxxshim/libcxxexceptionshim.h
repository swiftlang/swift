//===----------------------------------------------------------------------===//
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

#ifndef SWIFT_CXX_EXCEPTION_SHIM_H
#define SWIFT_CXX_EXCEPTION_SHIM_H

#if !__has_feature(cxx_exceptions)
#error "C++ exception bridging requires C++ exceptions"
#endif

// The Microsoft C++ ABI implements exceptions on top of SEH and is not
// supported yet.
#if !__has_include(<cxxabi.h>)
#error "C++ exception bridging requires the Itanium C++ exception ABI"
#endif

#include <cxxabi.h>
#include <exception>
#include <new>

// The foreign exception check below depends on how the C++ runtime treats
// foreign exceptions, not on the platform. It has been verified for libc++abi
// and for libstdc++'s libsupc++.
#if !defined(_LIBCPPABI_VERSION) && !defined(__GLIBCXX__)
#error "C++ exception bridging requires the libc++abi or libstdc++ runtime"
#endif

// Apple's Objective-C runtime throws Objective-C exceptions as native C++
// exceptions, so they can only be told apart with Objective-C exception
// handling.
#if defined(__APPLE__) && !defined(__OBJC__)
#error "C++ exception bridging on Darwin requires Objective-C interoperability"
#endif

namespace __swift_cxx_exception_support {

inline void reportNativeException(
    void *context,
    void (*callback)(void *context, const char *message) noexcept) noexcept {
  try {
    throw;
  } catch (const std::exception &exception) {
    callback(context, exception.what());
  } catch (...) {
    callback(context, "Unknown C++ exception");
  }
}

} // namespace __swift_cxx_exception_support

/// Report the currently caught C++ exception to a synchronous callback.
///
/// Call this only from a C++ catch handler. The callback must copy the message
/// before returning. No exception object or borrowed message pointer may escape
/// the callback. Foreign exceptions, including forced unwinding for thread
/// cancellation, cannot unwind through Swift frames.
inline void __swift_cxx_report_current_exception(
    void *context,
    void (*callback)(void *context, const char *message) noexcept) noexcept {
  // On libc++abi and libstdc++, current_exception returns null for foreign
  // exceptions without accessing native exception fields. This also rejects
  // calls outside an active exception handler. __cxa_current_exception_type is
  // not used because libstdc++ reads exceptionType from a fabricated
  // __cxa_exception header when the caught exception is foreign.
  if (!std::current_exception())
    std::terminate();

#if defined(__OBJC__)
  // Darwin represents Objective-C exceptions using the C++ exception ABI, so
  // capturing the exception alone cannot distinguish them. Let the Objective-C
  // runtime identify its exception objects before reporting a C++ exception.
  @try {
    __cxxabiv1::__cxa_rethrow();
  } @catch (id) {
    std::terminate();
  } @catch (...) {
    __swift_cxx_exception_support::reportNativeException(context, callback);
  }
#else
  __swift_cxx_exception_support::reportNativeException(context, callback);
#endif
}

#endif // SWIFT_CXX_EXCEPTION_SHIM_H
