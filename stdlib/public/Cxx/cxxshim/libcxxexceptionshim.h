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

#if !defined(__APPLE__) && !defined(__linux__)
#error "C++ exception bridging is only supported on Darwin and Linux"
#endif

#if defined(__APPLE__) && !defined(__OBJC__)
#error "C++ exception bridging on Darwin requires Objective-C interoperability"
#endif

#if !__has_feature(cxx_exceptions)
#error "C++ exception bridging requires C++ exceptions"
#endif

#include <cxxabi.h>
#include <exception>

namespace __swift_cxx_exception_support {

using Callback = void (*)(void *context, const char *message);

inline void reportNativeException(void *context, Callback callback) noexcept {
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
/// before returning and must not throw. No exception object or borrowed message
/// pointer may escape the callback. Foreign exceptions, including forced
/// unwinding for thread cancellation, cannot unwind through Swift frames.
inline void __swift_cxx_report_current_exception(
    void *context, __swift_cxx_exception_support::Callback callback) noexcept {
  // On the supported Itanium runtimes, a foreign exception has no C++ type.
  // This also rejects calls outside an active exception handler.
  if (!__cxxabiv1::__cxa_current_exception_type())
    std::terminate();

#if defined(__OBJC__)
  // Darwin represents Objective-C exceptions using the C++ exception ABI, so
  // a non-null C++ type alone cannot distinguish them. Let the Objective-C
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
