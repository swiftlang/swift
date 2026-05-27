//===--- Exclusivity.cpp - Exclusivity tracking ---------------------------===//
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
//
// This implements the runtime support for dynamically tracking exclusivity.
//
//===----------------------------------------------------------------------===//

// NOTE: This should really be applied in the CMakeLists.txt.  However, we do
// not have a way to currently specify that at the target specific level yet.

#if defined(_WIN32)
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#define VCEXTRALEAN
#endif

#include "swift/Runtime/Exclusivity.h"
#include "swift/shims/Visibility.h"
#include "SwiftTLSContext.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Threading/ThreadLocalStorage.h"
#include <cinttypes>
#include <cstdio>
#include <memory>

// Pick a return-address strategy
#if defined(__wasm__)
// Wasm can't access call frame for security purposes
#define get_return_address() ((void*) 0)
#elif __GNUC__
#define get_return_address() __builtin_return_address(0)
#elif _MSC_VER
#include <intrin.h>
#define get_return_address() _ReturnAddress()
#else
#error missing implementation for get_return_address
#define get_return_address() ((void*) 0)
#endif

using namespace swift;
using namespace swift::runtime;

bool swift::_swift_disableExclusivityChecking = false;

static const char *getAccessName(ExclusivityFlags flags) {
  switch (flags) {
  case ExclusivityFlags::Read: return "read";
  case ExclusivityFlags::Modify: return "modification";
  default: return "unknown";
  }
}

// In asserts builds if the environment variable
// SWIFT_DEBUG_RUNTIME_EXCLUSIVITY_LOGGING is set, emit logging information.
#ifndef NDEBUG

static inline bool isExclusivityLoggingEnabled() {
  return runtime::environment::SWIFT_DEBUG_RUNTIME_EXCLUSIVITY_LOGGING();
}

static inline void _flockfile_stderr() {
#if defined(_WIN32)
  _lock_file(stderr);
#elif defined(__wasi__)
  // FIXME: WebAssembly/WASI doesn't support file locking yet (https://github.com/apple/swift/issues/54533).
#else
  flockfile(stderr);
#endif
}

static inline void _funlockfile_stderr() {
#if defined(_WIN32)
  _unlock_file(stderr);
#elif defined(__wasi__)
  // FIXME: WebAssembly/WASI doesn't support file locking yet (https://github.com/apple/swift/issues/54533).
#else
  funlockfile(stderr);
#endif
}

/// Used to ensure that logging printfs are deterministic.
static inline void withLoggingLock(std::function<void()> func) {
  assert(isExclusivityLoggingEnabled() &&
         "Should only be called if exclusivity logging is enabled!");

  _flockfile_stderr();
  func();
  fflush(stderr);
  _funlockfile_stderr();
}

#endif

SWIFT_ALWAYS_INLINE
static void reportExclusivityConflict(ExclusivityFlags oldAction, void *oldPC,
                                      ExclusivityFlags newFlags, void *newPC,
                                      void *pointer) {
  constexpr unsigned maxMessageLength = 100;
  constexpr unsigned maxAccessDescriptionLength = 50;
  char message[maxMessageLength];
  snprintf(message, sizeof(message),
           "Simultaneous accesses to 0x%" PRIxPTR ", but modification requires "
           "exclusive access",
           reinterpret_cast<uintptr_t>(pointer));
  fprintf(stderr, "%s.\n", message);

  char oldAccess[maxAccessDescriptionLength];
  snprintf(oldAccess, sizeof(oldAccess),
           "Previous access (a %s) started at", getAccessName(oldAction));
  fprintf(stderr, "%s ", oldAccess);
  if (oldPC) {
    dumpStackTraceEntry(0, oldPC, /*shortOutput=*/true);
    fprintf(stderr, " (0x%" PRIxPTR ").\n", reinterpret_cast<uintptr_t>(oldPC));
  } else {
    fprintf(stderr, "<unknown>.\n");
  }

  char newAccess[maxAccessDescriptionLength];
  snprintf(newAccess, sizeof(newAccess), "Current access (a %s) started at",
           getAccessName(getAccessAction(newFlags)));
  fprintf(stderr, "%s:\n", newAccess);
  // The top frame is in swift_beginAccess, don't print it.
  constexpr unsigned framesToSkip = 1;
  printCurrentBacktrace(framesToSkip);

  RuntimeErrorDetails::Thread secondaryThread = {
    .description = oldAccess,
    .threadID = 0,
    .numFrames = 1,
    .frames = &oldPC
  };
  RuntimeErrorDetails details = {
    .version = RuntimeErrorDetails::currentVersion,
    .errorType = "exclusivity-violation",
    .currentStackDescription = newAccess,
    .framesToSkip = framesToSkip,
    .memoryAddress = pointer,
    .numExtraThreads = 1,
    .threads = &secondaryThread,
    .numFixIts = 0,
    .fixIts = nullptr,
    .numNotes = 0,
    .notes = nullptr,
  };
  _swift_reportToDebugger(RuntimeErrorFlagFatal, message, &details);
  fatalError(0, "Fatal access conflict detected.\n");
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_reportExclusivityConflict(uintptr_t oldAction, void *oldPC,
                                      uintptr_t newFlags, void *newPC,
                                      void *pointer) {
  reportExclusivityConflict((ExclusivityFlags)oldAction, oldPC,
                            (ExclusivityFlags)newFlags, newPC, pointer);
}

static SWIFT_THREAD_LOCAL_TYPE(TLSPointer<void>, swift::tls_key::exclusivity)
    AccessSetValue;

SWIFT_RUNTIME_STDLIB_INTERNAL
void * _Nullable _swift_getExclusivityTLSImpl() {
  return AccessSetValue.get();
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void _swift_setExclusivityTLSImpl(void * _Nullable newValue) {
  AccessSetValue.set(newValue);
}

// Declare two internal helpers from the Swift implementation that are used by
// the concurrency-specific code.
extern "C" void _swift_exclusivityAccessSetNext(void *access,
                                                void *_Nullable next);
extern "C" void *_swift_exclusivityAccessGetParent(void *access,
                                                   void *_Nullable child);

// Bring in the concurrency-specific exclusivity code.
#include "ConcurrencyExclusivity.inc"
