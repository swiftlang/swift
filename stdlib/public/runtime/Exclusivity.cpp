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

