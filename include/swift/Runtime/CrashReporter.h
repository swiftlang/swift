//===--- CrashReporter.h - Crash Reporter integration -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines gCRAnnotations and the code that accumulates reported messages in
// it. The demangling library and the runtime both report messages, and they
// aren't always linked together, so this is inline code with inline statics.
// The linker coalesces those, giving every image one lock and one record of the
// message it owns, alongside its one gCRAnnotations.
//
// gCRAnnotations is inline, so a binary linking a static library that defines
// it too, such as LLVMSupport, gets that library's copy regardless of link
// order.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_CRASHREPORTER_H
#define SWIFT_RUNTIME_CRASHREPORTER_H

#include "swift/Runtime/Config.h"

#include <stdint.h>

#if __STDC_HOSTED__
#include <stdlib.h>
#include <string.h>
#endif

namespace swift {

// appendToCrashLogMessage need malloc which is not available in freestanding
// mode.
#if __STDC_HOSTED__

// The number of bytes of previously reported messages to keep. The message
// being reported is always kept in full, however long it is.
constexpr size_t crashLogMessageLimit = 4096;

/// Concatenate oldMessage and message, dropping enough of the front of
/// oldMessage to fit within crashLogMessageLimit. Returns a malloc'd string,
/// or nullptr if the allocation fails.
///
/// Also used by the runtime's non-CrashReporter message handling in
/// RuntimeErrorReporting.cpp.
inline char *appendToCrashLogMessage(const char *oldMessage,
                                     const char *message) {
  size_t messageLength = strlen(message);
  size_t historyLimit = messageLength > crashLogMessageLimit
                            ? 0
                            : crashLogMessageLimit - messageLength;

  const char *history = oldMessage ? oldMessage : "";
  size_t historyLength = strlen(history);
  if (historyLength > historyLimit) {
    history += historyLength - historyLimit;
    historyLength = historyLimit;

    // Cut at a newline so the oldest retained message doesn't begin mid-line.
    if (const char *lineStart = strchr(history, '\n')) {
      historyLength -= lineStart + 1 - history;
      history = lineStart + 1;
    }
  }

  char *newMessage = (char *)malloc(historyLength + messageLength + 1);
  if (!newMessage)
    return nullptr;

  memcpy(newMessage, history, historyLength);
  memcpy(newMessage + historyLength, message, messageLength + 1);
  return newMessage;
}

#endif // __STDC_HOSTED__

} // namespace swift

#ifdef SWIFT_HAVE_CRASHREPORTERCLIENT

#include "swift/Threading/Mutex.h"

#define CRASHREPORTER_ANNOTATIONS_VERSION 5
#define CRASHREPORTER_ANNOTATIONS_SECTION "__crash_info"

struct crashreporter_annotations_t {
  uint64_t version;          // unsigned long
  uint64_t message;          // char *
  uint64_t signature_string; // char *
  uint64_t backtrace;        // char *
  uint64_t message2;         // char *
  uint64_t thread;           // uint64_t
  uint64_t dialog_mode;      // unsigned int
  uint64_t abort_cause;      // unsigned int
};

// Instead of linking to CrashReporterClient.a (because it complicates the
// build system), define the only symbol from that static archive ourselves.
//
// The layout of this struct is CrashReporter ABI, so there are no ABI concerns
// here.
extern "C" {
SWIFT_RUNTIME_LIBRARY_VISIBILITY
inline struct crashreporter_annotations_t gCRAnnotations __attribute__((
    __section__("__DATA," CRASHREPORTER_ANNOTATIONS_SECTION))) = {
    CRASHREPORTER_ANNOTATIONS_VERSION, 0, 0, 0, 0, 0, 0, 0};
}

SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
static inline void CRSetCrashLogMessage(const char *message) {
  gCRAnnotations.message = reinterpret_cast<uint64_t>(message);
}

SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
static inline const char *CRGetCrashLogMessage() {
  return reinterpret_cast<const char *>(gCRAnnotations.message);
}

namespace swift {

/// Append a message to the one the crash reporter will record.
inline void appendCrashLogMessage(const char *message) {
  // Reading the old message, replacing it, and freeing it has to be exclusive.
  // Use an "unsafe" mutex, because the checked one reports a fatal error when
  // it detects a problem, and reporting an error comes back here.
  static LazyUnsafeMutex lock;

  // The message last stored here. Anything else in gCRAnnotations was put there
  // by other code, and try to defensively guard against that. This isn't safe
  // against concurrent modification by other code, but it does eliminate one
  // way for it to fail.
  static char *ownedMessage = nullptr;

  // LazyUnsafeMutex inherits LazyMutex::ScopedLock, which takes the checked
  // lock.
  ScopedLockT<LazyUnsafeMutex, false> guard(lock);

  char *oldMessage = const_cast<char *>(CRGetCrashLogMessage());
  char *newMessage = appendToCrashLogMessage(oldMessage, message);
  if (!newMessage)
    return;

  CRSetCrashLogMessage(newMessage);

  if (oldMessage && oldMessage == ownedMessage)
    free(oldMessage);
  ownedMessage = newMessage;
}

} // namespace swift

#else

// swift::crash() sets the message on every platform.
SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
static inline void CRSetCrashLogMessage(const char *) {}

#endif // SWIFT_HAVE_CRASHREPORTERCLIENT

#endif // SWIFT_RUNTIME_CRASHREPORTER_H
