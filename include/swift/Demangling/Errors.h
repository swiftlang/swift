//===--- Errors.h - Demangling library error handling -----------*- C++ -*-===//
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
//
// This file exists because not every client links to libswiftCore (the
// runtime), so calling swift::fatalError() or swift::warning() from within
// the demangler is not an option.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_ERRORS_H
#define SWIFT_DEMANGLING_ERRORS_H

#include "../../../stdlib/public/SwiftShims/Visibility.h"
#include "swift/Demangling/NamespaceMacros.h"
#include <inttypes.h>
#include <stdarg.h>

#ifndef SWIFT_FORMAT
// SWIFT_FORMAT(fmt,first) marks a function as taking a format string argument
// at argument `fmt`, with the first argument for the format string as `first`.
#if __has_attribute(format)
#define SWIFT_FORMAT(fmt, first) __attribute__((format(printf, fmt, first)))
#else
#define SWIFT_FORMAT(fmt, first)
#endif
#endif

#ifndef SWIFT_VFORMAT
// SWIFT_VFORMAT(fmt) marks a function as taking a format string argument at
// argument `fmt`, with the arguments in a `va_list`.
#if __has_attribute(format)
#define SWIFT_VFORMAT(fmt) __attribute__((format(printf, fmt, 0)))
#else
#define SWIFT_VFORMAT(fmt)
#endif
#endif

#ifdef SWIFT_HAVE_CRASHREPORTERCLIENT

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

extern "C" {
SWIFT_RUNTIME_LIBRARY_VISIBILITY
extern struct crashreporter_annotations_t gCRAnnotations;
}

SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
static inline void CRSetCrashLogMessage(const char *message) {
  gCRAnnotations.message = reinterpret_cast<uint64_t>(message);
}

SWIFT_RUNTIME_ATTRIBUTE_ALWAYS_INLINE
static inline const char *CRGetCrashLogMessage() {
  return reinterpret_cast<const char *>(gCRAnnotations.message);
}

#endif

namespace swift {
namespace Demangle {
SWIFT_BEGIN_INLINE_NAMESPACE

SWIFT_NORETURN SWIFT_FORMAT(2, 3) void fatal(uint32_t flags, const char *format,
                                             ...);
SWIFT_FORMAT(2, 3) void warn(uint32_t flags, const char *format, ...);

SWIFT_NORETURN SWIFT_VFORMAT(2) void fatalv(uint32_t flags, const char *format,
                                            va_list val);
SWIFT_VFORMAT(2) void warnv(uint32_t flags, const char *format, va_list val);

SWIFT_END_INLINE_NAMESPACE
} // end namespace Demangle
} // end namespace swift

#endif // SWIFT_DEMANGLING_DEMANGLE_H
