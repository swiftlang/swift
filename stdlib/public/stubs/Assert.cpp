//===--- Assert.cpp - Assertion failure reporting -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementation of
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"
#include <cstdarg>
#include <cstdint>
#include <stdio.h>
#include <stdlib.h>

using namespace swift;

static int swift_asprintf(char **strp, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
#if defined(_MSC_VER)
  int len = _vscprintf(fmt, args);
  if (len < 0) {
    va_end(args);
    return -1;
  }
  char *buffer = static_cast<char *>(malloc(len + 1));
  if (!buffer) {
    va_end(args);
    return -1;
  }
  int result = vsprintf(buffer, fmt, args);
  if (result < 0) {
    va_end(args);
    free(buffer);
    return -1;
  }
  *strp = buffer;
#else
  int result = vasprintf(strp, fmt, args);
#endif
  va_end(args);
  return result;
}

// Report a fatal error to system console, stderr, and crash logs.
// <prefix>: <message>: file <file>, line <line>\n
// The message may be omitted by passing messageLength=0.
SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_stdlib_reportFatalErrorInFile(const char *prefix, intptr_t prefixLength,
                                   const char *message, intptr_t messageLength,
                                   const char *file, intptr_t fileLength,
                                   uintptr_t line,
                                     uint32_t flags) {
  char *log;
  swift_asprintf(&log, "%.*s: %.*s%sfile %.*s, line %zu\n", (int)prefixLength,
                 prefix, (int)messageLength, message,
                 (messageLength ? ": " : ""), (int)fileLength, file,
                 (size_t)line);

  swift_reportError(flags, log);
  free(log);
}

// Report a fatal error to system console, stderr, and crash logs.
// <prefix>: <message>: file <file>, line <line>\n
// The message may be omitted by passing messageLength=0.
SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_stdlib_reportFatalError(const char *prefix,
                               intptr_t prefixLength,
                               const char *message,
                               intptr_t messageLength,
                               uint32_t flags) {
  char *log;
  swift_asprintf(&log, "%.*s: %.*s\n", (int)prefixLength, prefix,
                 (int)messageLength, message);

  swift_reportError(flags, log);
  free(log);
}

// Report a call to an unimplemented initializer.
// <file>: <line>: <column>: fatal error: use of unimplemented
// initializer '<initName>' for class 'className'
SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_stdlib_reportUnimplementedInitializerInFile(
         const char *className, intptr_t classNameLength, const char *initName,
         intptr_t initNameLength, const char *file, intptr_t fileLength,
         uintptr_t line, uintptr_t column, uint32_t flags) {
  char *log;
  swift_asprintf(&log, "%.*s: %zu: %zu: fatal error: use of unimplemented "
                       "initializer '%.*s' for class '%.*s'\n",
                 (int)fileLength, file, (size_t)line, (size_t)column,
                 (int)initNameLength, initName, (int)classNameLength,
                 className);

  swift_reportError(flags, log);
  free(log);
}

// Report a call to an unimplemented initializer.
// fatal error: use of unimplemented initializer '<initName>' for class
// 'className'
SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" void
_swift_stdlib_reportUnimplementedInitializer(const char *className,
                                             intptr_t classNameLength,
                                             const char *initName,
                                             intptr_t initNameLength,
                                             uint32_t flags) {
  char *log;
  swift_asprintf(&log, "fatal error: use of unimplemented "
                       "initializer '%.*s' for class '%.*s'\n",
                 (int)initNameLength, initName, (int)classNameLength,
                 className);

  swift_reportError(flags, log);
  free(log);
}

