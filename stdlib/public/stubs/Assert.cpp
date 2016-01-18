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

#include "swift/Runtime/Debug.h"
#include <cstdint>
#include <stdio.h>
#include <stdlib.h>

using namespace swift;

// Report a fatal error to system console, stderr, and crash logs.
// <prefix>: <message>: file <file>, line <line>\n
// The message may be omitted by passing messageLength=0.
extern "C" void
_swift_stdlib_reportFatalErrorInFile(const char *prefix, intptr_t prefixLength,
                                   const char *message, intptr_t messageLength,
                                   const char *file, intptr_t fileLength,
                                   uintptr_t line) {
  char *log;
  asprintf(&log, "%.*s: %.*s%sfile %.*s, line %zu\n", (int)prefixLength, prefix,
           (int)messageLength, message, (messageLength ? ": " : ""),
           (int)fileLength, file, (size_t)line);
  
  swift_reportError(log);
  free(log);
}

// Report a fatal error to system console, stderr, and crash logs.
// <prefix>: <message>: file <file>, line <line>\n
// The message may be omitted by passing messageLength=0.
extern "C" void
_swift_stdlib_reportFatalError(const char *prefix,
                               intptr_t prefixLength,
                               const char *message,
                               intptr_t messageLength) {
  char *log;
  asprintf(&log, "%.*s: %.*s\n", (int)prefixLength, prefix,
           (int)messageLength, message);
  
  swift_reportError(log);
  free(log);
}

// Report a call to an unimplemented initializer.
// <file>: <line>: <column>: fatal error: use of unimplemented
// initializer '<initName>' for class 'className'
extern "C" void
_swift_stdlib_reportUnimplementedInitializerInFile(
         const char *className, intptr_t classNameLength, const char *initName,
         intptr_t initNameLength, const char *file, intptr_t fileLength,
         uintptr_t line, uintptr_t column) {
  char *log;
  asprintf(&log, "%.*s: %zu: %zu: fatal error: use of unimplemented "
           "initializer '%.*s' for class '%.*s'\n",
           (int)fileLength, file, (size_t)line, (size_t)column,
           (int)initNameLength, initName, (int)classNameLength, className);

  swift_reportError(log);
  free(log);
}

// Report a call to an unimplemented initializer.
// fatal error: use of unimplemented initializer '<initName>' for class
// 'className'
extern "C" void
_swift_stdlib_reportUnimplementedInitializer(const char *className,
                                             intptr_t classNameLength,
                                             const char *initName,
                                             intptr_t initNameLength) {
  char *log;
  asprintf(&log, "fatal error: use of unimplemented "
           "initializer '%.*s' for class '%.*s'\n",
           (int)initNameLength, initName, (int)classNameLength, className);

  swift_reportError(log);
  free(log);
}

