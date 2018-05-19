//===--- Assert.cpp - Assertion failure reporting -------------------------===//
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

#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"
#include "../SwiftShims/AssertionReporting.h"
#include <cstdarg>
#include <cstdint>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

using namespace swift;

static void logPrefixAndMessageToDebugger(
    const unsigned char *prefix, int prefixLength,
    const unsigned char *message, int messageLength
) {
  if (!_swift_shouldReportFatalErrorsToDebugger())
    return;

  if (messageLength) {
    _swift_reportToDebugger(RuntimeErrorFlagFatal, nullptr,
                            "%.*s: %.*s", prefixLength, prefix,
                            messageLength, message);
  } else {
    _swift_reportToDebugger(RuntimeErrorFlagFatal, nullptr,
                            "%.*s", prefixLength, prefix);
  }
}

void swift::_swift_stdlib_reportFatalErrorInFile(
    const unsigned char *prefix, int prefixLength,
    const unsigned char *message, int messageLength,
    const unsigned char *file, int fileLength,
    uint32_t line,
    uint32_t flags
) {
  logPrefixAndMessageToDebugger(prefix, prefixLength, message, messageLength);

  swift_reportError(flags, "%.*s: %.*s%sfile %.*s, line %" PRIu32 "\n",
                    prefixLength, prefix,
                    messageLength, message,
                    (messageLength ? ": " : ""),
                    fileLength, file,
                    line);
}

void swift::_swift_stdlib_reportFatalError(
    const unsigned char *prefix, int prefixLength,
    const unsigned char *message, int messageLength,
    uint32_t flags
) {
  logPrefixAndMessageToDebugger(prefix, prefixLength, message, messageLength);

  swift_reportError(flags, "%.*s: %.*s\n",
                    prefixLength, prefix,
                    messageLength, message);
}

void swift::_swift_stdlib_reportUnimplementedInitializerInFile(
    const unsigned char *className, int classNameLength,
    const unsigned char *initName, int initNameLength,
    const unsigned char *file, int fileLength,
    uint32_t line, uint32_t column,
    uint32_t flags
) {
  swift_reportError(flags,
            "%.*s: %" PRIu32 ": %" PRIu32 ": Fatal error: Use of unimplemented "
            "initializer '%.*s' for class '%.*s'\n",
            fileLength, file,
            line, column,
            initNameLength, initName,
            classNameLength, className);
}

void swift::_swift_stdlib_reportUnimplementedInitializer(
    const unsigned char *className, int classNameLength,
    const unsigned char *initName, int initNameLength,
    uint32_t flags
) {
  swift_reportError(flags,
                    "Fatal error: Use of unimplemented "
                    "initializer '%.*s' for class '%.*s'\n",
                    initNameLength, initName,
                    classNameLength, className);
}

