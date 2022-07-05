//===--- CrashReporter.h - Crash Reporter integration -----------*- C++ -*-===//
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
// Declares gCRAnnotations.  This lets us link with other static libraries
// that also declare gCRAnnotations, because we'll pull in their copy
// (assuming they're linked first).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_CRASHREPORTER_H
#define SWIFT_DEMANGLING_CRASHREPORTER_H

#if SWIFT_HAVE_CRASHREPORTERCLIENT

// For SWIFT_LIBRARY_VISIBILITY
#include "swift/Basic/Visibility.h"

#include <inttypes.h>

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
SWIFT_LIBRARY_VISIBILITY
extern struct crashreporter_annotations_t gCRAnnotations;
}

#endif // SWIFT_HAVE_CRASHREPORTERCLIENT

#endif // SWIFT_DEMANGLING_CRASHREPORTER_H
