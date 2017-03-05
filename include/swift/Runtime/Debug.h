//===--- Debug.h - Swift Runtime debug helpers ------------------*- C++ -*-===//
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
// Random debug support
//
//===----------------------------------------------------------------------===//

#ifndef _SWIFT_RUNTIME_DEBUG_HELPERS_
#define _SWIFT_RUNTIME_DEBUG_HELPERS_

#include <llvm/Support/Compiler.h>
#include <stdint.h>
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Unreachable.h"

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
LLVM_LIBRARY_VISIBILITY
extern struct crashreporter_annotations_t gCRAnnotations;
}

LLVM_ATTRIBUTE_ALWAYS_INLINE
static void CRSetCrashLogMessage(const char *message) {
  gCRAnnotations.message = reinterpret_cast<uint64_t>(message);
}

LLVM_ATTRIBUTE_ALWAYS_INLINE
static const char *CRGetCrashLogMessage() {
  return reinterpret_cast<const char *>(gCRAnnotations.message);
}

#else

LLVM_ATTRIBUTE_ALWAYS_INLINE
static void CRSetCrashLogMessage(const char *) {}

#endif

namespace swift {

// Duplicated from Metadata.h. We want to use this header
// in places that cannot themselves include Metadata.h.
struct InProcess;
template <typename Runtime> struct TargetMetadata;
using Metadata = TargetMetadata<InProcess>;

// swift::crash() halts with a crash log message, 
// but otherwise tries not to disturb register state.

LLVM_ATTRIBUTE_NORETURN
LLVM_ATTRIBUTE_ALWAYS_INLINE // Minimize trashed registers
static inline void crash(const char *message) {
  CRSetCrashLogMessage(message);

  LLVM_BUILTIN_TRAP;
  swift_runtime_unreachable("Expected compiler to crash.");
}

/// Report a corrupted type object.
LLVM_ATTRIBUTE_NORETURN
LLVM_ATTRIBUTE_ALWAYS_INLINE // Minimize trashed registers
static inline void _failCorruptType(const Metadata *type) {
  swift::crash("Corrupt Swift type object");
}

// swift::fatalError() halts with a crash log message, 
// but makes no attempt to preserve register state.
LLVM_ATTRIBUTE_NORETURN
extern void
fatalError(uint32_t flags, const char *format, ...);

// swift_dynamicCastFailure halts using fatalError()
// with a description of a failed cast's types.
LLVM_ATTRIBUTE_NORETURN
void
swift_dynamicCastFailure(const Metadata *sourceType,
                         const Metadata *targetType,
                         const char *message = nullptr);

// swift_dynamicCastFailure halts using fatalError()
// with a description of a failed cast's types.
LLVM_ATTRIBUTE_NORETURN
void
swift_dynamicCastFailure(const void *sourceType, const char *sourceName, 
                         const void *targetType, const char *targetName, 
                         const char *message = nullptr);

SWIFT_RUNTIME_EXPORT
void swift_reportError(uint32_t flags, const char *message);

// Halt due to an overflow in swift_retain().
LLVM_ATTRIBUTE_NORETURN LLVM_ATTRIBUTE_NOINLINE
void swift_abortRetainOverflow();

// Halt due to reading an unowned reference to a dead object.
LLVM_ATTRIBUTE_NORETURN LLVM_ATTRIBUTE_NOINLINE
void swift_abortRetainUnowned(const void *object);

// namespace swift
}

#endif // _SWIFT_RUNTIME_DEBUG_HELPERS_
