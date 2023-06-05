//===--- Backtrace.cpp - Swift crash catching and backtracing support ---- ===//
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
// Definitions relating to backtracing.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_BACKTRACE_H
#define SWIFT_RUNTIME_BACKTRACE_H

#ifdef __linux__
#include <sys/types.h>
#include <sys/wait.h>

#include <signal.h>
#endif // defined(__linux__)

#include "swift/Runtime/Config.h"
#include "swift/Runtime/CrashInfo.h"

#include "swift/shims/Visibility.h"

#include <inttypes.h>

#ifdef _WIN32
// For DWORD
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>

// For wchar_t
#include <cwchar>
#endif

#ifdef __cplusplus
namespace swift {
namespace runtime {
namespace backtrace {
#endif

#ifdef _WIN32
typedef wchar_t ArgChar;
typedef DWORD ErrorCode;
#else
typedef char ArgChar;
typedef int ErrorCode;
#endif

SWIFT_RUNTIME_STDLIB_INTERNAL ErrorCode _swift_installCrashHandler();

#ifdef __linux__
SWIFT_RUNTIME_STDLIB_INTERNAL bool _swift_spawnBacktracer(const ArgChar * const *argv, int memserver_fd);
#else
SWIFT_RUNTIME_STDLIB_INTERNAL bool _swift_spawnBacktracer(const ArgChar * const *argv);
#endif

enum class UnwindAlgorithm {
  Auto = 0,
  Fast = 1,
  Precise = 2
};

enum class OnOffTty {
  Off = 0,
  On = 1,
  TTY = 2
};

enum class Preset {
  Auto = -1,
  Friendly = 0,
  Medium = 1,
  Full = 2
};

enum class ThreadsToShow {
  Preset = -1,
  All = 0,
  Crashed = 1
};

enum class RegistersToShow {
  Preset = -1,
  None = 0,
  All = 1,
  Crashed = 2
};

enum class ImagesToShow {
  Preset = -1,
  None = 0,
  All = 1,
  Mentioned = 2
};

enum class SanitizePaths {
  Preset = -1,
  Off = 0,
  On = 1
};

enum class OutputTo {
  Stdout = 0,
  Stderr = 2,
};

struct BacktraceSettings {
  UnwindAlgorithm  algorithm;
  OnOffTty         enabled;
  bool             demangle;
  OnOffTty         interactive;
  OnOffTty         color;
  unsigned         timeout;
  ThreadsToShow    threads;
  RegistersToShow  registers;
  ImagesToShow     images;
  unsigned         limit;
  unsigned         top;
  SanitizePaths    sanitize;
  Preset           preset;
  bool             cache;
  OutputTo         outputTo;
  const char      *swiftBacktracePath;
};

SWIFT_RUNTIME_STDLIB_INTERNAL BacktraceSettings _swift_backtraceSettings;

SWIFT_RUNTIME_STDLIB_SPI SWIFT_CC(swift) bool _swift_isThunkFunction(const char *mangledName);

SWIFT_RUNTIME_STDLIB_SPI
char *_swift_backtrace_demangle(const char *mangledName,
                                size_t mangledNameLength,
                                char *outputBuffer,
                                size_t *outputBufferSize,
                                int *status);
#ifdef __cplusplus
} // namespace backtrace
} // namespace runtime
} // namespace swift
#endif

#endif // SWIFT_RUNTIME_BACKTRACE_H
