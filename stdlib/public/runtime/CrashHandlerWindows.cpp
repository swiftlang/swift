//===--- CrashHandlerWindows.cpp - Swift crash handler for Windows ------- ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The Windows crash handler implementation.
//
//===----------------------------------------------------------------------===//

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "swift/Runtime/Backtrace.h"
#include "BacktracePrivate.h"

using namespace swift::runtime::backtrace;

namespace {

SRWLOCK crashLock = SRWLOCK_INIT;

CrashInfo crashInfo;

void *pcFromContext(PCONTEXT Context);
LONG NTAPI handleException(EXCEPTION_POINTERS *ExceptionInfo);
LONG NTAPI reinstallUnhandledExceptionFilter(EXCEPTION_POINTERS *ExceptionInfo);

}

namespace swift {
namespace runtime {
namespace backtrace {

SWIFT_RUNTIME_STDLIB_INTERNAL ErrorCode
_swift_installCrashHandler()
{
  AddVectoredExceptionHandler(0, reinstallUnhandledExceptionFilter);
  SetUnhandledExceptionFilter(handleException);

  return ERROR_SUCCESS;
}

} // namespace backtrace
} // namespace runtime
} // namespace swift

namespace {

LPTOP_LEVEL_EXCEPTION_FILTER pOldFilter = NULL;

// Yuck.  This is horrible because the C runtime's startup code sets the
// unhandled exception filter, but that happens *after*
// _swift_installCrashHandler() has run because *that* is called during DLL
// initialization.
//
// Since we don't have a top-of-main() entry point for Swift, for now we
// hack things by reinstalling our filter from the vectored exception
// handler.  This is utterly horrible.
LONG reinstallUnhandledExceptionFilter(EXCEPTION_POINTERS *ExceptionInfo) {
  LPTOP_LEVEL_EXCEPTION_FILTER pFilter = SetUnhandledExceptionFilter(handleException);
  if (pFilter != handleException)
    pOldFilter = pFilter;
  return EXCEPTION_CONTINUE_SEARCH;
}

LONG handleException(EXCEPTION_POINTERS *ExceptionInfo) {
  HANDLE hOutput;
  if (_swift_backtraceSettings.outputTo == OutputTo::Stderr)
    hOutput = GetStdHandle(STD_ERROR_HANDLE);
  else
    hOutput = GetStdHandle(STD_OUTPUT_HANDLE);

  AcquireSRWLockExclusive(&crashLock);

  crashInfo.crashing_thread = GetCurrentThreadId();
  crashInfo.signal = ExceptionInfo->ExceptionRecord->ExceptionCode;
  crashInfo.fault_address = (uint64_t)(ExceptionInfo->ExceptionRecord->ExceptionAddress);
  crashInfo.exception_info = (uint64_t)ExceptionInfo;

  _swift_displayCrashMessage(ExceptionInfo->ExceptionRecord->ExceptionCode,
                             pcFromContext(ExceptionInfo->ContextRecord));

  // It isn't safe to try to stop all the threads here on Windows, so we
  // delegate doing that to the backtracer process.

  if (!_swift_spawnBacktracer(&crashInfo)) {
    const char *message = _swift_backtraceSettings.color == OnOffTty::On
      ? " failed\n\n" : " failed ***\n\n";
    WriteFile(hOutput, message, strlen(message), NULL, NULL);
  }

  ReleaseSRWLockExclusive(&crashLock);

  if (pOldFilter)
    return pOldFilter(ExceptionInfo);

  return EXCEPTION_EXECUTE_HANDLER;
}

void *pcFromContext(PCONTEXT Context) {
#if defined(_M_X64)
  return (void *)(Context->Rip);
#elif defined(_M_IX86)
  return (void *)(Context->Eip);
#elif defined(_M_ARM) || defined(_M_ARM64)
  return (void *)(Context->Pc);
#endif
}

} // namespace

#endif // _WIN32
