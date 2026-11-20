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
#include <psapi.h>

#include "swift/Runtime/Backtrace.h"
#include "BacktracePrivate.h"

using namespace swift::runtime::backtrace;

#ifndef STATUS_FAIL_FAST_EXCEPTION
#define STATUS_FAIL_FAST_EXCEPTION ((DWORD)0xC0000602L)
#endif

namespace {

typedef VOID (WINAPI *RaiseFailFastException_t)(PEXCEPTION_RECORD,
                                                PCONTEXT,
                                                DWORD);

SRWLOCK crashLock = SRWLOCK_INIT;

CrashInfo crashInfo;

void *altStackTop;

void *pcFromContext(PCONTEXT Context);
LONG NTAPI handleException(EXCEPTION_POINTERS *ExceptionInfo);
LONG NTAPI reinstallUnhandledExceptionFilter(EXCEPTION_POINTERS *ExceptionInfo);
LONG callWithAltStack(EXCEPTION_POINTERS *arg,
                      LONG (*fn)(EXCEPTION_POINTERS *));
LONG reallyHandleException(EXCEPTION_POINTERS *ExceptionInfo);
VOID WINAPI hookRaiseFailFastException(PEXCEPTION_RECORD pExceptionRecord,
                                       PCONTEXT pContextRecord,
                                       DWORD dwFlags);
void hookFailFastInLoadedModules();
LONG NTAPI hookFailFastOnException(EXCEPTION_POINTERS *ExceptionInfo);

RaiseFailFastException_t pRealRaiseFailFastException = NULL;

}

namespace swift {
namespace runtime {
namespace backtrace {

SWIFT_RUNTIME_STDLIB_INTERNAL ErrorCode
_swift_installCrashHandler()
{
  // Allocate an alternate stack to use when handling exceptions
  const size_t altStackSize = 65536;
  void *altStackBase = VirtualAlloc(NULL, altStackSize,
                                    MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  altStackTop = (char *)altStackBase + altStackSize;

  // Install the exception handler
  AddVectoredExceptionHandler(0, reinstallUnhandledExceptionFilter);
  SetUnhandledExceptionFilter(handleException);

  if (_swift_backtraceSettings.hookFailFast) {
    pRealRaiseFailFastException
      = (RaiseFailFastException_t)GetProcAddress(GetModuleHandleW(L"kernelbase.dll"),
                                                 "RaiseFailFastException");
    if (pRealRaiseFailFastException) {
      hookFailFastInLoadedModules();
      AddVectoredExceptionHandler(0, hookFailFastOnException);
    }
  }

  return ERROR_SUCCESS;
}

} // namespace backtrace
} // namespace runtime
} // namespace swift

namespace {

LPTOP_LEVEL_EXCEPTION_FILTER pOldFilter = NULL;
bool reinstalled = false;

// Yuck.  This is horrible because the C runtime's startup code sets the
// unhandled exception filter, but that happens *after*
// _swift_installCrashHandler() has run because *that* is called during DLL
// initialization.
//
// Since we don't have a top-of-main() entry point for Swift, for now we
// hack things by reinstalling our filter from the vectored exception
// handler.  This is utterly horrible.
LONG reinstallUnhandledExceptionFilter(EXCEPTION_POINTERS *ExceptionInfo) {
  if (reinstalled)
    return EXCEPTION_CONTINUE_SEARCH;

  reinstalled = true;

  LPTOP_LEVEL_EXCEPTION_FILTER pFilter = SetUnhandledExceptionFilter(handleException);

  if (pFilter != handleException)
    pOldFilter = pFilter;
  return EXCEPTION_CONTINUE_SEARCH;
}

// Call a function, passing an argument, but using the alternate stack
LONG callWithAltStack(EXCEPTION_POINTERS *arg,
                      LONG (*fn)(EXCEPTION_POINTERS *)) {
#if defined(__x86_64__)
  register LONG result __asm__("rax");

  asm volatile (
    "    xchg  %%rsp, %3;      \
         subq  $16, %%rsp;     \
         movq  %3, (%%rsp);    \
         callq *%2;            \
         movq  (%%rsp), %%rsp; \
    "
    : "=a" (result)
    : "c" (arg), "d" (fn), "r" (altStackTop)
    : "memory"
  );

  return result;
#elif defined(__arm64__)
  register LONG result __asm__("x0");
  register LONG (*fn_reg)(EXCEPTION_POINTERS *) __asm__("x1");

  asm volatile (
    "    str sp, [%3, -16]!; \
         mov sp, %3;         \
         bl  %2;             \
         ldr sp, [sp];       \
    "
    : "=r" (result)
    : "0" (arg), "r" (fn_reg), "r" (altStackTop)
    : "memory"
  );

  return result;
#else
#warning You should fill this out with code for your architecture
  // Don't actually switch stack
  return fn(arg);
#endif
}

LONG handleException(EXCEPTION_POINTERS *ExceptionInfo) {
  LONG result;

  switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
  case DBG_PRINTEXCEPTION_C:
  case DBG_PRINTEXCEPTION_WIDE_C:
  case 0x406d1388:      // Set debugger thread name
    result = EXCEPTION_CONTINUE_EXECUTION;
    break;
  default:
    AcquireSRWLockExclusive(&crashLock);
    result = callWithAltStack(ExceptionInfo, reallyHandleException);
    ReleaseSRWLockExclusive(&crashLock);
    break;
  }

  if (pOldFilter && result == EXCEPTION_CONTINUE_SEARCH) {
    return pOldFilter(ExceptionInfo);
  }

  return result;
}

LONG reallyHandleException(EXCEPTION_POINTERS *ExceptionInfo) {
  HANDLE hOutput;
  if (_swift_backtraceSettings.outputTo == OutputTo::Stderr
      || _swift_backtraceSettings.outputTo == OutputTo::Console)
    hOutput = GetStdHandle(STD_ERROR_HANDLE);
  else
    hOutput = GetStdHandle(STD_OUTPUT_HANDLE);

  crashInfo.crashing_thread = GetCurrentThreadId();
  crashInfo.signal = ExceptionInfo->ExceptionRecord->ExceptionCode;

  // For access violations or in-page-errors, the fault address is the
  // address of the inaccessible data; for others, it's the instruction
  // address.  We do this to match the behaviour on other platforms.
  switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
  case EXCEPTION_ACCESS_VIOLATION:
  case EXCEPTION_IN_PAGE_ERROR:
    crashInfo.fault_address = ExceptionInfo->ExceptionRecord->ExceptionInformation[1];
    break;
  default:
    crashInfo.fault_address = (uint64_t)(ExceptionInfo->ExceptionRecord->ExceptionAddress);
    break;
  }

  crashInfo.exception_info = (uint64_t)ExceptionInfo;

  _swift_displayCrashMessage(ExceptionInfo->ExceptionRecord->ExceptionCode,
                             pcFromContext(ExceptionInfo->ContextRecord));

  // It isn't safe to try to stop all the threads here on Windows, so we
  // delegate doing that to the backtracer process.

  // Also, closing open file descriptors is
  // not something we can easily do on Windows.

  if (!_swift_spawnBacktracer(&crashInfo)) {
    const char *message = _swift_backtraceSettings.color == OnOffTty::On
      ? " failed\n\n" : " failed ***\n\n";
    WriteFile(hOutput, message, strlen(message), NULL, NULL);
  }

  return EXCEPTION_CONTINUE_SEARCH;
}

// Redirect any import address table entries for RaiseFailFastException()
// in the specified module to our hook.
void hookFailFastInModule(HMODULE hModule) {
  BYTE *base = (BYTE *)hModule;
  IMAGE_DOS_HEADER *dosHeader = (IMAGE_DOS_HEADER *)base;
  if (dosHeader->e_magic != IMAGE_DOS_SIGNATURE)
    return;

  IMAGE_NT_HEADERS *ntHeaders = (IMAGE_NT_HEADERS *)(base + dosHeader->e_lfanew);
  if (ntHeaders->Signature != IMAGE_NT_SIGNATURE)
    return;

  IMAGE_DATA_DIRECTORY *iatDirectory
    = &ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT];
  if (!iatDirectory->VirtualAddress || !iatDirectory->Size)
    return;

  void **iat = (void **)(base + iatDirectory->VirtualAddress);
  size_t count = iatDirectory->Size / sizeof(void *);
  for (size_t n = 0; n < count; ++n) {
    if (iat[n] != (void *)pRealRaiseFailFastException)
      continue;

    DWORD dwOldProtect;
    if (VirtualProtect(&iat[n], sizeof(void *), PAGE_READWRITE,
                       &dwOldProtect)) {
      InterlockedExchangePointer(&iat[n], (void *)hookRaiseFailFastException);
      VirtualProtect(&iat[n], sizeof(void *), dwOldProtect, &dwOldProtect);
    }
  }
}

SRWLOCK hookLock = SRWLOCK_INIT;
HMODULE loadedModules[1024];
HMODULE hookedModules[1024];
DWORD hookedModuleCount = 0;

void hookFailFastInLoadedModules() {
  AcquireSRWLockExclusive(&hookLock);

  DWORD cbNeeded;
  if (K32EnumProcessModules(GetCurrentProcess(), loadedModules,
                            sizeof(loadedModules), &cbNeeded)) {
    if (cbNeeded > sizeof(loadedModules))
      cbNeeded = sizeof(loadedModules);
    DWORD count = cbNeeded / sizeof(HMODULE);

    // Nothing to do if no modules have been loaded since we last did this
    if (count != hookedModuleCount
        || memcmp(loadedModules, hookedModules, cbNeeded) != 0) {
      for (DWORD n = 0; n < count; ++n)
        hookFailFastInModule(loadedModules[n]);

      memcpy(hookedModules, loadedModules, cbNeeded);
      hookedModuleCount = count;
    }
  }

  ReleaseSRWLockExclusive(&hookLock);
}

// Modules loaded after _swift_installCrashHandler() (e.g. WinUI's) won't
// have been hooked yet, so do that before any __except filter gets to see
// a potentially fatal exception.
LONG hookFailFastOnException(EXCEPTION_POINTERS *ExceptionInfo) {
  switch (ExceptionInfo->ExceptionRecord->ExceptionCode) {
  case EXCEPTION_ACCESS_VIOLATION:
  case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
  case EXCEPTION_BREAKPOINT:
  case EXCEPTION_DATATYPE_MISALIGNMENT:
  case EXCEPTION_ILLEGAL_INSTRUCTION:
  case EXCEPTION_IN_PAGE_ERROR:
  case EXCEPTION_INT_DIVIDE_BY_ZERO:
  case EXCEPTION_PRIV_INSTRUCTION:
    hookFailFastInLoadedModules();
    break;
  default:
    break;
  }

  return EXCEPTION_CONTINUE_SEARCH;
}

volatile LONG failFastThread = 0;

// RaiseFailFastException() never returns, so neither does this.
VOID WINAPI hookRaiseFailFastException(PEXCEPTION_RECORD pExceptionRecord,
                                       PCONTEXT pContextRecord,
                                       DWORD dwFlags) {
  LONG currentThread = (LONG)GetCurrentThreadId();
  LONG owner = InterlockedCompareExchange(&failFastThread, currentThread, 0);

  // If another thread is generating a backtrace, wait for it to terminate
  // the process; if this thread is, we failed while doing so.
  if (owner != 0 && owner != currentThread) {
    for (;;)
      Sleep(INFINITE);
  }

  if (owner == 0) {
    EXCEPTION_RECORD exceptionRecord;
    CONTEXT context;
    EXCEPTION_POINTERS exceptionPointers = { pExceptionRecord, pContextRecord };

    if (!pContextRecord) {
      RtlCaptureContext(&context);
      exceptionPointers.ContextRecord = &context;
    }
    if (!pExceptionRecord) {
      memset(&exceptionRecord, 0, sizeof(exceptionRecord));
      exceptionRecord.ExceptionCode = STATUS_FAIL_FAST_EXCEPTION;
      exceptionRecord.ExceptionFlags = EXCEPTION_NONCONTINUABLE;
      exceptionRecord.ExceptionAddress = __builtin_return_address(0);
      exceptionPointers.ExceptionRecord = &exceptionRecord;
    }

    AcquireSRWLockExclusive(&crashLock);
    callWithAltStack(&exceptionPointers, reallyHandleException);
    ReleaseSRWLockExclusive(&crashLock);
  }

  pRealRaiseFailFastException(pExceptionRecord, pContextRecord, dwFlags);
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
