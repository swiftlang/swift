//==--- Win32Defs.h - Windows API definitions ------------------ -*-C++ -*-===//
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
// We cannot include <windows.h> from the Threading headers because they get
// included all over the place and <windows.h> defines a large number of
// obnoxious macros.  Instead, this header declares *just* what we need.
//
// If you need <windows.h> in a file, please make sure to include it *before*
// this file, or you'll get errors about RTL_SRWLOCK.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_WIN32_DEFS_H
#define SWIFT_THREADING_IMPL_WIN32_DEFS_H

#define DECLSPEC_IMPORT __declspec(dllimport)
#define WINBASEAPI DECLSPEC_IMPORT
#define WINAPI __stdcall
#define NTAPI __stdcall

// <windows.h> #defines VOID rather than typedefing it(!)  Changing that
// to use a typedef instead isn't problematic later on, so let's do that.
#undef VOID

typedef void VOID, *PVOID;
typedef unsigned char BYTE;
typedef BYTE BOOLEAN;
typedef int BOOL;
typedef unsigned long DWORD;
typedef long LONG;
typedef unsigned long ULONG;
#if defined(_WIN64)
typedef unsigned __int64 ULONG_PTR;
#else
typedef unsigned long ULONG_PTR;
#endif
typedef PVOID HANDLE;

typedef VOID(NTAPI *PFLS_CALLBACK_FUNCTION)(PVOID lpFlsData);

typedef struct _RTL_SRWLOCK *PRTL_SRWLOCK;
typedef PRTL_SRWLOCK PSRWLOCK;

typedef struct _RTL_CONDITION_VARIABLE *PRTL_CONDITION_VARIABLE;
typedef PRTL_CONDITION_VARIABLE PCONDITION_VARIABLE;

typedef struct _RTL_CRITICAL_SECTION_DEBUG *PRTL_CRITICAL_SECTION_DEBUG;
typedef struct _RTL_CRITICAL_SECTION *PRTL_CRITICAL_SECTION;
typedef PRTL_CRITICAL_SECTION PCRITICAL_SECTION;
typedef PRTL_CRITICAL_SECTION LPCRITICAL_SECTION;

// These have to be #defines, to avoid problems with <windows.h>
#define RTL_SRWLOCK_INIT {0}
#define SRWLOCK_INIT RTL_SRWLOCK_INIT
#define FLS_OUT_OF_INDEXES ((DWORD)0xFFFFFFFF)

#define RTL_CONDITION_VARIABLE_INIT {0}
#define CONDITION_VARIABLE_INIT RTL_CONDITION_VARIABLE_INIT

#define RTL_CONDITION_VARIABLE_LOCKMODE_SHARED 0x1
#define CONDITION_VARIABLE_LOCKMODE_SHARED RTL_CONDITION_VARIABLE_LOCKMODE_SHARED

#define INFINITE 0xFFFFFFFF // Infinite timeout

extern "C" {
WINBASEAPI DWORD WINAPI GetCurrentThreadId(VOID);

WINBASEAPI VOID WINAPI InitializeSRWLock(PSRWLOCK SRWLock);
WINBASEAPI VOID WINAPI ReleaseSRWLockExclusive(PSRWLOCK SRWLock);
WINBASEAPI VOID WINAPI AcquireSRWLockExclusive(PSRWLOCK SRWLock);
WINBASEAPI BOOLEAN WINAPI TryAcquireSRWLockExclusive(PSRWLOCK SRWLock);

WINBASEAPI VOID WINAPI InitializeConditionVariable(
  PCONDITION_VARIABLE ConditionVariable
);
WINBASEAPI VOID WINAPI WakeConditionVariable(
  PCONDITION_VARIABLE ConditionVariable
);
WINBASEAPI VOID WINAPI WakeAllConditionVariable(
  PCONDITION_VARIABLE ConditionVariable
);
WINBASEAPI BOOL WINAPI SleepConditionVariableSRW(
  PCONDITION_VARIABLE ConditionVariable,
  PSRWLOCK SRWLock,
  DWORD dwMilliseconds,
  ULONG Flags
);

WINBASEAPI VOID WINAPI InitializeCriticalSection(
  LPCRITICAL_SECTION lpCriticalSection
);
WINBASEAPI VOID WINAPI DeleteCriticalSection(
  LPCRITICAL_SECTION lpCriticalSection
);
WINBASEAPI VOID WINAPI EnterCriticalSection(
  LPCRITICAL_SECTION lpCriticalSection
);
WINBASEAPI VOID WINAPI LeaveCriticalSection(
  LPCRITICAL_SECTION lpCriticalSection
);

WINBASEAPI DWORD WINAPI FlsAlloc(PFLS_CALLBACK_FUNCTION lpCallback);
WINBASEAPI PVOID WINAPI FlsGetValue(DWORD dwFlsIndex);
WINBASEAPI BOOL WINAPI FlsSetValue(DWORD dwFlsIndex, PVOID lpFlsData);
WINBASEAPI BOOL WINAPI FlsFree(DWORD dwFlsIndex);
}

namespace swift {
namespace threading_impl {

// We do this because we can't declare _RTL_SRWLOCK here in case someone
// later includes <windows.h>
struct SWIFT_SRWLOCK {
  PVOID Ptr;
};

typedef SWIFT_SRWLOCK *PSWIFT_SRWLOCK;

inline VOID InitializeSRWLock(PSWIFT_SRWLOCK SRWLock) {
  ::InitializeSRWLock(reinterpret_cast<PSRWLOCK>(SRWLock));
}
inline VOID ReleaseSRWLockExclusive(PSWIFT_SRWLOCK SRWLock) {
  ::ReleaseSRWLockExclusive(reinterpret_cast<PSRWLOCK>(SRWLock));
}
inline VOID AcquireSRWLockExclusive(PSWIFT_SRWLOCK SRWLock) {
  ::AcquireSRWLockExclusive(reinterpret_cast<PSRWLOCK>(SRWLock));
}
inline BOOLEAN TryAcquireSRWLockExclusive(PSWIFT_SRWLOCK SRWLock) {
  return ::TryAcquireSRWLockExclusive(reinterpret_cast<PSRWLOCK>(SRWLock));
}

// Similarly we have the same problem with _RTL_CONDITION_VARIABLE
struct SWIFT_CONDITION_VARIABLE {
  PVOID Ptr;
};

typedef SWIFT_CONDITION_VARIABLE *PSWIFT_CONDITION_VARIABLE;

inline VOID InitializeConditionVariable(PSWIFT_CONDITION_VARIABLE CondVar) {
  ::InitializeConditionVariable(reinterpret_cast<PCONDITION_VARIABLE>(CondVar));
}
inline VOID WakeConditionVariable(PSWIFT_CONDITION_VARIABLE CondVar) {
  ::WakeConditionVariable(reinterpret_cast<PCONDITION_VARIABLE>(CondVar));
}
inline VOID WakeAllConditionVariable(PSWIFT_CONDITION_VARIABLE CondVar) {
  ::WakeAllConditionVariable(reinterpret_cast<PCONDITION_VARIABLE>(CondVar));
}
inline BOOL SleepConditionVariableSRW(PSWIFT_CONDITION_VARIABLE CondVar,
                                      PSWIFT_SRWLOCK SRWLock,
                                      DWORD dwMilliseconds,
                                      ULONG Flags) {
  return ::SleepConditionVariableSRW(
    reinterpret_cast<PCONDITION_VARIABLE>(CondVar),
    reinterpret_cast<PSRWLOCK>(SRWLock),
    dwMilliseconds,
    Flags);
}

// And with CRITICAL_SECTION
#pragma pack(push, 8)
typedef struct SWIFT_CRITICAL_SECTION {
  PRTL_CRITICAL_SECTION_DEBUG DebugInfo;
  LONG LockCount;
  LONG RecursionCount;
  HANDLE OwningThread;
  HANDLE LockSemaphore;
  ULONG_PTR SpinCount;
} SWIFT_CRITICAL_SECTION, *PSWIFT_CRITICAL_SECTION;
#pragma pack(pop)

inline VOID InitializeCriticalSection(PSWIFT_CRITICAL_SECTION CritSec) {
  ::InitializeCriticalSection(reinterpret_cast<LPCRITICAL_SECTION>(CritSec));
}

inline VOID DeleteCriticalSection(PSWIFT_CRITICAL_SECTION CritSec) {
  ::DeleteCriticalSection(reinterpret_cast<LPCRITICAL_SECTION>(CritSec));
}

inline VOID EnterCriticalSection(PSWIFT_CRITICAL_SECTION CritSec) {
  ::EnterCriticalSection(reinterpret_cast<LPCRITICAL_SECTION>(CritSec));
}

inline VOID LeaveCriticalSection(PSWIFT_CRITICAL_SECTION CritSec) {
  ::LeaveCriticalSection(reinterpret_cast<LPCRITICAL_SECTION>(CritSec));
}

} // namespace threading_impl
} // namespace swift

#endif // SWIFT_THREADING_IMPL_WIN32_DEFS_H
