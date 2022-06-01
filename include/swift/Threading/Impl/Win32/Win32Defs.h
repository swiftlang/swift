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

typedef VOID(NTAPI *PFLS_CALLBACK_FUNCTION)(PVOID lpFlsData);

typedef struct _RTL_SRWLOCK *PRTL_SRWLOCK;
typedef PRTL_SRWLOCK PSRWLOCK;

// These have to be #defines, to avoid problems with <windows.h>
#define RTL_SRWLOCK_INIT                                                       \
  { 0 }
#define SRWLOCK_INIT RTL_SRWLOCK_INIT
#define FLS_OUT_OF_INDEXES ((DWORD)0xFFFFFFFF)

extern "C" {
WINBASEAPI DWORD WINAPI GetCurrentThreadId(VOID);

WINBASEAPI VOID WINAPI InitializeSRWLock(PSRWLOCK SRWLock);
WINBASEAPI VOID WINAPI ReleaseSRWLockExclusive(PSRWLOCK SRWLock);
WINBASEAPI VOID WINAPI AcquireSRWLockExclusive(PSRWLOCK SRWLock);
WINBASEAPI BOOLEAN WINAPI TryAcquireSRWLockExclusive(PSRWLOCK SRWLock);

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

} // namespace threading_impl
} // namespace swift

#endif // SWIFT_THREADING_IMPL_WIN32_DEFS_H
