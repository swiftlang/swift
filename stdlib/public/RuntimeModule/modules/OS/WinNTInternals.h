//===--- WinNTInternals.h - Windows specifics -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Definitions of some NT internal functions that we make use of.
//
//  This is technically naughty, but it is more robust than using the
//  DebugActiveProcess() API, which sometimes appears to fail to work.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BACKTRACING_WINNT_INTERNALS_H
#define SWIFT_BACKTRACING_WINNT_INTERNALS_H

#ifdef _WIN32

#include <windows.h>
#include <winternl.h>

#ifndef STATUS_INFO_LENGTH_MISMATCH
#define STATUS_INFO_LENGTH_MISMATCH ((DWORD)0xC0000004L)
#endif

typedef __kernel_entry NTSTATUS (NTAPI *PNT_QUERY_SYSTEM_INFORMATION)(
  SYSTEM_INFORMATION_CLASS,
  PVOID,
  ULONG,
  PULONG
);

typedef __kernel_entry NTSTATUS (NTAPI *PNT_SUSPEND_PROCESS)(
  HANDLE
);

typedef __kernel_entry NTSTATUS (NTAPI *PNT_RESUME_PROCESS)(
  HANDLE
);

#endif

#endif // SWIFT_BACKTRACING_WINNT_INTERNALS_H
