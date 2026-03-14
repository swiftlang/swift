//===--- ImageInspectionWin32.cpp - Win32 image inspection ----------------===//
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

#if !defined(__ELF__) && !defined(__MACH__)

#include "ImageInspection.h"

#if defined(__CYGWIN__)
#include <dlfcn.h>
#elif defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#include <DbgHelp.h>

#pragma comment(lib, "DbgHelp.Lib")
#endif

#include "swift/Runtime/Win32.h"
#include "swift/Threading/Mutex.h"

using namespace swift;

#if defined(_WIN32)
static LazyMutex mutex;
static HANDLE dbgHelpHandle = nullptr;
static bool isDbgHelpInitialized = false;

void _swift_win32_withDbgHelpLibrary(
  void (* body)(HANDLE hProcess, void *context), void *context) {
  mutex.withLock([=] () {
    // If we have not previously created a handle to use with the library, do so
    // now. This handle belongs to the Swift runtime and should not be closed by
    // `body` (or anybody else.)
    if (!dbgHelpHandle) {
      // Per the documentation for the Debug Help library, we should not use the
      // current process handle because other subsystems might also use it and
      // end up stomping on each other. So we'll try to duplicate that handle to
      // get a unique one that still fulfills the needs of the library. If that
      // fails (presumably because the current process doesn't have the
      // PROCESS_DUP_HANDLE access right) then fall back to using the original
      // process handle and hope nobody else is using it too.
      HANDLE currentProcess = GetCurrentProcess();
      if (!DuplicateHandle(currentProcess, currentProcess, currentProcess,
                           &dbgHelpHandle, 0, false, DUPLICATE_SAME_ACCESS)) {
        dbgHelpHandle = currentProcess;
      }
    }

    // If we have not previously initialized the Debug Help library, do so now.
    if (dbgHelpHandle && !isDbgHelpInitialized) {
      isDbgHelpInitialized = SymInitialize(dbgHelpHandle, nullptr, true);
    }

    if (isDbgHelpInitialized) {
      // Set the library's options to what the Swift runtime generally expects.
      // If the options aren't going to change, we can skip the call and save a
      // few CPU cycles on the library call.
      constexpr const DWORD options = SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS;
      DWORD oldOptions = SymGetOptions();
      if (oldOptions != options) {
        SymSetOptions(options);
      }

      body(dbgHelpHandle, context);

      // Before returning, reset the library's options back to their previous
      // value. No need to call if the options didn't change because LazyMutex
      // is not recursive, so there shouldn't be an outer call expecting the
      // original options, and a subsequent call to this function will set them
      // to the defaults above.
      if (oldOptions != options) {
        SymSetOptions(oldOptions);
      }
    } else {
      body(nullptr, context);
    }
  });
}
#endif

#endif // !defined(__ELF__) && !defined(__MACH__)
