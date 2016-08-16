//===--- MutexWin32.cpp - -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Mutex, ConditionVariable, Read/Write lock, and Scoped lock implementations
// using Windows Slim Reader/Writer Locks and Conditional Variables.
//
//===----------------------------------------------------------------------===//

#if defined(_WIN32) || defined(__CYGWIN__)
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/Debug.h"

using namespace swift;

void ConditionPlatformHelper::wait(CONDITION_VARIABLE &condition,
                                   SRWLOCK &mutex) {
  BOOL result = SleepConditionVariableSRW(&condition, &mutex, INFINITE, 0);
  if (!result) {
    DWORD errorcode = GetLastError();
    fatalError(/* flags = */ 0,
               "'SleepConditionVariableSRW()' failed with error code %d\n",
               errorcode);
  }
}
#endif
