//===--- CommandLine.cpp - OS-specific command line arguments -------------===//
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
// OS-specific command line argument handling is defined here.
//
//===----------------------------------------------------------------------===//

#include <vector>
#include <string>
#include <cassert>
#include <climits>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "swift/Runtime/Debug.h"

#include "../SwiftShims/RuntimeStubs.h"
#include "../SwiftShims/GlobalObjects.h"

// Backing storage for overrides of `Swift.CommandLine.arguments`.
static char **_swift_stdlib_ProcessOverrideUnsafeArgv = nullptr;
static int _swift_stdlib_ProcessOverrideUnsafeArgc = 0;

SWIFT_RUNTIME_STDLIB_API
void _swift_stdlib_overrideUnsafeArgvArgc(char **argv, int argc) {
  _swift_stdlib_ProcessOverrideUnsafeArgv = argv;
  _swift_stdlib_ProcessOverrideUnsafeArgc = argc;
}

#if defined(__APPLE__)
// NOTE: forward declare this rather than including crt_externs.h as not all
// SDKs provide it
extern "C" char ***_NSGetArgv(void);
extern "C" int *_NSGetArgc(void);

SWIFT_RUNTIME_STDLIB_API
char ** _swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  *outArgLen = *_NSGetArgc();
  return *_NSGetArgv();
}
#elif defined(__linux__) || defined(__CYGWIN__)
SWIFT_RUNTIME_STDLIB_API
char ** _swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  FILE *cmdline = fopen("/proc/self/cmdline", "rb");
  if (!cmdline) {
    swift::fatalError(0,
            "Fatal error: Unable to open interface to '/proc/self/cmdline'.\n");
  }
  char *arg = nullptr;
  size_t size = 0;
  std::vector<char *> argvec;
  while (getdelim(&arg, &size, 0, cmdline) != -1) {
    argvec.push_back(strdup(arg));
  }
  if (arg) {
    free(arg);
  }
  fclose(cmdline);
  *outArgLen = argvec.size();
  char **outBuf = (char **)calloc(argvec.size() + 1, sizeof(char *));
  std::copy(argvec.begin(), argvec.end(), outBuf);
  outBuf[argvec.size()] = nullptr;

  return outBuf;
}
#elif defined (_WIN32)
#include <stdlib.h>

SWIFT_RUNTIME_STDLIB_API
char ** _swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  *outArgLen = __argc;
  return __argv;
}
#elif defined(__FreeBSD__)
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/sysctl.h>

SWIFT_RUNTIME_STDLIB_API
char ** _swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  char *argPtr = nullptr; // or use ARG_MAX? 8192 is used in LLDB though..
  int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_ARGS, getpid() };
  size_t argPtrSize = 0;
  for (int i = 0; i < 3 && !argPtr; i++) { // give up after 3 tries
    if (sysctl(mib, 4, nullptr, &argPtrSize, nullptr, 0) != -1) {
      argPtr = (char *)malloc(argPtrSize);
      if (sysctl(mib, 4, argPtr, &argPtrSize, nullptr, 0) == -1) {
        free(argPtr);
        argPtr = nullptr;
        if (errno != ENOMEM)
          break;
      }
    } else {
      break;
    }
  }
  if (!argPtr)
    swift::fatalError(0, "Fatal error: Could not retrieve commandline "
                         "arguments: sysctl: %s.\n", strerror(errno));

  char *curPtr = argPtr;
  char *endPtr = argPtr + argPtrSize;

  std::vector<char *> argvec;
  for (; curPtr < endPtr; curPtr += strlen(curPtr) + 1)
    argvec.push_back(strdup(curPtr));
  *outArgLen = argvec.size();
  char **outBuf = (char **)calloc(argvec.size() + 1, sizeof(char *));
  std::copy(argvec.begin(), argvec.end(), outBuf);
  outBuf[argvec.size()] = nullptr;

  free(argPtr);
    
  return outBuf;
}
#else // __ANDROID__; Add your favorite arch's command line arg grabber here.
SWIFT_RUNTIME_STDLIB_API
char ** _swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }
  
  swift::fatalError(0,
      "Fatal error: Command line arguments not supported on this platform.\n");
}
#endif

