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

#include <cassert>
#include <climits>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

#include "swift/Runtime/Debug.h"

#include "../SwiftShims/GlobalObjects.h"
#include "../SwiftShims/RuntimeStubs.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <shellapi.h>
#endif

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
char **_swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
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
char **_swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  FILE *cmdline = fopen("/proc/self/cmdline", "rb");
  if (!cmdline) {
    swift::fatalError(
        0, "Fatal error: Unable to open interface to '/proc/self/cmdline'.\n");
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
  auto outBuf = static_cast<char **>(calloc(argvec.size() + 1, sizeof(char *)));
  std::copy(argvec.begin(), argvec.end(), outBuf);
  outBuf[argvec.size()] = nullptr;

  return outBuf;
}
#elif defined(_WIN32)
#include <stdlib.h>

SWIFT_RUNTIME_STDLIB_API
char **_swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  *outArgLen = 0;

  LPWSTR *szArgList;
  int nArgs;
  szArgList = CommandLineToArgvW(GetCommandLineW(), &nArgs);
  if (szArgList == nullptr)
    return nullptr;

  std::vector<char *> argv;
  for (int i = 0; i < nArgs; ++i) {
    int szBufferSize =
        WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, szArgList[i], -1,
                            nullptr, 0, nullptr, nullptr);
    if (szBufferSize == 0) {
      swift::fatalError(0,
                        "Fatal error: Could not retrieve commandline "
                        "arguments: %u\n",
                        GetLastError());
      return nullptr;
    }

    char *buffer = static_cast<char *>(
        calloc(static_cast<size_t>(szBufferSize), sizeof(char)));
    if (buffer == nullptr) {
      swift::fatalError(0,
                        "Fatal error: Could not allocate space for commandline"
                        "arguments");
      return nullptr;
    }

    if (!WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, szArgList[i], -1,
                             buffer, szBufferSize, nullptr, nullptr)) {
      swift::fatalError(0,
                        "Fatal error: Conversion to UTF-8 failed for "
                        "commandline arguments");
      return nullptr;
    }

    argv.push_back(buffer);
  }

  LocalFree(szArgList);

  char **args = static_cast<char **>(calloc(argv.size() + 1, sizeof(char *)));
  std::copy(argv.begin(), argv.end(), args);
  args[argv.size()] = nullptr;

  assert(argv.size() < INT_MAX && "overflow");
  *outArgLen = static_cast<int>(argv.size());
  return args;
}
#elif defined(__FreeBSD__)
#include <errno.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <unistd.h>

SWIFT_RUNTIME_STDLIB_API
char **_swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  char *argPtr = nullptr; // or use ARG_MAX? 8192 is used in LLDB though..
  int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_ARGS, getpid()};
  size_t argPtrSize = 0;
  for (int i = 0; i < 3 && !argPtr; ++i) { // give up after 3 tries
    if (sysctl(mib, 4, nullptr, &argPtrSize, nullptr, 0) != -1) {
      argPtr = static_cast<char *>(malloc(argPtrSize));
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
    swift::fatalError(0,
                      "Fatal error: Could not retrieve commandline "
                      "arguments: sysctl: %s.\n",
                      strerror(errno));

  char *curPtr = argPtr;
  char *endPtr = argPtr + argPtrSize;

  std::vector<char *> argvec;
  for (; curPtr < endPtr; curPtr += strlen(curPtr) + 1)
    argvec.push_back(strdup(curPtr));
  *outArgLen = argvec.size();
  auto outBuf = static_cast<char **>(calloc(argvec.size() + 1, sizeof(char *)));
  std::copy(argvec.begin(), argvec.end(), outBuf);
  outBuf[argvec.size()] = nullptr;

  free(argPtr);

  return outBuf;
}
#elif defined(__wasi__)
#include <stdlib.h>
#include <wasi/api.h>
#include <wasi/libc.h>

SWIFT_RUNTIME_STDLIB_API
char **_swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  __wasi_errno_t err;

  size_t argv_buf_size = 0;
  size_t argc = 0;
  err = __wasi_args_sizes_get(&argc, &argv_buf_size);
  if (err != __WASI_ERRNO_SUCCESS) {
    swift::fatalError(0,
                      "Fatal error: Could not retrieve commandline "
                      "arguments: %d.\n", static_cast<int>(err));
    return nullptr;
  }

  char *argv_buf = static_cast<char *>(malloc(argv_buf_size));
  char **argv = static_cast<char **>(calloc(argc + 1, sizeof(char *)));

  err = __wasi_args_get((uint8_t **)argv, (uint8_t *)argv_buf);
  if (err != __WASI_ERRNO_SUCCESS) {
    swift::fatalError(0,
                      "Fatal error: Could not retrieve commandline "
                      "arguments: %d.\n", static_cast<int>(err));
    return nullptr;
  }

  *outArgLen = static_cast<int>(argc);

  return argv;
}
#elif defined(__OpenBSD__)
#include <sys/types.h>
#include <sys/sysctl.h>
#include <sys/exec.h>

SWIFT_RUNTIME_STDLIB_API
char ** _swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);
  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  int mib[2] = {CTL_VM, VM_PSSTRINGS};
  struct _ps_strings _ps;
  size_t len = sizeof(_ps);

  if (sysctl(mib, 2, &_ps, &len, NULL, 0) == -1) {
    char **empty_argv = static_cast<char **>(calloc(1, sizeof(char *)));
    empty_argv[0] = nullptr;
    *outArgLen = 0;
    return empty_argv;
  }

  struct ps_strings *ps = static_cast<struct ps_strings *>(_ps.val);
  *outArgLen = ps->ps_nargvstr;

  char **argv_copy =
      static_cast<char **>(calloc(ps->ps_nargvstr + 1, sizeof(char *)));
  for(int i = 0; i < ps->ps_nargvstr; i++) {
    argv_copy[i] = strdup(ps->ps_argvstr[i]);
  }
  argv_copy[ps->ps_nargvstr] = nullptr;

  return argv_copy;
}
#else // Add your favorite OS's command line arg grabber here.
SWIFT_RUNTIME_STDLIB_API
char **_swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  if (_swift_stdlib_ProcessOverrideUnsafeArgv) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  swift::fatalError(
      0,
      "Fatal error: Command line arguments not supported on this platform.\n");
}
#endif
