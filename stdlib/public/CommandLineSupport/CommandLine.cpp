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

#include <algorithm>
#include <cassert>
#include <climits>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>

#include <errno.h>

#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Win32.h"

#include "swift/shims/GlobalObjects.h"
#include "swift/shims/RuntimeStubs.h"
#include "swift/shims/Visibility.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <shellapi.h>
#pragma comment(lib, "shell32.lib")
#endif

// Backing storage for overrides of `Swift.CommandLine.arguments`.
static char **_swift_stdlib_ProcessOverrideUnsafeArgv = nullptr;
static int _swift_stdlib_ProcessOverrideUnsafeArgc = 0;

// This needs to findable by dlopen() for JIT purposes (see Immediate.cpp).
SWIFT_CC(swift) extern "C" SWIFT_ATTRIBUTE_FOR_EXPORTS
void _swift_stdlib_overrideUnsafeArgvArgc(char **argv, int argc) {
  _swift_stdlib_ProcessOverrideUnsafeArgv = argv;
  _swift_stdlib_ProcessOverrideUnsafeArgc = argc;
}

namespace swift {
  /// A platform-specific implementation of @c _swift_stdlib_getUnsafeArgvArgc.
  /// 
  /// This function should return @c argc and @c argv cheaply (ideally in
  /// constant time and without needing to allocate.) If it cannot do so,
  /// it should return @c nullptr, at which point the caller can call
  /// @c enumerateUnsafeArgv() in order to reconstruct @c argv locally.
  /// 
  /// The result of this function is @em not owned by the caller and should
  /// persist for the lifetime of the process.
  static char **getUnsafeArgvArgc(int *outArgLen);

  /// A platform-specific function that enumerates the contents of @c argv
  /// one argument at a time.
  /// 
  /// @a body is a function that takes two arguments:
  /// 
  /// - The first argument is the value of @c argc if it can be readily
  ///   computed, or @c -1 otherwise.
  /// - The second argument is the element of @c argv being enumerated. The
  ///   caller makes a copy of this string. The implementation should not
  ///   enumerate the trailing @c nullptr required by the C standard.
  /// 
  /// Callers should call @c getUnsafeArgvArgc() before calling this function
  /// in case a fast path is available. If that function is implemented on this
  /// platform, then this function does not need to be implemented.
  template <typename F>
  static void enumerateUnsafeArgv(const F& body);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
char **_swift_stdlib_getUnsafeArgvArgc(int *outArgLen) {
  assert(outArgLen != nullptr);

  // Check the override before doing any platform-specific work.
  if (SWIFT_UNLIKELY(_swift_stdlib_ProcessOverrideUnsafeArgv)) {
    *outArgLen = _swift_stdlib_ProcessOverrideUnsafeArgc;
    return _swift_stdlib_ProcessOverrideUnsafeArgv;
  }

  // Try the platform-specific fast path that avoids heap (re)allocation. Not
  // all platforms implement this function.
  if (auto argv = swift::getUnsafeArgvArgc(outArgLen)) {
    return argv;
  }

  // Take a slower path wherein we construct the argv array one element at a
  // time. If the enumeration function can provide us with a hint for argc, we
  // can avoid calling realloc() more than once.
  int maxArgc = 0;
  int argc = 0;
  char **argv = nullptr;
  swift::enumerateUnsafeArgv([&] (int maxArgcHint, const char *arg) {
    if (argc >= maxArgc) {
      if (maxArgcHint > maxArgc) {
        // The platform was able to cheaply get argc, so use the
        // provided value.
        maxArgc = maxArgcHint;
      } else {
        // The platform could not tell us argc, so grow the argument array by
        // small amounts. We assume here that the command line doesn't usually
        // have many arguments, so a small/linear growth factor should be safe.
        //
        // If we ask for space for 15 arguments on the first allocation, then
        // when we call realloc (asking for additional space for the trailing
        // nullptr), we'll avoid wasting memory due to malloc()'s granularity.
        maxArgc = (maxArgc == 0) ? 15 : (maxArgc + 16);
      }

      // Reallocate (or initially allocate) the argv buffer. Overallocate by
      // one element to allow for a trailing nullptr.
      // 
      // NOTE: It is intentional that we do not simply use std::vector here.
      // STL collections may call operator new() which can be overridden by 
      // client code and that client code could call back into Swift.
      size_t argvSize = sizeof(char *) * (maxArgc + 1);
      argv = reinterpret_cast<char **>(realloc(argv, argvSize));
      if (!argv) {
        swift::fatalError(0,
          "Fatal error: Could not allocate space for %d commandline "
          " arguments: %d\n",
          argc, errno);
      }
    }

#if defined(_WIN32)
    argv[argc] = _strdup(arg);
#else
    argv[argc] = strdup(arg);
#endif
    argc += 1;
  });

  if (argv) {
    // Ensure the arguments array has a trailing nullptr, per the C standard.
    argv[argc] = nullptr;

  } else {
    // We didn't get any arguments and never ended up allocating an array.
    // Return an empty array (save for the trailing nullptr) instead.
    static char *emptyArgv[] = { nullptr };
    argc = 0;
    argv = emptyArgv;
  }

  *outArgLen = argc;
  return argv;
}

#if defined(__APPLE__)
// NOTE: forward declare this rather than including crt_externs.h as not all
// SDKs provide it
extern "C" char ***_NSGetArgv(void);
extern "C" int *_NSGetArgc(void);

static char **swift::getUnsafeArgvArgc(int *outArgLen) {
  *outArgLen = *_NSGetArgc();
  return *_NSGetArgv();
}

template <typename F>
static void swift::enumerateUnsafeArgv(const F& body) { }
#elif defined(__linux__)
// On Linux, there is no easy way to get the argument vector pointer outside
// of the main() function.  However, the ABI specifications dictate the layout
// of the process's initial stack, which looks something like:
//
// stack top ----> ┌────────────────────────┐
//                 │ Unspecified            │
//                 ┊                        ┊
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ Information block      │
//                 │ (argument strings,     │
//                 │ environment strings,   │
//                 │ auxiliary information) │
//                 ┊                        ┊
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ Unspecified            │
//                 ┊                        ┊
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ NULL                   │
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ Auxiliary Vector       │
//                 ┊                        ┊
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ NULL                   │
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ Environment Pointers   │
//                 ┊                        ┊
// environ ------> ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ NULL                   │
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ Argument Pointers      │
//                 ┊                        ┊
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 │ Argument Count         │
//                 ├┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┤
//                 ┊                        ┊
//
//                 See https://gitlab.com/x86-psABIs/x86-64-ABI,
//                     https://gitlab.com/x86-psABIs/i386-ABI
//
// The upshot is that if we can get hold of `environ` before anything has
// had a chance to change it, we can find the `argv` array and also the
// argument count, `argc`, by walking back up the stack.
//
// (Note that Linux uses this same layout for all platforms, not just x86-based
// ones.  It also has a fixed layout for the data at the top of the stack, but
// we don't need to take advantage of that here and can stick to things that
// are defined in the ABI specs.)

#include <unistd.h>

#define DEBUG_ARGVGRABBER 0
#if DEBUG_ARGVGRABBER
#define ARGVDEBUG(...) fprintf(stderr, __VA_ARGS__)
#else
#define ARGVDEBUG(...)
#endif

namespace {

struct ArgvGrabber {
  char **argv;
  int argc;

  ArgvGrabber();

private:
  struct stack {
    void *base;
    void *top;

    stack() : base(nullptr), top(nullptr) {}
    stack(void *b, void *t) : base(b), top(t) {}
  };

  stack findStack();
  void findArgv(stack s);
};

// Find the stack by looking at /proc/self/maps
ArgvGrabber::stack ArgvGrabber::findStack(void) {
  FILE *maps = fopen("/proc/self/maps", "r");
  if (!maps) {
    ARGVDEBUG("unable to open maps - %d\n", errno);
    return stack();
  }

  char line[256];
  void *base = NULL, *top = NULL;
  bool found = false;
  while (fgets(line, sizeof(line), maps)) {
    // line is on the stack, so we know we're looking at the right
    // region if line is between base and top.
    //
    // Note that we can't look for [stack], because Rosetta and qemu
    // set up a separate stack for the emulated code.
    //
    // We also need to glom on extra VM ranges after the first one
    // we find, because *sometimes* we end up with an extra range.
    void *lo, *hi;
    if (sscanf(line, "%p-%p", &lo, &hi) == 2) {
      if ((void *)line >= lo && (void *)line < hi) {
        base = lo;
        top = hi;
        found = true;
      } else if (found && top == lo) {
        top = hi;
      }
    }
  }

  fclose(maps);

  if (!found) {
    ARGVDEBUG("stack not found in maps\n");
    return stack();
  }

  return stack(base, top);
}

#if DEBUG_ARGVGRABBER
void printMaps() {
  FILE *maps = fopen("/proc/self/maps", "r");
  if (!maps) {
    fprintf(stderr, "unable to open maps - %d\n", errno);
    return;
  }

  char line[256];
  while (fgets(line, sizeof(line), maps)) {
    fputs(line, stderr);
  }

  fclose(maps);
}
#endif

// Find argv by walking backwards from environ
void ArgvGrabber::findArgv(ArgvGrabber::stack stack) {
  if (!stack.base) {
    ARGVDEBUG("no stack\n");
    return;
  }

  // Check that environ points to the stack
  char **envp = environ;
  if ((void *)envp < stack.base || (void *)envp >= stack.top) {
    ARGVDEBUG("envp = %p, stack is from %p to %p\n",
              envp, stack.base, stack.top);
#if DEBUG_ARGVGRABBER
    printMaps();
#endif
    return;
  }

  char **ptr = envp - 1;

  // We're now pointing at the NULL that terminates argv.  Keep going back
  // while we're seeing pointers (values greater than envp).
  while ((void *)(ptr - 1) > stack.base) {
    --ptr;

    // The first thing less than envp must be the argc value
    if ((void *)*ptr < (void *)envp) {
      argc = (int)(intptr_t)*ptr++;
      argv = ptr;
      return;
    }
  }

  ARGVDEBUG("didn't find argc\n");
}

ArgvGrabber::ArgvGrabber() : argv(nullptr), argc(0) {
  ARGVDEBUG("***GRABBING ARGV for %d***\n", getpid());
  findArgv(findStack());
#if DEBUG_ARGVGRABBER
  fprintf(stderr, "ARGV is at %p with count %d\n", argv, argc);
  for (int i = 0; i < argc; ++i) {
    fprintf(stderr, "  argv[%d] = \"%s\"\n", i, argv[i]);
  }
  fprintf(stderr, "***ARGV GRABBED***\n");
#endif
}

ArgvGrabber argvGrabber;

} // namespace

static char **swift::getUnsafeArgvArgc(int *outArgLen) {
  *outArgLen = argvGrabber.argc;
  return argvGrabber.argv;
}

template <typename F>
static void swift::enumerateUnsafeArgv(const F& body) { }
#elif defined(__CYGWIN__)
static char **swift::getUnsafeArgvArgc(int *outArgLen) {
  return nullptr;
}

template <typename F>
static void swift::enumerateUnsafeArgv(const F& body) {
  FILE *cmdline = fopen("/proc/self/cmdline", "rb");
  if (!cmdline) {
    swift::fatalError(0,
      "Fatal error: Unable to open interface to '/proc/self/cmdline': %d.\n",
      errno);
  }

  char *arg = nullptr;
  size_t size = 0;
  while (getdelim(&arg, &size, '\0', cmdline) != -1) {
    body(-1, arg);
  }
  if (arg) {
    free(arg);
  }

  fclose(cmdline);
}
#elif defined(_WIN32)
#include <stdlib.h>

static char **swift::getUnsafeArgvArgc(int *outArgLen) {
  return nullptr;
}

template <typename F>
static void swift::enumerateUnsafeArgv(const F& body) {
  int argc = 0;
  if (LPWSTR *wargv = CommandLineToArgvW(GetCommandLineW(), &argc)) {
    std::for_each(wargv, wargv + argc, [=] (wchar_t *warg) {
      if (char *arg = _swift_win32_copyUTF8FromWide(warg)) {
        body(argc, arg);
        free(arg);
      } else {
        // Note that GetLastError() and errno may not be so useful here,
        // as in the error case we may have called free(), which might reset
        // either or both of them.
        swift::fatalError(0,
                          "Fatal error: Unable to convert argument '%ls' to "
                          "UTF-8: %lx, %d.\n",
                          warg, ::GetLastError(), errno);
      }
    });

    LocalFree(wargv);
  }
}
#elif defined(__FreeBSD__)
#include <errno.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <unistd.h>

static char **swift::getUnsafeArgvArgc(int *outArgLen) {
  return nullptr;
}

template <typename F>
static void swift::enumerateUnsafeArgv(const F& body) {
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

  for (; curPtr < endPtr; curPtr += strlen(curPtr) + 1) {
    body(-1, curPtr);
  }
  free(argPtr);
}
#elif defined(__wasi__)
#include <stdlib.h>
#include <wasi/api.h>
#include <wasi/libc.h>

static char **swift::getUnsafeArgvArgc(int *outArgLen) {
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

template <typename F>
static void swift::enumerateUnsafeArgv(const F& body) { }
#elif defined(__OpenBSD__)
#include <sys/types.h>
#include <sys/sysctl.h>
#include <sys/exec.h>

static char **swift::getUnsafeArgvArgc(int *outArgLen) {
  int mib[2] = {CTL_VM, VM_PSSTRINGS};
  struct _ps_strings _ps;
  size_t len = sizeof(_ps);

  if (sysctl(mib, 2, &_ps, &len, NULL, 0) == -1) {
    return nullptr;
  }

  struct ps_strings *ps = static_cast<struct ps_strings *>(_ps.val);
  *outArgLen = ps->ps_nargvstr;
  return ps->ps_argvstr;
}

template <typename F>
static void swift::enumerateUnsafeArgv(const F& body) { }
#else // Add your favorite OS's command line arg grabber here.
static char **swift::getUnsafeArgvArgc(int *outArgLen) {
  swift::fatalError(
      0,
      "Fatal error: Command line arguments not supported on this platform.\n");
}

template <typename F>
static void swift::enumerateUnsafeArgv(const F& body) { }
#endif
