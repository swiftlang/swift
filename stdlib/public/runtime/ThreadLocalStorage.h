//===--- ThreadLocalStorage.h - Thread-local storage interface. --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_THREADLOCALSTORAGE_H
#define SWIFT_RUNTIME_THREADLOCALSTORAGE_H

#include "llvm/Support/Compiler.h"
#include "swift/Runtime/Config.h"

// Depending on the target, we may be able to use dedicated TSD keys or
// thread_local variables. When dedicated TSD keys aren't available,
// wrap the target's API for thread-local data for things that don't want
// to use thread_local.

// On Apple platforms, we have dedicated TSD keys.
#if defined(__APPLE__)
# define SWIFT_TLS_HAS_RESERVED_PTHREAD_SPECIFIC 1
#endif

// If we're using Clang, and Clang claims not to support thread_local,
// it must be because we're on a platform that doesn't support it.
#if __clang__ && !__has_feature(cxx_thread_local)
// No thread_local.
#else
// Otherwise, we do have thread_local.
# define SWIFT_TLS_HAS_THREADLOCAL 1
static_assert(LLVM_ENABLE_THREADS, "LLVM_THREAD_LOCAL will use a global?");
#endif

#if SWIFT_TLS_HAS_RESERVED_PTHREAD_SPECIFIC
// Use reserved TSD keys.
# if __has_include(<pthread/tsd_private.h>)
#  include <pthread/tsd_private.h>
# else
// We still need to use the SPI for setting the destructor, so declare it here.
extern "C" int pthread_key_init_np(int key, void (*destructor)(void *));
# endif

// If the keys are not available from the header, define them ourselves. The values match
// what tsd_private.h provides.
# ifndef __PTK_FRAMEWORK_SWIFT_KEY0
#  define __PTK_FRAMEWORK_SWIFT_KEY0 100
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY1
#  define __PTK_FRAMEWORK_SWIFT_KEY1 101
# endif

# define SWIFT_EXCLUSIVITY_TLS_KEY __PTK_FRAMEWORK_SWIFT_KEY0
# define SWIFT_STDLIB_TLS_KEY __PTK_FRAMEWORK_SWIFT_KEY1

#endif

// If the reserved key path didn't already provide get/setspecific macros,
// wrap the platform's APIs.
#ifndef SWIFT_THREAD_GETSPECIFIC

// Pick the right typedef for the key.
# if defined(__linux__)
#  if defined(__ANDROID__)
typedef int __swift_thread_key_t;
#  else
typedef unsigned int __swift_thread_key_t;
#  endif
# elif defined(__FreeBSD__)
typedef int __swift_thread_key_t;
# elif defined(_WIN32)
typedef unsigned long __swift_thread_key_t;
# elif defined(__HAIKU__)
typedef int __swift_thread_key_t;
# else
typedef unsigned long __swift_thread_key_t;
# endif

# if defined(_WIN32) && !defined(__CYGWIN__)
// Windows has its own flavor of API.
#  include <io.h>
#  define WIN32_LEAN_AND_MEAN
#  include <Windows.h>
static_assert(std::is_same<__swift_thread_key_t, DWORD>::value,
              "__swift_thread_key_t is not a DWORD");

#  define SWIFT_THREAD_KEY_CREATE _stdlib_thread_key_create
#  define SWIFT_THREAD_GETSPECIFIC FlsGetValue
#  define SWIFT_THREAD_SETSPECIFIC(key, value) (FlsSetValue(key, value) == TRUE)

# else
// Otherwise use the pthread API.
#  include <pthread.h>
#  define SWIFT_THREAD_KEY_CREATE pthread_key_create
#  define SWIFT_THREAD_GETSPECIFIC pthread_getspecific
#  define SWIFT_THREAD_SETSPECIFIC pthread_setspecific
# endif
#endif

#endif // SWIFT_RUNTIME_THREADLOCALSTORAGE_H
