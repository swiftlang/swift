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

#include "swift/Runtime/Config.h"

#if SWIFT_STDLIB_THREADING_DARWIN || SWIFT_STDLIB_THREADING_PTHREADS
#include <pthread.h>
#elif SWIFT_STDLIB_THREADING_C11
#include <threads.h>
#elif SWIFT_STDLIB_THREADING_WIN32
#  define WIN32_LEAN_AND_MEAN
#  include <Windows.h>
#endif

// Depending on the target, we may be able to use dedicated TSD keys or
// thread_local variables. When dedicated TSD keys aren't available,
// wrap the target's API for thread-local data for things that don't want
// to use thread_local.

// On Apple platforms, we have dedicated TSD keys.
#if SWIFT_STDLIB_THREADING_DARWIN
# define SWIFT_TLS_HAS_RESERVED_PTHREAD_SPECIFIC 1
// Use reserved TSD keys.
# if __has_include(<pthread/tsd_private.h>)
#  include <pthread/tsd_private.h>
# else
// We still need to use the SPI for setting the destructor, so declare it here.
extern "C" int pthread_key_init_np(int key, void (*destructor)(void *));
#define SWIFT_THREAD_KEY_INIT pthread_key_init_np
# endif

// If the keys are not available from the header, define them ourselves. The values match
// what tsd_private.h provides.
# ifndef __PTK_FRAMEWORK_SWIFT_KEY0
#  define __PTK_FRAMEWORK_SWIFT_KEY0 100
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY1
#  define __PTK_FRAMEWORK_SWIFT_KEY1 101
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY2
#  define __PTK_FRAMEWORK_SWIFT_KEY2 102
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY3
#  define __PTK_FRAMEWORK_SWIFT_KEY3 103
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY4
#  define __PTK_FRAMEWORK_SWIFT_KEY4 104
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY5
#  define __PTK_FRAMEWORK_SWIFT_KEY5 105
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY6
#  define __PTK_FRAMEWORK_SWIFT_KEY6 106
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY7
#  define __PTK_FRAMEWORK_SWIFT_KEY7 107
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY8
#  define __PTK_FRAMEWORK_SWIFT_KEY8 108
# endif
# ifndef __PTK_FRAMEWORK_SWIFT_KEY9
#  define __PTK_FRAMEWORK_SWIFT_KEY9 109
# endif


# define SWIFT_RUNTIME_TLS_KEY __PTK_FRAMEWORK_SWIFT_KEY0
# define SWIFT_STDLIB_TLS_KEY __PTK_FRAMEWORK_SWIFT_KEY1
# define SWIFT_COMPATIBILITY_50_TLS_KEY __PTK_FRAMEWORK_SWIFT_KEY2
# define SWIFT_CONCURRENCY_TASK_KEY __PTK_FRAMEWORK_SWIFT_KEY3
# define SWIFT_CONCURRENCY_EXECUTOR_TRACKING_INFO_KEY __PTK_FRAMEWORK_SWIFT_KEY4
# define SWIFT_CONCURRENCY_FALLBACK_TASK_LOCAL_STORAGE_KEY \
  __PTK_FRAMEWORK_SWIFT_KEY5

#endif

// If the reserved key path didn't already provide get/setspecific macros,
// wrap the platform's APIs.
#ifndef SWIFT_THREAD_GETSPECIFIC

// Pick the right typedef for the key.
#if SWIFT_STDLIB_THREADING_WIN32
typedef DWORD __swift_thread_key_t;
#  define SWIFT_THREAD_KEY_CREATE _stdlib_thread_key_create
#  define SWIFT_THREAD_GETSPECIFIC FlsGetValue
#  define SWIFT_THREAD_SETSPECIFIC(key, value) (FlsSetValue(key, value) == FALSE)
#elif SWIFT_STDLIB_THREADING_PTHREADS || SWIFT_STDLIB_THREADING_DARWIN
typedef pthread_key_t __swift_thread_key_t;
#  define SWIFT_THREAD_KEY_CREATE pthread_key_create
#  define SWIFT_THREAD_GETSPECIFIC pthread_getspecific
#  define SWIFT_THREAD_SETSPECIFIC pthread_setspecific
#elif SWIFT_STDLIB_THREADING_C11
typedef tss_t __swift_thread_key_t;
#  define SWIFT_THREAD_KEY_CREATE tss_create
#  define SWIFT_THREAD_GETSPECIFIC tss_get
#  define SWIFT_THREAD_SETSPECIFIC tss_set
#else
typedef unsigned long __swift_thread_key_t;
#endif

#endif

#endif // SWIFT_RUNTIME_THREADLOCALSTORAGE_H
