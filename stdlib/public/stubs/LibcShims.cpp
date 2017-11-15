//===--- LibcShims.cpp ----------------------------------------------------===//
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

#include <random>
#include <type_traits>
#include <cmath>
#if defined(_WIN32)
#include <io.h>
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <unistd.h>
#include <pthread.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <string.h>
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"
#include "../SwiftShims/LibcShims.h"
#include "llvm/Support/DataTypes.h"

using namespace swift;

static_assert(std::is_same<ssize_t, swift::__swift_ssize_t>::value,
              "__swift_ssize_t must be defined as equivalent to ssize_t");

SWIFT_RUNTIME_STDLIB_INTERFACE
void swift::_swift_stdlib_free(void *ptr) {
  free(ptr);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_putchar_unlocked(int c) {
#if defined(_WIN32)
  return _putc_nolock(c, stdout);
#else
  return putchar_unlocked(c);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_swift_stdlib_fwrite_stdout(const void *ptr,
                                                  __swift_size_t size,
                                                  __swift_size_t nitems) {
  return fwrite(ptr, size, nitems, stdout);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_swift_stdlib_strlen(const char *s) {
  return strlen(s);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_swift_stdlib_strlen_unsigned(const unsigned char *s) {
  return strlen(reinterpret_cast<const char *>(s));
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_memcmp(const void *s1, const void *s2,
                                __swift_size_t n) {
  return memcmp(s1, s2, n);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_open(const char *path, int oflags) {
#if defined(_WIN32)
  return _open(path, oflags);
#else
  return open(path, oflags);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_ssize_t
swift::_swift_stdlib_read(int fd, void *buf, __swift_size_t nbyte) {
#if defined(_WIN32)
  return _read(fd, buf, nbyte);
#else
  return read(fd, buf, nbyte);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_ssize_t
swift::_swift_stdlib_write(int fd, const void *buf, __swift_size_t nbyte) {
#if defined(_WIN32)
  return _write(fd, buf, nbyte);
#else
  return write(fd, buf, nbyte);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_close(int fd) {
#if defined(_WIN32)
  return _close(fd);
#else
  return close(fd);
#endif
}

#if defined(_WIN32)
static_assert(std::is_same<__swift_thread_key_t, DWORD>::value,
              "__swift_thread_key_t is not a DWORD");

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_stdlib_destroyTLS(void *);

static void
#if defined(_M_IX86)
__stdcall
#endif
_swift_stdlib_destroyTLS_CCAdjustmentThunk(void *ptr) {
  _swift_stdlib_destroyTLS(ptr);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int
swift::_swift_stdlib_thread_key_create(__swift_thread_key_t * _Nonnull key,
                                       void (* _Nullable destructor)(void *)) {
  *key = FlsAlloc(_swift_stdlib_destroyTLS_CCAdjustmentThunk);
  return *key != FLS_OUT_OF_INDEXES;
}

SWIFT_RUNTIME_STDLIB_INTERFACE
void * _Nullable
swift::_swift_stdlib_thread_getspecific(__swift_thread_key_t key) {
  return FlsGetValue(key);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_thread_setspecific(__swift_thread_key_t key,
                                            const void * _Nullable value) {
  return FlsSetValue(key, const_cast<void *>(value)) == TRUE;
}
#else
// Guard compilation on the typedef for __swift_thread_key_t in LibcShims.h
// being identical to the platform's pthread_key_t
static_assert(std::is_same<__swift_thread_key_t, pthread_key_t>::value,
              "This platform's pthread_key_t differs. If you hit this assert, "
              "fix __swift_pthread_key_t's typedef in LibcShims.h by adding an "
              "#if guard and definition for your platform");

SWIFT_RUNTIME_STDLIB_INTERFACE
int
swift::_swift_stdlib_thread_key_create(__swift_thread_key_t * _Nonnull key,
                                       void (* _Nullable destructor)(void *)) {
  return pthread_key_create(key, destructor);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
void * _Nullable
swift::_swift_stdlib_thread_getspecific(__swift_thread_key_t key) {
  return pthread_getspecific(key);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_thread_setspecific(__swift_thread_key_t key,
                                            const void * _Nullable value) {
  return pthread_setspecific(key, value);
}
#endif

#if defined(__APPLE__)
#include <malloc/malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_size(ptr);
}
#elif defined(__GNU_LIBRARY__) || defined(__CYGWIN__) || defined(__ANDROID__) || defined(__HAIKU__)
#if defined(__HAIKU__)
#define _GNU_SOURCE
#endif
#include <malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#elif defined(_WIN32)
#include <malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return _msize(const_cast<void *>(ptr));
}
#elif defined(__FreeBSD__)
#include <malloc_np.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#else
#error No malloc_size analog known for this platform/libc.
#endif

static Lazy<std::mt19937> theGlobalMT19937;

static std::mt19937 &getGlobalMT19937() {
  return theGlobalMT19937.get();
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t swift::_swift_stdlib_cxx11_mt19937() {
  return getGlobalMT19937()();
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t
swift::_swift_stdlib_cxx11_mt19937_uniform(__swift_uint32_t upper_bound) {
  if (upper_bound > 0)
    upper_bound--;
  std::uniform_int_distribution<__swift_uint32_t> RandomUniform(0, upper_bound);
  return RandomUniform(getGlobalMT19937());
}

#if defined(__APPLE__)
#include "TargetConditionals.h"
#if defined(TARGET_IPHONE_SIMULATOR) \
  || ( defined(TARGET_OS_IPHONE) && __IPHONE_OS_VERSION_MIN_REQUIRED >= 10000 ) \
  || ( defined(TARGET_OS_MAC) && __MAC_OS_X_VERSION_MIN_REQUIRED >= 101200 )
SWIFT_RUNTIME_STDLIB_INTERFACE
void swift::_swift_stdlib_random(void *buf,
                                __swift_ssize_t nbytes,
                                __swift_uint32_t debug_flags) {
  arc4random_buf(buf, nbytes);
}
#else
#include <Security/Security.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
void swift::_swift_stdlib_random(void *buf,
                                __swift_ssize_t nbytes,
                                __swift_uint32_t debug_flags) {
  if (SecRandomCopyBytes(kSecRandomDefault, nbytes, buf) != 0) {
    fatalError(
      debug_flags,
      "Fatal error: Unexpected error with SecRandomCopyBytes\n"
    );
  }
}
#endif
#elif defined(__linux__)
#include <linux/version.h>
#if LINUX_VERSION_CODE >= KERNEL_VERSION(3,17,0)
#define _GNU_SOURCE
#include <sys/syscall.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
void swift::_swift_stdlib_random(void *buf,
                                __swift_ssize_t nbytes,
                                __swift_uint32_t debug_flags) {
  int result = syscall(SYS_getrandom, buf, nbytes, 0);
  if (result != 0) {
    fatalError(debug_flags, "Fatal error: Unexpected error with getrandom\n")
  }
}
#else
SWIFT_RUNTIME_STDLIB_INTERFACE
void swift::_swift_stdlib_random(void *buf,
                                __swift_ssize_t nbytes,
                                __swift_uint32_t debug_flags) {
  int oflags = O_RDONLY;
  int fd = swift::_swift_stdlib_open("/dev/urandom", oflags);
  if (fd < 0) {
    fatalError(debug_flags, "Fatal error: Unable to open /dev/urandom\n");
  }
  if (swift::_swift_stdlib_read(fd, buf, nbytes) < 0) {
    fatalError(debug_flags, "Fatal error: Unable to read /dev/urandom\n");
  }
  swift::_swift_stdlib_close(fd);
}
#endif
#endif
