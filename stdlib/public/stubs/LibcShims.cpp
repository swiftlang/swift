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

#if defined(__APPLE__)
#define _REENTRANT
#include <math.h>
#endif
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
#include <semaphore.h>
#include <sys/ioctl.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Config.h"
#include "../SwiftShims/LibcShims.h"
#include "llvm/Support/DataTypes.h"

using namespace swift;

static_assert(std::is_same<ssize_t, swift::__swift_ssize_t>::value,
              "__swift_ssize_t must be defined as equivalent to ssize_t in LibcShims.h");
#if !defined(_WIN32) || defined(__CYGWIN__)
static_assert(std::is_same<mode_t, swift::__swift_mode_t>::value,
              "__swift_mode_t must be defined as equivalent to mode_t in LibcShims.h");
#endif

SWIFT_RUNTIME_STDLIB_INTERFACE
void swift::_stdlib_free(void *ptr) {
  free(ptr);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_stdlib_putchar_unlocked(int c) {
#if defined(_WIN32)
  return _putc_nolock(c, stdout);
#else
  return putchar_unlocked(c);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_stdlib_fwrite_stdout(const void *ptr,
                                         __swift_size_t size,
                                         __swift_size_t nitems) {
    return fwrite(ptr, size, nitems, stdout);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_stdlib_strlen(const char *s) {
    return strlen(s);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_stdlib_strlen_unsigned(const unsigned char *s) {
  return strlen(reinterpret_cast<const char *>(s));
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_stdlib_memcmp(const void *s1, const void *s2,
                       __swift_size_t n) {
  return memcmp(s1, s2, n);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_ssize_t
swift::_stdlib_read(int fd, void *buf, __swift_size_t nbyte) {
#if defined(_WIN32)
  return _read(fd, buf, nbyte);
#else
  return read(fd, buf, nbyte);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_ssize_t
swift::_stdlib_write(int fd, const void *buf, __swift_size_t nbyte) {
#if defined(_WIN32)
  return _write(fd, buf, nbyte);
#else
  return write(fd, buf, nbyte);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_stdlib_close(int fd) {
#if defined(_WIN32)
  return _close(fd);
#else
  return close(fd);
#endif
}


#if defined(_WIN32) && !defined(__CYGWIN__)
// Windows

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_stdlib_open(const char *path, int oflag, __swift_mode_t mode) {
  return _open(path, oflag, static_cast<int>(mode));
}

#else
// not Windows

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_stdlib_open(const char *path, int oflag, __swift_mode_t mode) {
  return open(path, oflag, mode);
}

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_stdlib_openat(int fd, const char *path, int oflag,
                          __swift_mode_t mode) {
  return openat(fd, path, oflag, mode);
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void *swift::_stdlib_sem_open2(const char *name, int oflag) {
  return sem_open(name, oflag);
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void *swift::_stdlib_sem_open4(const char *name, int oflag,
                               __swift_mode_t mode, unsigned int value) {
  return sem_open(name, oflag, mode, value);
}

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_stdlib_fcntl(int fd, int cmd, int value) {
  return fcntl(fd, cmd, value);
}

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_stdlib_fcntlPtr(int fd, int cmd, void* ptr) {
  return fcntl(fd, cmd, ptr);
}

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_stdlib_ioctl(int fd, unsigned long int request, int value) {
  return ioctl(fd, request, value);
}

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_stdlib_ioctlPtr(int fd, unsigned long int request, void* ptr) {
  return ioctl(fd, request, ptr);
}

#if defined(__FreeBSD__)
SWIFT_RUNTIME_STDLIB_INTERNAL
char * _Nullable *swift::_stdlib_getEnviron() {
  extern char **environ;
  return environ;
}
#elif defined(__APPLE__)
SWIFT_RUNTIME_STDLIB_INTERNAL
char * _Nullable *swift::_stdlib_getEnviron() {
  extern char * _Nullable **_NSGetEnviron(void);
  return *_NSGetEnviron();
}
#endif

#endif // !(defined(_WIN32) && !defined(__CYGWIN__))

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_stdlib_getErrno() {
  return errno;
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift::_stdlib_setErrno(int value) {
  errno = value;
}


#if defined(_WIN32)
static_assert(std::is_same<__swift_thread_key_t, DWORD>::value,
              "__swift_thread_key_t is not a DWORD");

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
void _stdlib_destroyTLS(void *);

static void
#if defined(_M_IX86)
__stdcall
#endif
destroyTLS_CCAdjustmentThunk(void *ptr) {
  _stdlib_destroyTLS(ptr);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int
swift::_stdlib_thread_key_create(__swift_thread_key_t * _Nonnull key,
                              void (* _Nullable destructor)(void *)) {
  *key = FlsAlloc(destroyTLS_CCAdjustmentThunk);
  return *key != FLS_OUT_OF_INDEXES;
}

SWIFT_RUNTIME_STDLIB_INTERFACE
void * _Nullable
swift::_stdlib_thread_getspecific(__swift_thread_key_t key) {
  return FlsGetValue(key);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_stdlib_thread_setspecific(__swift_thread_key_t key,
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
swift::_stdlib_thread_key_create(__swift_thread_key_t * _Nonnull key,
                              void (* _Nullable destructor)(void *)) {
  return pthread_key_create(key, destructor);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
void * _Nullable
swift::_stdlib_thread_getspecific(__swift_thread_key_t key) {
  return pthread_getspecific(key);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_stdlib_thread_setspecific(__swift_thread_key_t key,
                                      const void * _Nullable value) {
  return pthread_setspecific(key, value);
}
#endif

#if defined(__APPLE__)
#include <malloc/malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_stdlib_malloc_size(const void *ptr) {
  return malloc_size(ptr);
}
#elif defined(__GNU_LIBRARY__) || defined(__CYGWIN__) || defined(__ANDROID__) || defined(__HAIKU__)
#if defined(__HAIKU__)
#define _GNU_SOURCE
#endif
#include <malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#elif defined(_WIN32)
#include <malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_stdlib_malloc_size(const void *ptr) {
  return _msize(const_cast<void *>(ptr));
}
#elif defined(__FreeBSD__)
#include <malloc_np.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_stdlib_malloc_size(const void *ptr) {
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
__swift_uint32_t swift::_stdlib_cxx11_mt19937() {
  return getGlobalMT19937()();
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t
swift::_stdlib_cxx11_mt19937_uniform(__swift_uint32_t upper_bound) {
  if (upper_bound > 0)
    upper_bound--;
  std::uniform_int_distribution<__swift_uint32_t> RandomUniform(0, upper_bound);
  return RandomUniform(getGlobalMT19937());
}
