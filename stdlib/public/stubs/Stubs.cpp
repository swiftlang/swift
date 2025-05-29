//===--- Stubs.cpp - Swift Language ABI Runtime Stubs ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Misc stubs for functions which should be defined in the core standard
// library, but are difficult or impossible to write in Swift at the
// moment.
//
//===----------------------------------------------------------------------===//

#if defined(__FreeBSD__)
#define _WITH_GETLINE
#endif

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
// Avoid defining macro max(), min() which conflict with std::max(), std::min()
#define NOMINMAX
#include <windows.h>
#else // defined(_WIN32)
#include <errno.h>
#if __has_include(<sys/resource.h>)
#include <sys/resource.h>
#endif
#endif // else defined(_WIN32)

#include <climits>
#include <cmath>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#if defined(__CYGWIN__) || defined(__HAIKU__)
#include <sstream>
#endif

#if SWIFT_STDLIB_HAS_LOCALE
#include <clocale>
#if __has_include(<xlocale.h>)
#include <xlocale.h>
#endif
#if defined(_WIN32)
#define locale_t _locale_t
#endif
#endif // SWIFT_STDLIB_HAS_LOCALE

#include <limits>
#ifndef SWIFT_THREADING_NONE
#include <thread>
#endif

#if defined(__ANDROID__)
#include <android/api-level.h>
#endif

#include "swift/Runtime/Debug.h"
#include "swift/Runtime/SwiftDtoa.h"
#include "swift/Basic/Lazy.h"

#include "swift/Threading/Thread.h"

#include "swift/shims/LibcShims.h"
#include "swift/shims/RuntimeShims.h"
#include "swift/shims/RuntimeStubs.h"

#include "llvm/ADT/StringExtras.h"

static uint64_t uint64ToStringImpl(char *Buffer, uint64_t Value,
                                   int64_t Radix, bool Uppercase,
                                   bool Negative) {
  char *P = Buffer;
  uint64_t Y = Value;

  if (Y == 0) {
    *P++ = '0';
  } else if (Radix == 10) {
    while (Y) {
      *P++ = '0' + char(Y % 10);
      Y /= 10;
    }
  } else {
    unsigned Radix32 = Radix;
    while (Y) {
      *P++ = llvm::hexdigit(Y % Radix32, !Uppercase);
      Y /= Radix32;
    }
  }

  if (Negative)
    *P++ = '-';
  std::reverse(Buffer, P);
  return size_t(P - Buffer);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
uint64_t swift_int64ToString(char *Buffer, size_t BufferLength,
                             int64_t Value, int64_t Radix,
                             bool Uppercase) {
  if ((Radix >= 10 && BufferLength < 32) || (Radix < 10 && BufferLength < 65))
    swift::crash("swift_int64ToString: insufficient buffer size");

  if (Radix == 0 || Radix > 36)
    swift::crash("swift_int64ToString: invalid radix for string conversion");

  bool Negative = Value < 0;

  // Compute an absolute value safely, without using unary negation on INT_MIN,
  // which is undefined behavior.
  uint64_t UnsignedValue = Value;
  if (Negative) {
    // Assumes two's complement representation.
    UnsignedValue = ~UnsignedValue + 1;
  }

  return uint64ToStringImpl(Buffer, UnsignedValue, Radix, Uppercase,
                            Negative);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
uint64_t swift_uint64ToString(char *Buffer, intptr_t BufferLength,
                              uint64_t Value, int64_t Radix,
                              bool Uppercase) {
  if ((Radix >= 10 && BufferLength < 32) || (Radix < 10 && BufferLength < 64))
    swift::crash("swift_int64ToString: insufficient buffer size");

  if (Radix == 0 || Radix > 36)
    swift::crash("swift_int64ToString: invalid radix for string conversion");

  return uint64ToStringImpl(Buffer, Value, Radix, Uppercase,
                            /*Negative=*/false);
}

#if SWIFT_STDLIB_HAS_LOCALE
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__ANDROID__)
static inline locale_t getCLocale() {
  // On these platforms convenience functions from xlocale.h interpret nullptr
  // as C locale.
  return nullptr;
}
#elif defined(_WIN32)
static _locale_t makeCLocale() {
  _locale_t CLocale = _create_locale(LC_ALL, "C");
  if (!CLocale) {
    swift::crash("makeCLocale: _create_locale() returned a null pointer");
  }
  return CLocale;
}

static _locale_t getCLocale() {
  return SWIFT_LAZY_CONSTANT(makeCLocale());
}
#else
static locale_t makeCLocale() {
  locale_t CLocale = newlocale(LC_ALL_MASK, "C", nullptr);
  if (!CLocale) {
    swift::crash("makeCLocale: newlocale() returned a null pointer");
  }
  return CLocale;
}

static locale_t getCLocale() {
  return SWIFT_LAZY_CONSTANT(makeCLocale());
}
#endif
#endif // SWIFT_STDLIB_HAS_LOCALE

#if SWIFT_DTOA_PASS_FLOAT16_AS_FLOAT
using _CFloat16Argument = float;
#else
using _CFloat16Argument = _Float16;
#endif

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
__swift_ssize_t swift_float16ToString(char *Buffer, size_t BufferLength,
                                      _CFloat16Argument Value, bool Debug) {
#if SWIFT_DTOA_PASS_FLOAT16_AS_FLOAT
  __fp16 v = Value;
  return swift_dtoa_optimal_binary16_p(&v, Buffer, BufferLength);
#else
  return swift_dtoa_optimal_binary16_p(&Value, Buffer, BufferLength);
#endif
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
uint64_t swift_float32ToString(char *Buffer, size_t BufferLength,
                               float Value, bool Debug) {
  return swift_dtoa_optimal_float(Value, Buffer, BufferLength);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
uint64_t swift_float64ToString(char *Buffer, size_t BufferLength,
                               double Value, bool Debug) {
  return swift_dtoa_optimal_double(Value, Buffer, BufferLength);
}

// We only support float80 on platforms that use that exact format for 'long double'
// This should match the conditionals in Runtime.swift
#if !defined(_WIN32) && !defined(__ANDROID__) && (defined(__i386__) || defined(__i686__) || defined(__x86_64__))
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
uint64_t swift_float80ToString(char *Buffer, size_t BufferLength,
                               long double Value, bool Debug) {
  // SwiftDtoa.cpp automatically enables float80 on platforms that use it for 'long double'
  return swift_dtoa_optimal_float80_p(&Value, Buffer, BufferLength);
}
#endif

#if SWIFT_STDLIB_HAS_STDIN

/// \param[out] LinePtr Replaced with the pointer to the malloc()-allocated
/// line.  Can be NULL if no characters were read. This buffer should be
/// freed by the caller.
///
/// \returns Size of character data returned in \c LinePtr, or -1
/// if an error occurred, or EOF was reached.
__swift_ssize_t
swift_stdlib_readLine_stdin(unsigned char **LinePtr) {
#if defined(_WIN32)
  if (LinePtr == nullptr)
    return -1;

  __swift_ssize_t Capacity = 0;
  __swift_ssize_t Pos = 0;
  unsigned char *ReadBuf = nullptr;

  _lock_file(stdin);

  for (;;) {
    int ch = _fgetc_nolock(stdin);

    if (ferror(stdin) || (ch == EOF && Pos == 0)) {
      if (ReadBuf)
        free(ReadBuf);
      _unlock_file(stdin);
      return -1;
    }

    if (Capacity - Pos <= 1) {
      // Capacity changes to 128, 128*2, 128*4, 128*8, ...
      Capacity = Capacity ? Capacity * 2 : 128;
      unsigned char *NextReadBuf =
          static_cast<unsigned char *>(realloc(ReadBuf, Capacity));
      if (NextReadBuf == nullptr) {
        if (ReadBuf)
          free(ReadBuf);
        _unlock_file(stdin);
        return -1;
      }
      ReadBuf = NextReadBuf;
    }

    if (ch == EOF)
      break;
    ReadBuf[Pos++] = ch;
    if (ch == '\n')
      break;
  }

  ReadBuf[Pos] = '\0';
  *LinePtr = ReadBuf;
  _unlock_file(stdin);
  return Pos;
#else
  size_t Capacity = 0;
  int result;
  do {
    result = getline((char **)LinePtr, &Capacity, stdin);
  } while (result < 0 && errno == EINTR);
  return result;
#endif
}

#endif  // SWIFT_STDLIB_HAS_STDIN

static bool swift_stringIsSignalingNaN(const char *nptr) {
  if (nptr[0] == '+' || nptr[0] == '-') {
    ++nptr;
  }

  if ((nptr[0] == 's' || nptr[0] == 'S') &&
      (nptr[1] == 'n' || nptr[1] == 'N') &&
      (nptr[2] == 'a' || nptr[2] == 'A') &&
      (nptr[3] == 'n' || nptr[3] == 'N') && (nptr[4] == '\0')) {
    return true;
  }

  return false;
}

#if defined(__CYGWIN__) || defined(__HAIKU__)
// This implementation should only be used on platforms without the
// relevant strto* functions, such as Cygwin or Haiku.
// Note that using this currently causes test failures.
template <typename T>
T _swift_strto(const char *nptr, char **endptr) {
  std::istringstream ValueStream(nptr);
  ValueStream.imbue(std::locale::classic());
  T ParsedValue;
  ValueStream >> ParsedValue;

  std::streamoff pos = ValueStream.tellg();
  if (ValueStream.eof())
    pos = static_cast<std::streamoff>(strlen(nptr));
  if (pos <= 0) {
    errno = ERANGE;
    return 0.0;
  }

  return ParsedValue;
}
#endif

#if SWIFT_STDLIB_HAS_LOCALE

#if defined(__OpenBSD__) || defined(_WIN32) || defined(__CYGWIN__) || defined(__HAIKU__)
#define NEED_SWIFT_STRTOD_L
#define strtod_l swift_strtod_l
#define NEED_SWIFT_STRTOF_L
#define strtof_l swift_strtof_l
#define NEED_SWIFT_STRTOLD_L
#define strtold_l swift_strtold_l
#elif defined(__ANDROID__)
#if __ANDROID_API__ < 21 // Introduced in Android API 21 - L
#define NEED_SWIFT_STRTOLD_L
#define strtold_l swift_strtold_l
#endif

#if __ANDROID_API__ < 26 // Introduced in Android API 26 - O
#define NEED_SWIFT_STRTOD_L
#define strtod_l swift_strtod_l
#define NEED_SWIFT_STRTOF_L
#define strtof_l swift_strtof_l
#endif
#endif

#endif // SWIFT_STDLIB_HAS_LOCALE

#if defined(NEED_SWIFT_STRTOD_L)
static double swift_strtod_l(const char *nptr, char **endptr, locale_t loc) {
#if defined(_WIN32)
  return _strtod_l(nptr, endptr, getCLocale());
#elif defined(__CYGWIN__) || defined(__HAIKU__)
  return _swift_strto<double>(nptr, endptr);
#else
  return strtod(nptr, endptr);
#endif
}
#endif

#if defined(NEED_SWIFT_STRTOF_L)
static float swift_strtof_l(const char *nptr, char **endptr, locale_t loc) {
#if defined(_WIN32)
  return _strtof_l(nptr, endptr, getCLocale());
#elif defined(__CYGWIN__) || defined(__HAIKU__)
  return _swift_strto<float>(nptr, endptr);
#else
  return strtof(nptr, endptr);
#endif
}
#endif

#if defined(NEED_SWIFT_STRTOLD_L)
static long double swift_strtold_l(const char *nptr, char **endptr,
                                   locale_t loc) {
#if defined(_WIN32)
  return _strtod_l(nptr, endptr, getCLocale());
#elif defined(__ANDROID__)
  return strtod(nptr, endptr);
#elif defined(__CYGWIN__) || defined(__HAIKU__)
  return _swift_strto<long double>(nptr, endptr);
#else
  return strtold(nptr, endptr);
#endif
}
#endif

#undef NEED_SWIFT_STRTOD_L
#undef NEED_SWIFT_STRTOF_L
#undef NEED_SWIFT_STRTOLD_L

static inline void _swift_set_errno(int to) {
#if defined(_WIN32)
  _set_errno(0);
#else
  errno = 0;
#endif
}

// We can't return Float80, but we can receive a pointer to one, so
// switch the return type and the out parameter on strtold.
template <typename T>
#if SWIFT_STDLIB_HAS_LOCALE
static const char *_swift_stdlib_strtoX_clocale_impl(
    const char *nptr, T *outResult, T huge,
    T (*posixImpl)(const char *, char **, locale_t))
#else
static const char *_swift_stdlib_strtoX_impl(
    const char *nptr, T *outResult,
    T (*posixImpl)(const char *, char **))
#endif
{
  if (swift_stringIsSignalingNaN(nptr)) {
    // TODO: ensure that the returned sNaN bit pattern matches that of sNaNs
    // produced by Swift.
    *outResult = std::numeric_limits<T>::signaling_NaN();
    return nptr + std::strlen(nptr);
  }

  char *EndPtr;
  _swift_set_errno(0);
#if SWIFT_STDLIB_HAS_LOCALE
  const auto result = posixImpl(nptr, &EndPtr, getCLocale());
#else
  const auto result = posixImpl(nptr, &EndPtr);
#endif
  *outResult = result;
  return EndPtr;
}

const char *_swift_stdlib_strtold_clocale(const char *nptr, void *outResult) {
#if SWIFT_STDLIB_HAS_LOCALE
  return _swift_stdlib_strtoX_clocale_impl(
      nptr, static_cast<long double *>(outResult), HUGE_VALL, strtold_l);
#else
  return _swift_stdlib_strtoX_impl(
      nptr, static_cast<long double *>(outResult), strtold);
#endif
}

const char *_swift_stdlib_strtod_clocale(const char *nptr, double *outResult) {
#if SWIFT_STDLIB_HAS_LOCALE
  return _swift_stdlib_strtoX_clocale_impl(nptr, outResult, HUGE_VAL, strtod_l);
#else
  return _swift_stdlib_strtoX_impl(nptr, outResult, strtod);
#endif
}

const char *_swift_stdlib_strtof_clocale(const char *nptr, float *outResult) {
#if SWIFT_STDLIB_HAS_LOCALE
  return _swift_stdlib_strtoX_clocale_impl(nptr, outResult, HUGE_VALF,
                                           strtof_l);
#else
  return _swift_stdlib_strtoX_impl(nptr, outResult, strtof);
#endif
}

const char *_swift_stdlib_strtof16_clocale(const char *nptr,
                                           __fp16 *outResult) {
  float tmp;
  const char *result = _swift_stdlib_strtof_clocale(nptr, &tmp);
  *outResult = tmp;
  return result;
}

void _swift_stdlib_flockfile_stdout() {
#if defined(_WIN32)
  _lock_file(stdout);
#elif defined(__wasi__)
  // FIXME: WebAssembly/WASI doesn't support file locking yet (https://github.com/apple/swift/issues/54533).
#else
  flockfile(stdout);
#endif
}

void _swift_stdlib_funlockfile_stdout() {
#if defined(_WIN32)
  _unlock_file(stdout);
#elif defined(__wasi__)
  // FIXME: WebAssembly/WASI doesn't support file locking yet (https://github.com/apple/swift/issues/54533).
#else
  funlockfile(stdout);
#endif
}

int _swift_stdlib_putc_stderr(int C) {
  return putc(C, stderr);
}

size_t _swift_stdlib_getHardwareConcurrency() {
#ifdef SWIFT_THREADING_NONE
  return 1;
#else
  return std::thread::hardware_concurrency();
#endif
}

__swift_bool swift_stdlib_isStackAllocationSafe(__swift_size_t byteCount,
                                                __swift_size_t alignment) {
  // This function is not currently implemented. Future releases of Swift can
  // implement heuristics in this function to allow for larger stack allocations
  // if conditions are suitable. These heuristics need to be significantly
  // cheaper than simply calling malloc().
  //
  // A possible implementation is provided below (#iffed out), but has not yet
  // been measured for its performance characteristics. In particular, if the
  // platform-specific functions we need to use end up calling malloc(), it's
  // pointless to use them.
  return false;

#if 0
  uintptr_t stackBegin = 0;
  uintptr_t stackEnd = 0;
  if (!_swift_stdlib_getCurrentStackBounds(&stackBegin, &stackEnd)) {
    return false;
  }

  // Locate a value on the stack. The start of this function's stack frame is a
  // good approximation.
  uintptr_t stackAddress = (uintptr_t)__builtin_frame_address(0);
  if (stackAddress < stackBegin || stackAddress >= stackEnd) {
    // The stack range we got from the OS doesn't contain the stack address we
    // just got. That may indicate that the current thread's stack has been
    // moved (e.g. with sigaltstack().)
    return false;
  }

  // How much space remains on the stack after that stack value right there?
  uintptr_t stackRemaining = stackAddress - stackBegin;

  // Make sure we leave some room at the end of the stack for other variables,
  // allocations, etc. For a 1MB stack, we'll leave the last 64KB alone.
  uintptr_t stackSafetyMargin = (stackEnd - stackBegin) >> 4;
  if (stackRemaining < stackSafetyMargin) {
    return false;
  }

  return stackRemaining >= byteCount;
#endif
}

__swift_bool _swift_stdlib_getCurrentStackBounds(__swift_uintptr_t *outBegin,
                                                 __swift_uintptr_t *outEnd) {
  std::optional<swift::Thread::StackBounds> bounds =
      swift::Thread::stackBounds();
  if (!bounds)
    return false;
  *outBegin = (uintptr_t)bounds->low;
  *outEnd = (uintptr_t)bounds->high;
  return true;
}
