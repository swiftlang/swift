//===--- Stubs.cpp - Swift Language ABI Runtime Stubs ---------------------===//
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
#else
#include <sys/resource.h>
#include <sys/errno.h>
#include <unistd.h>
#endif
#include <climits>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#if defined(__CYGWIN__) || defined(_WIN32)
#include <sstream>
#include <cmath>
#define fmodl(lhs, rhs) std::fmod(lhs, rhs)
#elif defined(__ANDROID__)
// Android's libc implementation Bionic currently only supports the "C" locale
// (https://android.googlesource.com/platform/bionic/+/ndk-r11c/libc/bionic/locale.cpp#40).
// As such, we have no choice but to map functions like strtod_l, which should
// respect the given locale_t parameter, to functions like strtod, which do not.
#include <locale.h>
static double swift_strtod_l(const char *nptr, char **endptr, locale_t loc) {
  return strtod(nptr, endptr);
}
static float swift_strtof_l(const char *nptr, char **endptr, locale_t loc) {
  return strtof(nptr, endptr);
}
static long double swift_strtold_l(const char *nptr,
                                   char **endptr,
                                   locale_t loc) {
  return strtod(nptr, endptr);
}
#define strtod_l swift_strtod_l
#define strtof_l swift_strtof_l
#define strtold_l swift_strtold_l
#else
#include <xlocale.h>
#endif
#include <limits>
#include <thread>
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Compiler.h"
#include "swift/Runtime/Debug.h"
#include "swift/Basic/Lazy.h"

#include "../SwiftShims/RuntimeShims.h"
#include "../SwiftShims/RuntimeStubs.h"

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

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
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

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
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

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__ANDROID__)
static inline locale_t getCLocale() {
  // On these platforms convenience functions from xlocale.h interpret nullptr
  // as C locale.
  return nullptr;
}
#elif defined(__CYGWIN__) || defined(_WIN32)
// In Cygwin, getCLocale() is not used.
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

#if defined(__APPLE__)
#define swift_snprintf_l snprintf_l
#elif defined(__CYGWIN__) || defined(_WIN32)
// In Cygwin, swift_snprintf_l() is not used.
#else
static int swift_snprintf_l(char *Str, size_t StrSize, locale_t Locale,
                            const char *Format, ...) {
  if (Locale == nullptr) {
    Locale = getCLocale();
  }
  locale_t OldLocale = uselocale(Locale);

  va_list Args;
  va_start(Args, Format);
  int Result = std::vsnprintf(Str, StrSize, Format, Args);
  va_end(Args);

  uselocale(OldLocale);

  return Result;
}
#endif

template <typename T>
static uint64_t swift_floatingPointToString(char *Buffer, size_t BufferLength,
                                            T Value, const char *Format, 
                                            bool Debug) {
  if (BufferLength < 32)
    swift::crash("swift_floatingPointToString: insufficient buffer size");

  int Precision = std::numeric_limits<T>::digits10;
  if (Debug) {
    Precision = std::numeric_limits<T>::max_digits10;
  }

#if defined(__CYGWIN__) || defined(_WIN32)
  // Cygwin does not support uselocale(), but we can use the locale feature 
  // in stringstream object.
  std::ostringstream ValueStream;
  ValueStream.width(0);
  ValueStream.precision(Precision);
  ValueStream.imbue(std::locale::classic());
  ValueStream << Value;
  std::string ValueString(ValueStream.str());
  size_t i = ValueString.length();

  if (i < BufferLength) {
    std::copy(ValueString.begin(), ValueString.end(), Buffer);
    Buffer[i] = '\0';
  } else {
    swift::crash("swift_floatingPointToString: insufficient buffer size");
  }
#else
  // Pass a null locale to use the C locale.
  int i = swift_snprintf_l(Buffer, BufferLength, /*Locale=*/nullptr, Format,
                           Precision, Value);

  if (i < 0)
    swift::crash(
        "swift_floatingPointToString: unexpected return value from sprintf");
  if (size_t(i) >= BufferLength)
    swift::crash("swift_floatingPointToString: insufficient buffer size");
#endif

  // Add ".0" to a float that (a) is not in scientific notation, (b) does not
  // already have a fractional part, (c) is not infinite, and (d) is not a NaN
  // value.
  if (strchr(Buffer, 'e') == nullptr && strchr(Buffer, '.') == nullptr &&
      strchr(Buffer, 'n') == nullptr) {
    Buffer[i++] = '.';
    Buffer[i++] = '0';
  }

  return i;
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
uint64_t swift_float32ToString(char *Buffer, size_t BufferLength,
                               float Value, bool Debug) {
  return swift_floatingPointToString<float>(Buffer, BufferLength, Value,
                                            "%0.*g", Debug);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
uint64_t swift_float64ToString(char *Buffer, size_t BufferLength,
                               double Value, bool Debug) {
  return swift_floatingPointToString<double>(Buffer, BufferLength, Value,
                                             "%0.*g", Debug);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
uint64_t swift_float80ToString(char *Buffer, size_t BufferLength,
                               long double Value, bool Debug) {
  return swift_floatingPointToString<long double>(Buffer, BufferLength, Value,
                                                  "%0.*Lg", Debug);
}

/// \param[out] LinePtr Replaced with the pointer to the malloc()-allocated
/// line.  Can be NULL if no characters were read. This buffer should be
/// freed by the caller if this function returns a positive value.
///
/// \returns Size of character data returned in \c LinePtr, or -1
/// if an error occurred, or EOF was reached.
ssize_t swift::swift_stdlib_readLine_stdin(unsigned char **LinePtr) {
#if defined(_WIN32)
  if (LinePtr == nullptr)
    return -1;

  ssize_t Capacity = 0;
  ssize_t Pos = 0;
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
  return getline((char **)LinePtr, &Capacity, stdin);
#endif
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
float _swift_fmodf(float lhs, float rhs) {
    return fmodf(lhs, rhs);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
double _swift_fmod(double lhs, double rhs) {
    return fmod(lhs, rhs);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERFACE
long double _swift_fmodl(long double lhs, long double rhs) {
    return fmodl(lhs, rhs);
}


// Although this builtin is provided by clang rt builtins,
// it isn't provided by libgcc, which is the default
// runtime library on Linux, even when compiling with clang.
// This implementation is copied here to avoid a new dependency
// on compiler-rt on Linux.
// FIXME: rdar://14883575 Libcompiler_rt omits muloti4
#if (defined(__linux__) && defined(__x86_64__)) || \
    (defined(__linux__) && defined(__aarch64__)) || \
    (defined(__linux__) && defined(__powerpc64__)) || \
    (defined(__linux__) && defined(__s390x__)) || \
    (defined(__ANDROID__) && defined(__arm64__))

typedef int ti_int __attribute__((__mode__(TI)));
SWIFT_RUNTIME_STDLIB_INTERFACE
ti_int
__muloti4(ti_int a, ti_int b, int* overflow)
{
    const int N = (int)(sizeof(ti_int) * CHAR_BIT);
    const ti_int MIN = (ti_int)1 << (N-1);
    const ti_int MAX = ~MIN;
    *overflow = 0;
    ti_int result = a * b;
    if (a == MIN)
    {
        if (b != 0 && b != 1)
            *overflow = 1;
        return result;
    }
    if (b == MIN)
    {
        if (a != 0 && a != 1)
            *overflow = 1;
        return result;
    }
    ti_int sa = a >> (N - 1);
    ti_int abs_a = (a ^ sa) - sa;
    ti_int sb = b >> (N - 1);
    ti_int abs_b = (b ^ sb) - sb;
    if (abs_a < 2 || abs_b < 2)
        return result;
    if (sa == sb)
    {
        if (abs_a > MAX / abs_b)
            *overflow = 1;
    }
    else
    {
        if (abs_a > MIN / -abs_b)
            *overflow = 1;
    }
    return result;
}

#endif

// FIXME: ideally we would have a slow path here for Windows which would be
// lowered to instructions as though MSVC had generated.  There does not seem to
// be a MSVC provided multiply with overflow detection that I can see, but this
// avoids an unnecessary dependency on compiler-rt for a single function.
#if (defined(__linux__) && defined(__arm__)) || defined(_WIN32)
// Similar to above, but with mulodi4.  Perhaps this is
// something that shouldn't be done, and is a bandaid over
// some other lower-level architecture issue that I'm
// missing.  Perhaps relevant bug report:
// FIXME: https://llvm.org/bugs/show_bug.cgi?id=14469
#if __has_attribute(__mode__)
#define SWIFT_MODE_DI __attribute__((__mode__(DI)))
#else
#define SWIFT_MODE_DI
#endif

typedef int di_int SWIFT_MODE_DI;
SWIFT_RUNTIME_STDLIB_INTERFACE
di_int
__mulodi4(di_int a, di_int b, int* overflow)
{
    const int N = (int)(sizeof(di_int) * CHAR_BIT);
    const di_int MIN = (di_int)1 << (N-1);
    const di_int MAX = ~MIN;
    *overflow = 0;
    di_int result = a * b;
    if (a == MIN)
    {
        if (b != 0 && b != 1)
            *overflow = 1;
        return result;
    }
    if (b == MIN)
    {
        if (a != 0 && a != 1)
            *overflow = 1;
        return result;
    }
    di_int sa = a >> (N - 1);
    di_int abs_a = (a ^ sa) - sa;
    di_int sb = b >> (N - 1);
    di_int abs_b = (b ^ sb) - sb;
    if (abs_a < 2 || abs_b < 2)
        return result;
    if (sa == sb)
    {
        if (abs_a > MAX / abs_b)
            *overflow = 1;
    }
    else
    {
        if (abs_a > MIN / -abs_b)
            *overflow = 1;
    }
    return result;
}
#endif

#if defined(__CYGWIN__) || defined(_WIN32)
  #define strcasecmp _stricmp
#endif

static bool swift_stringIsSignalingNaN(const char *nptr) {
  if (nptr[0] == '+' || nptr[0] == '-') {
    nptr++;
  }

  return strcasecmp(nptr, "snan") == 0;
}

#if defined(__CYGWIN__) || defined(_WIN32)
// Cygwin does not support uselocale(), but we can use the locale feature 
// in stringstream object.
template <typename T>
static const char *_swift_stdlib_strtoX_clocale_impl(
    const char *nptr, T *outResult) {
  if (swift_stringIsSignalingNaN(nptr)) {
    *outResult = std::numeric_limits<T>::signaling_NaN();
    return nptr + std::strlen(nptr);
  }
  
  std::istringstream ValueStream(nptr);
  ValueStream.imbue(std::locale::classic());
  T ParsedValue;
  ValueStream >> ParsedValue;
  *outResult = ParsedValue;

  int pos = ValueStream.tellg();
  if (pos <= 0)
    return nullptr;

  return nptr + pos;
}

const char *swift::_swift_stdlib_strtold_clocale(
    const char *nptr, void *outResult) {
  return _swift_stdlib_strtoX_clocale_impl(
    nptr, static_cast<long double*>(outResult));
}

const char *swift::_swift_stdlib_strtod_clocale(
    const char * nptr, double *outResult) {
  return _swift_stdlib_strtoX_clocale_impl(nptr, outResult);
}

const char *swift::_swift_stdlib_strtof_clocale(
    const char * nptr, float *outResult) {
  return _swift_stdlib_strtoX_clocale_impl(nptr, outResult);
}
#else

// We can't return Float80, but we can receive a pointer to one, so
// switch the return type and the out parameter on strtold.
template <typename T>
static const char *_swift_stdlib_strtoX_clocale_impl(
    const char * nptr, T* outResult, T huge,
    T (*posixImpl)(const char *, char **, locale_t)
) {
  if (swift_stringIsSignalingNaN(nptr)) {
    // TODO: ensure that the returned sNaN bit pattern matches that of sNaNs
    // produced by Swift.
    *outResult = std::numeric_limits<T>::signaling_NaN();
    return nptr + std::strlen(nptr);
  }
  
  char *EndPtr;
  errno = 0;
  const auto result = posixImpl(nptr, &EndPtr, getCLocale());
  *outResult = result;
  if (result == huge || result == -huge || result == 0.0 || result == -0.0) {
      if (errno == ERANGE)
          EndPtr = nullptr;
  }
  return EndPtr;
}
    
const char *swift::_swift_stdlib_strtold_clocale(
  const char * nptr, void *outResult) {
  return _swift_stdlib_strtoX_clocale_impl(
    nptr, static_cast<long double*>(outResult), HUGE_VALL, strtold_l);
}

const char *swift::_swift_stdlib_strtod_clocale(
    const char * nptr, double *outResult) {
  return _swift_stdlib_strtoX_clocale_impl(
    nptr, outResult, HUGE_VAL, strtod_l);
}

const char *swift::_swift_stdlib_strtof_clocale(
    const char * nptr, float *outResult) {
  return _swift_stdlib_strtoX_clocale_impl(
    nptr, outResult, HUGE_VALF, strtof_l);
}
#endif

void swift::_swift_stdlib_flockfile_stdout() {
#if defined(_WIN32)
  _lock_file(stdout);
#else
  flockfile(stdout);
#endif
}

void swift::_swift_stdlib_funlockfile_stdout() {
#if defined(_WIN32)
  _unlock_file(stdout);
#else
  funlockfile(stdout);
#endif
}

int swift::_swift_stdlib_putc_stderr(int C) {
  return putc(C, stderr);
}

size_t swift::_swift_stdlib_getHardwareConcurrency() {
  return std::thread::hardware_concurrency();
}
