//===-- llvm/Support/Compiler.h - Compiler abstraction support --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines several macros, based on the current compiler.  This allows
// use of compiler-specific features in a way that remains portable. This header
// can be included from either C or C++.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SUPPORT_COMPILER_H
#define LLVM_SUPPORT_COMPILER_H

#ifdef __cplusplus
#include <new>
#endif
#include <stddef.h>

#if defined(_MSC_VER)
#include <sal.h>
#endif

#ifndef __has_feature
# define __has_feature(x) 0
#endif

#ifndef __has_extension
# define __has_extension(x) 0
#endif

#ifndef __has_attribute
# define __has_attribute(x) 0
#endif

#ifndef __has_builtin
# define __has_builtin(x) 0
#endif

// Only use __has_cpp_attribute in C++ mode. GCC defines __has_cpp_attribute in
// C mode, but the :: in __has_cpp_attribute(scoped::attribute) is invalid.
#ifndef LLVM_HAS_CPP_ATTRIBUTE
#if defined(__cplusplus) && defined(__has_cpp_attribute)
# define LLVM_HAS_CPP_ATTRIBUTE(x) __has_cpp_attribute(x)
#else
# define LLVM_HAS_CPP_ATTRIBUTE(x) 0
#endif
#endif

/// \macro LLVM_GNUC_PREREQ
/// Extend the default __GNUC_PREREQ even if glibc's features.h isn't
/// available.
#ifndef LLVM_GNUC_PREREQ
# if defined(__GNUC__) && defined(__GNUC_MINOR__) && defined(__GNUC_PATCHLEVEL__)
#  define LLVM_GNUC_PREREQ(maj, min, patch) \
    ((__GNUC__ << 20) + (__GNUC_MINOR__ << 10) + __GNUC_PATCHLEVEL__ >= \
     ((maj) << 20) + ((min) << 10) + (patch))
# elif defined(__GNUC__) && defined(__GNUC_MINOR__)
#  define LLVM_GNUC_PREREQ(maj, min, patch) \
    ((__GNUC__ << 20) + (__GNUC_MINOR__ << 10) >= ((maj) << 20) + ((min) << 10))
# else
#  define LLVM_GNUC_PREREQ(maj, min, patch) 0
# endif
#endif

/// \macro LLVM_MSC_PREREQ
/// Is the compiler MSVC of at least the specified version?
/// The common \param version values to check for are:
/// * 1910: VS2017, version 15.1 & 15.2
/// * 1911: VS2017, version 15.3 & 15.4
/// * 1912: VS2017, version 15.5
/// * 1913: VS2017, version 15.6
/// * 1914: VS2017, version 15.7
/// * 1915: VS2017, version 15.8
/// * 1916: VS2017, version 15.9
/// * 1920: VS2019, version 16.0
/// * 1921: VS2019, version 16.1
#ifdef _MSC_VER
#define LLVM_MSC_PREREQ(version) (_MSC_VER >= (version))

// We require at least MSVC 2017.
#if !LLVM_MSC_PREREQ(1910)
#error LLVM requires at least MSVC 2017.
#endif

#else
#define LLVM_MSC_PREREQ(version) 0
#endif

/// Does the compiler support ref-qualifiers for *this?
///
/// Sadly, this is separate from just rvalue reference support because GCC
/// and MSVC implemented this later than everything else. This appears to be
/// corrected in MSVC 2019 but not MSVC 2017.
#if __has_feature(cxx_rvalue_references) || LLVM_GNUC_PREREQ(4, 8, 1) ||       \
    LLVM_MSC_PREREQ(1920)
#define LLVM_HAS_RVALUE_REFERENCE_THIS 1
#else
#define LLVM_HAS_RVALUE_REFERENCE_THIS 0
#endif

/// Expands to '&' if ref-qualifiers for *this are supported.
///
/// This can be used to provide lvalue/rvalue overrides of member functions.
/// The rvalue override should be guarded by LLVM_HAS_RVALUE_REFERENCE_THIS
#if LLVM_HAS_RVALUE_REFERENCE_THIS
#define LLVM_LVALUE_FUNCTION &
#else
#define LLVM_LVALUE_FUNCTION
#endif

/// LLVM_LIBRARY_VISIBILITY - If a class marked with this attribute is linked
/// into a shared library, then the class should be private to the library and
/// not accessible from outside it.  Can also be used to mark variables and
/// functions, making them private to any shared library they are linked into.
/// On PE/COFF targets, library visibility is the default, so this isn't needed.
///
/// LLVM_EXTERNAL_VISIBILITY - classes, functions, and variables marked with
/// this attribute will be made public and visible outside of any shared library
/// they are linked in to.
#if (__has_attribute(visibility) || LLVM_GNUC_PREREQ(4, 0, 0)) &&              \
    !defined(__MINGW32__) && !defined(__CYGWIN__) && !defined(_WIN32)
#define LLVM_LIBRARY_VISIBILITY __attribute__ ((visibility("hidden")))
#define LLVM_EXTERNAL_VISIBILITY __attribute__ ((visibility("default")))
#else
#define LLVM_LIBRARY_VISIBILITY
#define LLVM_EXTERNAL_VISIBILITY
#endif

#if defined(__GNUC__)
#define LLVM_PREFETCH(addr, rw, locality) __builtin_prefetch(addr, rw, locality)
#else
#define LLVM_PREFETCH(addr, rw, locality)
#endif

#if __has_attribute(used) || LLVM_GNUC_PREREQ(3, 1, 0)
#define LLVM_ATTRIBUTE_USED __attribute__((__used__))
#else
#define LLVM_ATTRIBUTE_USED
#endif

// Indicate that a non-static, non-const C++ member function reinitializes
// the entire object to a known state, independent of the previous state of
// the object.
//
// The clang-tidy check bugprone-use-after-move recognizes this attribute as a
// marker that a moved-from object has left the indeterminate state and can be
// reused.
#if LLVM_HAS_CPP_ATTRIBUTE(clang::reinitializes)
#define LLVM_ATTRIBUTE_REINITIALIZES [[clang::reinitializes]]
#else
#define LLVM_ATTRIBUTE_REINITIALIZES
#endif

// Some compilers warn about unused functions. When a function is sometimes
// used or not depending on build settings (e.g. a function only called from
// within "assert"), this attribute can be used to suppress such warnings.
//
// However, it shouldn't be used for unused *variables*, as those have a much
// more portable solution:
//   (void)unused_var_name;
// Prefer cast-to-void wherever it is sufficient.
#if __has_attribute(unused) || LLVM_GNUC_PREREQ(3, 1, 0)
#define LLVM_ATTRIBUTE_UNUSED __attribute__((__unused__))
#else
#define LLVM_ATTRIBUTE_UNUSED
#endif

// FIXME: Provide this for PE/COFF targets.
#if (__has_attribute(weak) || LLVM_GNUC_PREREQ(4, 0, 0)) &&                    \
    (!defined(__MINGW32__) && !defined(__CYGWIN__) && !defined(_WIN32))
#define LLVM_ATTRIBUTE_WEAK __attribute__((__weak__))
#else
#define LLVM_ATTRIBUTE_WEAK
#endif

// Prior to clang 3.2, clang did not accept any spelling of
// __has_attribute(const), so assume it is supported.
#if defined(__clang__) || defined(__GNUC__)
// aka 'CONST' but following LLVM Conventions.
#define LLVM_READNONE __attribute__((__const__))
#else
#define LLVM_READNONE
#endif

#if __has_attribute(pure) || defined(__GNUC__)
// aka 'PURE' but following LLVM Conventions.
#define LLVM_READONLY __attribute__((__pure__))
#else
#define LLVM_READONLY
#endif

#if __has_builtin(__builtin_expect) || LLVM_GNUC_PREREQ(4, 0, 0)
#define LLVM_LIKELY(EXPR) __builtin_expect((bool)(EXPR), true)
#define LLVM_UNLIKELY(EXPR) __builtin_expect((bool)(EXPR), false)
#else
#define LLVM_LIKELY(EXPR) (EXPR)
#define LLVM_UNLIKELY(EXPR) (EXPR)
#endif

/// LLVM_ATTRIBUTE_NOINLINE - On compilers where we have a directive to do so,
/// mark a method "not for inlining".
#if __has_attribute(noinline) || LLVM_GNUC_PREREQ(3, 4, 0)
#define LLVM_ATTRIBUTE_NOINLINE __attribute__((noinline))
#elif defined(_MSC_VER)
#define LLVM_ATTRIBUTE_NOINLINE __declspec(noinline)
#else
#define LLVM_ATTRIBUTE_NOINLINE
#endif

/// LLVM_ATTRIBUTE_ALWAYS_INLINE - On compilers where we have a directive to do
/// so, mark a method "always inline" because it is performance sensitive. GCC
/// 3.4 supported this but is buggy in various cases and produces unimplemented
/// errors, just use it in GCC 4.0 and later.
#if __has_attribute(always_inline) || LLVM_GNUC_PREREQ(4, 0, 0)
#define LLVM_ATTRIBUTE_ALWAYS_INLINE inline __attribute__((always_inline))
#elif defined(_MSC_VER)
#define LLVM_ATTRIBUTE_ALWAYS_INLINE __forceinline
#else
#define LLVM_ATTRIBUTE_ALWAYS_INLINE inline
#endif

#ifdef __GNUC__
#define LLVM_ATTRIBUTE_NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
#define LLVM_ATTRIBUTE_NORETURN __declspec(noreturn)
#else
#define LLVM_ATTRIBUTE_NORETURN
#endif

#if __has_attribute(returns_nonnull) || LLVM_GNUC_PREREQ(4, 9, 0)
#define LLVM_ATTRIBUTE_RETURNS_NONNULL __attribute__((returns_nonnull))
#elif defined(_MSC_VER)
#define LLVM_ATTRIBUTE_RETURNS_NONNULL _Ret_notnull_
#else
#define LLVM_ATTRIBUTE_RETURNS_NONNULL
#endif

/// \macro LLVM_ATTRIBUTE_RETURNS_NOALIAS Used to mark a function as returning a
/// pointer that does not alias any other valid pointer.
#ifdef __GNUC__
#define LLVM_ATTRIBUTE_RETURNS_NOALIAS __attribute__((__malloc__))
#elif defined(_MSC_VER)
#define LLVM_ATTRIBUTE_RETURNS_NOALIAS __declspec(restrict)
#else
#define LLVM_ATTRIBUTE_RETURNS_NOALIAS
#endif

/// LLVM_FALLTHROUGH - Mark fallthrough cases in switch statements.
#if defined(__cplusplus) && __cplusplus > 201402L && LLVM_HAS_CPP_ATTRIBUTE(fallthrough)
#define LLVM_FALLTHROUGH [[fallthrough]]
#elif LLVM_HAS_CPP_ATTRIBUTE(gnu::fallthrough)
#define LLVM_FALLTHROUGH [[gnu::fallthrough]]
#elif __has_attribute(fallthrough)
#define LLVM_FALLTHROUGH __attribute__((fallthrough))
#elif LLVM_HAS_CPP_ATTRIBUTE(clang::fallthrough)
#define LLVM_FALLTHROUGH [[clang::fallthrough]]
#else
#define LLVM_FALLTHROUGH
#endif

/// LLVM_REQUIRE_CONSTANT_INITIALIZATION - Apply this to globals to ensure that
/// they are constant initialized.
#if LLVM_HAS_CPP_ATTRIBUTE(clang::require_constant_initialization)
#define LLVM_REQUIRE_CONSTANT_INITIALIZATION                                   \
  [[clang::require_constant_initialization]]
#else
#define LLVM_REQUIRE_CONSTANT_INITIALIZATION
#endif

/// LLVM_GSL_OWNER - Apply this to owning classes like SmallVector to enable
/// lifetime warnings.
#if LLVM_HAS_CPP_ATTRIBUTE(gsl::Owner)
#define LLVM_GSL_OWNER [[gsl::Owner]]
#else
#define LLVM_GSL_OWNER
#endif

/// LLVM_GSL_POINTER - Apply this to non-owning classes like
/// StringRef to enable lifetime warnings.
#if LLVM_HAS_CPP_ATTRIBUTE(gsl::Pointer)
#define LLVM_GSL_POINTER [[gsl::Pointer]]
#else
#define LLVM_GSL_POINTER
#endif

/// LLVM_EXTENSION - Support compilers where we have a keyword to suppress
/// pedantic diagnostics.
#ifdef __GNUC__
#define LLVM_EXTENSION __extension__
#else
#define LLVM_EXTENSION
#endif

// LLVM_ATTRIBUTE_DEPRECATED(decl, "message")
// This macro will be removed.
// Use C++14's attribute instead: [[deprecated("message")]]
#define LLVM_ATTRIBUTE_DEPRECATED(decl, message) [[deprecated(message)]] decl

/// LLVM_BUILTIN_UNREACHABLE - On compilers which support it, expands
/// to an expression which states that it is undefined behavior for the
/// compiler to reach this point.  Otherwise is not defined.
#if __has_builtin(__builtin_unreachable) || LLVM_GNUC_PREREQ(4, 5, 0)
# define LLVM_BUILTIN_UNREACHABLE __builtin_unreachable()
#elif defined(_MSC_VER)
# define LLVM_BUILTIN_UNREACHABLE __assume(false)
#endif

/// LLVM_BUILTIN_TRAP - On compilers which support it, expands to an expression
/// which causes the program to exit abnormally.
#if __has_builtin(__builtin_trap) || LLVM_GNUC_PREREQ(4, 3, 0)
# define LLVM_BUILTIN_TRAP __builtin_trap()
#elif defined(_MSC_VER)
// The __debugbreak intrinsic is supported by MSVC, does not require forward
// declarations involving platform-specific typedefs (unlike RaiseException),
// results in a call to vectored exception handlers, and encodes to a short
// instruction that still causes the trapping behavior we want.
# define LLVM_BUILTIN_TRAP __debugbreak()
#else
# define LLVM_BUILTIN_TRAP *(volatile int*)0x11 = 0
#endif

/// LLVM_BUILTIN_DEBUGTRAP - On compilers which support it, expands to
/// an expression which causes the program to break while running
/// under a debugger.
#if __has_builtin(__builtin_debugtrap)
# define LLVM_BUILTIN_DEBUGTRAP __builtin_debugtrap()
#elif defined(_MSC_VER)
// The __debugbreak intrinsic is supported by MSVC and breaks while
// running under the debugger, and also supports invoking a debugger
// when the OS is configured appropriately.
# define LLVM_BUILTIN_DEBUGTRAP __debugbreak()
#else
// Just continue execution when built with compilers that have no
// support. This is a debugging aid and not intended to force the
// program to abort if encountered.
# define LLVM_BUILTIN_DEBUGTRAP
#endif

/// \macro LLVM_ASSUME_ALIGNED
/// Returns a pointer with an assumed alignment.
#if __has_builtin(__builtin_assume_aligned) || LLVM_GNUC_PREREQ(4, 7, 0)
# define LLVM_ASSUME_ALIGNED(p, a) __builtin_assume_aligned(p, a)
#elif defined(LLVM_BUILTIN_UNREACHABLE)
# define LLVM_ASSUME_ALIGNED(p, a) \
           (((uintptr_t(p) % (a)) == 0) ? (p) : (LLVM_BUILTIN_UNREACHABLE, (p)))
#else
# define LLVM_ASSUME_ALIGNED(p, a) (p)
#endif

/// \macro LLVM_PACKED
/// Used to specify a packed structure.
/// LLVM_PACKED(
///    struct A {
///      int i;
///      int j;
///      int k;
///      long long l;
///   });
///
/// LLVM_PACKED_START
/// struct B {
///   int i;
///   int j;
///   int k;
///   long long l;
/// };
/// LLVM_PACKED_END
#ifdef _MSC_VER
# define LLVM_PACKED(d) __pragma(pack(push, 1)) d __pragma(pack(pop))
# define LLVM_PACKED_START __pragma(pack(push, 1))
# define LLVM_PACKED_END   __pragma(pack(pop))
#else
# define LLVM_PACKED(d) d __attribute__((packed))
# define LLVM_PACKED_START _Pragma("pack(push, 1)")
# define LLVM_PACKED_END   _Pragma("pack(pop)")
#endif

/// \macro LLVM_PTR_SIZE
/// A constant integer equivalent to the value of sizeof(void*).
/// Generally used in combination with alignas or when doing computation in the
/// preprocessor.
#ifdef __SIZEOF_POINTER__
# define LLVM_PTR_SIZE __SIZEOF_POINTER__
#elif defined(_WIN64)
# define LLVM_PTR_SIZE 8
#elif defined(_WIN32)
# define LLVM_PTR_SIZE 4
#elif defined(_MSC_VER)
# error "could not determine LLVM_PTR_SIZE as a constant int for MSVC"
#else
# define LLVM_PTR_SIZE sizeof(void *)
#endif

/// \macro LLVM_MEMORY_SANITIZER_BUILD
/// Whether LLVM itself is built with MemorySanitizer instrumentation.
#if __has_feature(memory_sanitizer)
# define LLVM_MEMORY_SANITIZER_BUILD 1
# include <sanitizer/msan_interface.h>
# define LLVM_NO_SANITIZE_MEMORY_ATTRIBUTE __attribute__((no_sanitize_memory))
#else
# define LLVM_MEMORY_SANITIZER_BUILD 0
# define __msan_allocated_memory(p, size)
# define __msan_unpoison(p, size)
# define LLVM_NO_SANITIZE_MEMORY_ATTRIBUTE
#endif

/// \macro LLVM_ADDRESS_SANITIZER_BUILD
/// Whether LLVM itself is built with AddressSanitizer instrumentation.
#if __has_feature(address_sanitizer) || defined(__SANITIZE_ADDRESS__)
# define LLVM_ADDRESS_SANITIZER_BUILD 1
# include <sanitizer/asan_interface.h>
#else
# define LLVM_ADDRESS_SANITIZER_BUILD 0
# define __asan_poison_memory_region(p, size)
# define __asan_unpoison_memory_region(p, size)
#endif

/// \macro LLVM_THREAD_SANITIZER_BUILD
/// Whether LLVM itself is built with ThreadSanitizer instrumentation.
#if __has_feature(thread_sanitizer) || defined(__SANITIZE_THREAD__)
# define LLVM_THREAD_SANITIZER_BUILD 1
#else
# define LLVM_THREAD_SANITIZER_BUILD 0
#endif

#if LLVM_THREAD_SANITIZER_BUILD
// Thread Sanitizer is a tool that finds races in code.
// See http://code.google.com/p/data-race-test/wiki/DynamicAnnotations .
// tsan detects these exact functions by name.
#ifdef __cplusplus
extern "C" {
#endif
void AnnotateHappensAfter(const char *file, int line, const volatile void *cv);
void AnnotateHappensBefore(const char *file, int line, const volatile void *cv);
void AnnotateIgnoreWritesBegin(const char *file, int line);
void AnnotateIgnoreWritesEnd(const char *file, int line);
#ifdef __cplusplus
}
#endif

// This marker is used to define a happens-before arc. The race detector will
// infer an arc from the begin to the end when they share the same pointer
// argument.
# define TsanHappensBefore(cv) AnnotateHappensBefore(__FILE__, __LINE__, cv)

// This marker defines the destination of a happens-before arc.
# define TsanHappensAfter(cv) AnnotateHappensAfter(__FILE__, __LINE__, cv)

// Ignore any races on writes between here and the next TsanIgnoreWritesEnd.
# define TsanIgnoreWritesBegin() AnnotateIgnoreWritesBegin(__FILE__, __LINE__)

// Resume checking for racy writes.
# define TsanIgnoreWritesEnd() AnnotateIgnoreWritesEnd(__FILE__, __LINE__)
#else
# define TsanHappensBefore(cv)
# define TsanHappensAfter(cv)
# define TsanIgnoreWritesBegin()
# define TsanIgnoreWritesEnd()
#endif

/// \macro LLVM_NO_SANITIZE
/// Disable a particular sanitizer for a function.
#if __has_attribute(no_sanitize)
#define LLVM_NO_SANITIZE(KIND) __attribute__((no_sanitize(KIND)))
#else
#define LLVM_NO_SANITIZE(KIND)
#endif

/// Mark debug helper function definitions like dump() that should not be
/// stripped from debug builds.
/// Note that you should also surround dump() functions with
/// `#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)` so they do always
/// get stripped in release builds.
// FIXME: Move this to a private config.h as it's not usable in public headers.
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
#define LLVM_DUMP_METHOD LLVM_ATTRIBUTE_NOINLINE LLVM_ATTRIBUTE_USED
#else
#define LLVM_DUMP_METHOD LLVM_ATTRIBUTE_NOINLINE
#endif

/// \macro LLVM_PRETTY_FUNCTION
/// Gets a user-friendly looking function signature for the current scope
/// using the best available method on each platform.  The exact format of the
/// resulting string is implementation specific and non-portable, so this should
/// only be used, for example, for logging or diagnostics.
#if defined(_MSC_VER)
#define LLVM_PRETTY_FUNCTION __FUNCSIG__
#elif defined(__GNUC__) || defined(__clang__)
#define LLVM_PRETTY_FUNCTION __PRETTY_FUNCTION__
#else
#define LLVM_PRETTY_FUNCTION __func__
#endif

/// \macro LLVM_THREAD_LOCAL
/// A thread-local storage specifier which can be used with globals,
/// extern globals, and static globals.
///
/// This is essentially an extremely restricted analog to C++11's thread_local
/// support. It uses thread_local if available, falling back on gcc __thread
/// if not. __thread doesn't support many of the C++11 thread_local's
/// features. You should only use this for PODs that you can statically
/// initialize to some constant value. In almost all circumstances this is most
/// appropriate for use with a pointer, integer, or small aggregation of
/// pointers and integers.
#if LLVM_ENABLE_THREADS
#if __has_feature(cxx_thread_local) || defined(_MSC_VER)
#define LLVM_THREAD_LOCAL thread_local
#else
// Clang, GCC, and other compatible compilers used __thread prior to C++11 and
// we only need the restricted functionality that provides.
#define LLVM_THREAD_LOCAL __thread
#endif
#else // !LLVM_ENABLE_THREADS
// If threading is disabled entirely, this compiles to nothing and you get
// a normal global variable.
#define LLVM_THREAD_LOCAL
#endif

/// \macro LLVM_ENABLE_EXCEPTIONS
/// Whether LLVM is built with exception support.
#if __has_feature(cxx_exceptions)
#define LLVM_ENABLE_EXCEPTIONS 1
#elif defined(__GNUC__) && defined(__EXCEPTIONS)
#define LLVM_ENABLE_EXCEPTIONS 1
#elif defined(_MSC_VER) && defined(_CPPUNWIND)
#define LLVM_ENABLE_EXCEPTIONS 1
#endif

#endif
