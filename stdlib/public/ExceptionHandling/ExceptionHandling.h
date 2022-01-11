//===--- ExceptionHandling.h - C++ and Objective-C exception handling -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_RUNTIME_EXCEPTIONHANDLING_H__
#define __SWIFT_RUNTIME_EXCEPTIONHANDLING_H__

#include "swift/Runtime/Config.h"

#include <exception>
#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#endif
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#endif

// NOTE: Most of the functions in this header are not exported since they are
// only used by ExceptionHandling.swift and are not needed outside this module.

extern "C" {
namespace swift {

/// Create a copy of the current top-level C++ or Objective-C exception (as from
/// \c std::current_exception()) suitable for use with Swift runtime and
/// standard library functions.
///
/// \returns A pointer to a new \c std::exception_ptr structure. The caller is
///   responsible for deleting this pointer with
///   \c _swift_exceptionPointerDelete() when done with it. If there is no
///   current exception, returns \c nullptr.
std::exception_ptr *_swift_exceptionPointerCopyCurrent(void);

/// Create a copy of an existing C++ exception pointer suitable for use with
/// Swift runtime and standard library functions.
///
/// \param ep A pointer to a previously-thrown exception.
///
/// \returns A pointer to a new \c std::exception_ptr structure. The caller is
///   responsible for deleting this pointer with
///   \c _swift_exceptionPointerDelete() when done with it. If the exception
///   pointer is equal to \c nullptr, returns \c nullptr.
std::exception_ptr *_swift_exceptionPointerCopy(const std::exception_ptr *ep);

/// Delete an instance of \c std::exception_ptr.
///
/// \param ep A pointer to an instance of \c std::exception_ptr that was created
///   with \c _swift_exceptionPointerCopyCurrent().
void _swift_exceptionPointerDelete(std::exception_ptr *ep);

#pragma mark - Converting to other exception types

/// Check if an instance of \c std::exception_ptr is equal to \c nullptr.
///
/// \param ep A pointer to a previously-thrown exception.
///
/// \returns Whether or not \a ep is equal to \c nullptr.
bool _swift_exceptionPointerIsNil(const std::exception_ptr *ep);

#if SWIFT_OBJC_INTEROP
/// Get the Objective-C object (possibly an instance of \c NSException)
/// represented by the specified C++ exception pointer.
///
/// \param ep A pointer to a previously-thrown exception.
///
/// \returns The thrown Objective-C object, or \c nil if the thrown exception
///   was not an Objective-C object.
id
_swift_exceptionPointerGetThrownObjectiveCObject(const std::exception_ptr *ep);
#endif

#if defined(_WIN32)
/// Get the Windows structured exception code represented by the specified C++
/// exception pointer.
///
/// \param ep A pointer to a previously-thrown exception.
/// \param outER On successful return, the Windows structured exception
///   record that was thrown. On failure, unspecified.
///
/// \returns Whether or not \a ep represents a Windows structured exception.
///
/// The (proposed) Swift runtime convention for handling Windows structured
/// exceptions is to install a translator function before an exception might be
/// thrown, then to rethrow the \c EXCEPTION_RECORD structure from within that
/// function. If a Windows structured exception is thrown via another mechanism,
/// this function will not be able to detect it.
///
/// \sa _set_se_translator()
/// \sa https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/set-se-translator
bool
_swift_exceptionPointerGetThrownExceptionRecord(const std::exception_ptr *ep,
                                                EXCEPTION_RECORD *outER);
#endif

#pragma mark - Stringifying exceptions

/// Get a description of a C++ exception pointer's underlying exception type.
///
/// \param ep A pointer to a previously-thrown exception.
///
/// \returns A null-terminated C string containing a description of the
///   exception's type (such as \c "NSException" or \c "std::exception". The
///   caller is responsible for freeing this string with \c swift_slowDealloc()
///   when done with it. If no type information was available, returns
///   \c nullptr.
///
/// The resulting string may or may not be demangled depending on the
/// capabilities of the current system.
char *_swift_exceptionPointerCopyTypeName(const std::exception_ptr *ep);

/// Get a description of a C++ exception pointer.
///
/// \param ep A pointer to a previously-thrown exception.
/// \param debug Whether or not to copy a debug description containing more
///   information (when available.) The format of an exception's debug
///   description is subject to change.
///
/// \returns A null-terminated C string containing a description of the
///   exception. The caller is responsible for freeing this string with
///   \c swift_slowDealloc() when done with it. If no description was available,
///   returns \c nullptr.
char *_swift_exceptionPointerCopyDescription(const std::exception_ptr *ep,
                                             bool debug);

#pragma mark - Catching exceptions in Swift

/// Create a \c CaughtException instance from the current thrown-and-caught
/// exception.
///
/// \returns A pointer to a newly-allocated instance of \c CaughtException that
///   can be assigned to the \c swift_error_result -annotated parameter of a C
///   function that uses the Swift calling convention. If there is no current
///   exception, this function returns \c nullptr.
///
/// This function is used by the Swift runtime and standard library to convert a
/// caught exception to an \c Error. C++ callers outside the runtime or standard
/// library might also call it with the following signature:
///
/// \code
/// namespace swift {
///   extern "C" void *_Nullable swift_caughtException_copyCurrent(void);
/// }
/// \endcode
///
/// Swift callers should prefer \c CaughtException.current.
///
/// \warning Swift support for exception handling is experimental. It remains
///   undefined behavior to throw an exception through a Swift stack frame.
SWIFT_EXPORT_FROM(swift_ExceptionHandling)
void *swift_caughtException_copyCurrent(void);

/// Invoke a Swift function or closure in a context that can catch Objective-C
/// and C++ exceptions.
///
/// \param body A Swift function or closure to invoke.
/// \param bodyContext The context pointer for \a body.
/// \param context The context pointer for the function. Ignored.
/// \param outError The Swift error pointer for errors thrown from \a body.
///
/// \throws Whatever Swift error is thrown by \a body. Exceptions are thrown as
///   instances of the Swift \c CaughtException type.
///
/// \warning Swift support for exception handling is experimental. It remains
///   undefined behavior to throw an exception through a Swift stack frame.
SWIFT_CC(swift) void _swift_withUnsafeExceptionHandling(
  SWIFT_CC(swift) void (* body)(
    SWIFT_CONTEXT void *context,
    SWIFT_ERROR_RESULT void **outError),
  void *bodyContext,
  SWIFT_CONTEXT void *context,
  SWIFT_ERROR_RESULT void **outError);

}
}

#endif
