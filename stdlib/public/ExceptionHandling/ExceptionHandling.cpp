//===--- ExceptionHandling.cpp --------------------------------------------===//
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

#include "ExceptionHandling.h"

#include <cstring>
#include <memory>

#if __has_include(<cxxabi.h>)
#include <cxxabi.h>
#endif

#if defined(_MSC_VER)
#include <ehdata.h>
#endif

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Portability.h"
#include "../runtime/ImageInspection.h"

// NOTE: swift_slowDealloc() calls free() (or on Apple, an equivalent function)
// when alignment is small, so it should be okay to treat malloc()-allocated
// C strings as safe to pass to swift_slowDealloc().

std::exception_ptr *swift::_swift_exceptionPointerCopyCurrent(void) {
  if (auto ep = std::current_exception()) {
    return _swift_exceptionPointerCopy(&ep);
  }
  return nullptr;
}

std::exception_ptr *
swift::_swift_exceptionPointerCopy(const std::exception_ptr *ep) {
  if (_swift_exceptionPointerIsNil(ep)) {
    return nullptr;
  }

  // Avoid using operator new() here because the exception might be
  // related to an operator new() failure. Use good ol' slowAlloc and
  // placement new instead.
  auto result = reinterpret_cast<std::exception_ptr *>(
    swift_slowAlloc(sizeof(std::exception_ptr),
                    alignof(std::exception_ptr) - 1)
  );
  if (result) {
    new (result) std::exception_ptr(*ep);
  }

  return result;
}

void swift::_swift_exceptionPointerDelete(std::exception_ptr *ep) {
  // As with _swift_exceptionPointerCopyCurrent(), avoid using delete here.
  // Directly call the destructor and then deallocate the exception pointer
  // manually.
  if (!ep) {
    return;
  }

  ep->~exception_ptr();
  swift_slowDealloc(ep, sizeof(std::exception_ptr),
                    alignof(std::exception_ptr) - 1);
}

bool swift::_swift_exceptionPointerIsNil(const std::exception_ptr *ep) {
  return (!ep || !*ep);
}

#pragma mark - Converting to other exception types

#if defined(_WIN32)
bool swift::_swift_exceptionPointerGetThrownExceptionRecord(
  const std::exception_ptr *ep, EXCEPTION_RECORD *outER) {
  if (_swift_exceptionPointerIsNil(ep)) {
    return false;
  }

  try {
    std::rethrow_exception(*ep);
  } catch (const EXCEPTION_RECORD& exceptionRecord) {
    *outER = exceptionRecord;
    return true;
  } catch (...) {
    // Not a Windows structured exception.
  }

  return false;
}
#endif

#pragma mark - Stringifying exceptions

namespace swift {
/// Get the mangled name of the specified exception's type, if possible.
///
/// \param ep A pointer to a previously-thrown exception.
///
/// \returns The mangled name of the thrown exception's type, or \c nullptr if
///   it could not be determined. The caller should not free the resulting
///   pointer. It is owned by the system.
static const char *
_swift_exceptionPointerGetMangledTypeName(const std::exception_ptr *ep);

/// Create a demangled copy of the specified mangled type name.
///
/// \param mangledName The type name to demangle.
///
/// \returns A demangled copy of \a mangledName, or \c nullptr if it could not
///   be demangled. The caller is responsible for freeing the resulting pointer
///   with \c swift_slowDealloc() or \c free().
static char *
_swift_exceptionPointerCopyDemangledTypeName(const char *mangledName);

#if SWIFT_OBJC_INTEROP
/// Copy the description (from \c -description or \c -debugDescription) of a
/// previously-thrown Objective-C object.
///
/// \param obj A previously-thrown Objective-C object (as returned from
///   \c _swift_exceptionPointerGetThrownObjectiveCObject().)
/// \param debug Whether to use \c -debugDescription instead of \c -description.
///
/// \returns A null-terminated C string containing a description of the
///   thrown object. The caller is responsible for freeing this string with
///   \c swift_slowDealloc() when done with it. If no description was available,
///   returns \c nullptr.
///
/// \note This function is implemented in ExceptionHandling.mm.
extern char *
_swift_exceptionPointerCopyObjectiveCExceptionDescription(id obj, bool debug);
#endif
}

static const char *
swift::_swift_exceptionPointerGetMangledTypeName(const std::exception_ptr *ep) {
  if (_swift_exceptionPointerIsNil(ep)) {
    return nullptr;
  }

#if __has_include(<cxxabi.h>)
  // Let the C++ ABI figure out the type for all non-Objective-C exceptions.
  if (auto typeInfo = __cxxabiv1::__cxa_current_exception_type()) {
    return typeInfo->name();
  }

#elif defined(_MSC_VER)
  // Under the MSVC++ ABI, std::exception_ptr is secretly just a
  // std::shared_ptr<EXCEPTION_RECORD>. EHExceptionRecord is a layout-compatible
  // type that is used in the STL implementation and comes with macros for
  // discovering the underlying exception's mangled/decorated type name.
  //
  // For more information about std::exception_ptr as implemented by MSVC++,
  // see: https://github.com/microsoft/STL/blob/main/stl/src/excptptr.cpp
  auto *er = reinterpret_cast<const std::shared_ptr<EHExceptionRecord> *>(ep);
  if (PER_IS_MSVC_PURE_OR_NATIVE_EH(er)) {
    if (auto throwInfo = PER_PTHROW(er)) {
      if (THROW_COUNT(*throwInfo) > 0) {
        if (auto type = THROW_PCT(*throwInfo, 0)) {
          return CT_NAME(*type);
        }
      }
    }
  }

#else
  // Unsupported by this EH ABI, but we might still be able to get the
  // type name if the exception is a std::exception or of a subclass thereof.
  try {
    std::rethrow_exception(*ep);
  } catch (const std::exception& e) {
    return typeid(e).name();
  } catch (...) {
    // The exception was of some other type and this ABI does not allow us to
    // inspect any further. Stop.
  }
#endif

  return nullptr;
}

static char *
swift::_swift_exceptionPointerCopyDemangledTypeName(const char *mangledName) {
  if (!mangledName) {
    return nullptr;
  }

#if __has_include(<cxxabi.h>)
  int status = 0;
  return abi::__cxa_demangle(mangledName, nullptr, nullptr, &status);

#elif defined(_MSC_VER)
  return _swift_withWin32DbgHelpLibrary([=] (bool isInitialized) -> char * {
    if (!isInitialized) {
      return nullptr;
    }

    // std::type_info::raw_name() has a leading period that interferes with
    // demangling. Strip it off if found.
    if (mangledName[0] == '.') {
      mangledName += 1;
    }

    // Allocate some space for the demangled type name.
    static const constexpr size_t MAX_DEMANGLED_NAME_SIZE = 1024;
    auto demangledName = reinterpret_cast<char *>(
      swift_slowAlloc(MAX_DEMANGLED_NAME_SIZE), 0);
    if (demangledName) {

      // Call into DbgHelp to perform the demangling. These flags should give us
      // the correct demangling of a mangled C++ type name.
      DWORD undecorateFlags = UNDNAME_NAME_ONLY | UNDNAME_NO_ARGUMENTS;
  #if !defined(_WIN64)
      undecorateFlags |= UNDNAME_32_BIT_DECODE;
  #endif
      if (UnDecorateSymbolName(mangledName, demangledName,
                               MAX_DEMANGLED_NAME_SIZE, undecorateFlags)) {
        return demangledName;
      }

      // Don't leak the allocated buffer if demangling failed.
      swift_slowDealloc(demangledName, 0);
    }

    return nullptr;
  });
#else
  // Unsupported on this platform.
  return nullptr;
#endif
}

char *swift::_swift_exceptionPointerCopyTypeName(const std::exception_ptr *ep) {
  if (_swift_exceptionPointerIsNil(ep)) {
    return nullptr;
  }

#if SWIFT_OBJC_INTEROP
  if (id obj = _swift_exceptionPointerGetThrownObjectiveCObject(ep)) {
    return strdup(object_getClassName(obj));
  }
#endif

#if defined(_WIN32)
  EXCEPTION_RECORD exceptionRecord;
  if (_swift_exceptionPointerGetThrownExceptionRecord(ep, &exceptionRecord)) {
    // std::type_info::name() is demangled by default on Windows.
    return typeid(EXCEPTION_RECORD).name();
  }
#endif

  if (auto typeName = _swift_exceptionPointerGetMangledTypeName(ep)) {
    auto result = _swift_exceptionPointerCopyDemangledTypeName(typeName);
    if (!result) {
      // Couldn't demangle the type name. Use the mangled name instead.
      result = strdup(typeName);
    }
    return result;
  }

  return nullptr;
}

char *
swift::_swift_exceptionPointerCopyDescription(const std::exception_ptr *ep,
                                              bool debug) {
  if (_swift_exceptionPointerIsNil(ep)) {
    return nullptr;
  }

#if SWIFT_OBJC_INTEROP
  if (id obj = _swift_exceptionPointerGetThrownObjectiveCObject(ep)) {
    return _swift_exceptionPointerCopyObjectiveCExceptionDescription(obj,
                                                                     debug);
  }
#endif

#if defined(_WIN32)
  EXCEPTION_RECORD exceptionRecord;
  if (_swift_exceptionPointerGetThrownExceptionRecord(ep, &exceptionRecord)) {
    // TODO: OS-supplied descriptions of specific exception codes, if possible.
    char *result = nullptr;
    swift_asprintf(&result, "An error (exception code %u) occurred.",
                   exceptionRecord.ExceptionCode);
    return result;
  }
#endif

  try {
    std::rethrow_exception(*ep);
  } catch (const std::exception& e) {
    if (auto what = e.what()) {
      return strdup(what);
    }
  } catch (...) {
    // Not an exception we understand, so not something we can describe.
  }

  return nullptr;
}

#pragma mark - Catching exceptions in Swift

namespace swift {
/// Call out to a Swift function that might throw an exception while leaving a
/// breadcrumb in any resultant stack traces.
///
/// This function serves two purposes:
///
/// 1. It adds a "noisy" symbol to stack traces in crash logs that can be
///    useful in diagnosing issues; and
///
/// 2. It works around an apparent clang bug rdar://87360634 where functions
///    marked __attribute__((swiftcall)) do not generate correct exception
///    handling metadata (even if they are implemented in C++.)
SWIFT_NOINLINE SWIFT_DISABLE_TAIL_CALLS extern "C" void
__SWIFT_IS_CALLING_A_FUNCTION_THAT_MAY_THROW_EXCEPTIONS__(
  SWIFT_CC(swift) void (* body)(
    SWIFT_CONTEXT void *context,
    SWIFT_ERROR_RESULT void **outError),
  void *bodyContext,
  void **outError);

void __SWIFT_IS_CALLING_A_FUNCTION_THAT_MAY_THROW_EXCEPTIONS__(
  SWIFT_CC(swift) void (* body)(
    SWIFT_CONTEXT void *context,
    SWIFT_ERROR_RESULT void **outError),
  void *bodyContext,
  void **outError) {
  (* body)(bodyContext, outError);
}
}

SWIFT_CC(swift) void swift::_swift_withUnsafeExceptionHandling(
  SWIFT_CC(swift) void (* body)(
    SWIFT_CONTEXT void *context,
    SWIFT_ERROR_RESULT void **outError),
  void *bodyContext,
  SWIFT_CONTEXT void *context,
  SWIFT_ERROR_RESULT void **outError) {

#if defined(_WIN32)
  auto oldSEHTranslator = _set_se_translator([] (unsigned int u,
                                                 _EXCEPTION_POINTERS *pExp) {
    throw (* pExp->ExceptionRecord);
  });
#endif

  try {
    // Invoke the closure. Any thrown Swift error will be placed in
    // *outError and the caller will detect it and rethrow it.
    __SWIFT_IS_CALLING_A_FUNCTION_THAT_MAY_THROW_EXCEPTIONS__(body, bodyContext,
                                                              outError);

  } catch (...) {
    *outError = swift::swift_caughtException_copyCurrent();
  }

#if defined(_WIN32)
  _set_se_translator(oldSEHTranslator);
#endif
}

