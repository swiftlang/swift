//===--- Exception.cpp - Exception support --------------------------------===//
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
// Swift doesn't support exception handlers, but might call code that uses
// exceptions, and when they leak out into Swift code, we want to trap them.
//
// To that end, we have our own exception personality routine, which we use
// to trap exceptions and terminate.
//
//===----------------------------------------------------------------------===//

#if defined(__ELF__) || defined(__APPLE__)

#include <exception>

#include <cstdio>

#include <unwind.h>

#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Exception.h"

using namespace swift;

extern "C" void *__cxa_begin_catch(void *);

SWIFT_RUNTIME_STDLIB_API _Unwind_Reason_Code
_swift_exceptionPersonality(int version,
                            _Unwind_Action actions,
                            uint64_t exceptionClass,
                            struct _Unwind_Exception *exceptionObject,
                            struct _Unwind_Context *context)
{
#if __cpp_exceptions
  // Handle exceptions by catching them and calling std::terminate().
  // This, in turn, will trigger the unhandled exception routine in the
  // C++ runtime.
  __cxa_begin_catch(exceptionObject);
  std::terminate();
#else
  fatalError(0,
             "C++ exception handling detected but the Swift runtime was "
             "compiled with exceptions disabled\n");
#endif

  return _URC_FATAL_PHASE1_ERROR;
}

#endif /* defined(__ELF__) || defined(__APPLE__) */
