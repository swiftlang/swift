//===--- Exception.h - Exception support ------------------------*- C++ -*-===//
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

#ifndef SWIFT_RUNTIME_EXCEPTION_H
#define SWIFT_RUNTIME_EXCEPTION_H

#include "swift/Runtime/Config.h"

#if defined(__ELF__) || defined(__APPLE__)
#include <unwind.h>

namespace swift {

SWIFT_RUNTIME_STDLIB_API _Unwind_Reason_Code
swift_exceptionPersonality(int version,
                           _Unwind_Action actions,
                           uint64_t exceptionClass,
                           struct _Unwind_Exception *exceptionObject,
                           struct _Unwind_Context *context);

} // end namespace swift

#endif // defined(__ELF__) || defined(__APPLE__)

#endif // SWIFT_RUNTIME_EXCEPTION_H
