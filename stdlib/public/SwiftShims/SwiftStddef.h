//===--- SwiftStddef.h ------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_SWIFT_STDDEF_H
#define SWIFT_STDLIB_SHIMS_SWIFT_STDDEF_H

// stddef.h is provided by Clang, but it dispatches to libc's stddef.h.  As a
// result, using stddef.h here would pull in Darwin module (which includes
// libc). This creates a dependency cycle, so we can't use stddef.h in
// SwiftShims.
// On Linux, the story is different. We get the error message
// "/usr/include/x86_64-linux-gnu/sys/types.h:146:10: error: 'stddef.h' file not
// found"
// This is a known Clang/Ubuntu bug.
#if !defined(__APPLE__) && !defined(__linux__)
#include <stddef.h>
typedef size_t __swift_size_t;
#else
typedef __SIZE_TYPE__ __swift_size_t;
#endif

// This declaration is not universally correct.  We verify its correctness for
// the current platform in the runtime code.
#if defined(__linux__) && (defined(__arm__) || defined(__i386__))
typedef           int __swift_ssize_t;
#elif defined(_WIN32)
#if defined(_M_ARM) || defined(_M_IX86)
typedef           int __swift_ssize_t;
#elif defined(_M_X64) || defined(_M_ARM64)
typedef long long int __swift_ssize_t;
#else
#error unsupported machine type
#endif
#else
typedef      long int __swift_ssize_t;
#endif

#endif // SWIFT_STDLIB_SHIMS_SWIFT_STDDEF_H
