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
//
// On Linux, the story is different. We get the error message
// "/usr/include/x86_64-linux-gnu/sys/types.h:146:10: error: 'stddef.h' file not
// found"
// This is a known Clang/Ubuntu bug.
//
// On Windows, the complicated setup between clang and MSVC causes a cicular
// dependency between `ucrt` and `SwiftShims`, preventing a successful build of
// the module.
//
// Opt to use teh compiler vended type whenever possible.
#if defined(__clang__)
typedef __SIZE_TYPE__ __swift_size_t;
#else
#include <stddef.h>
typedef size_t __swift_size_t;
#endif

// This selects the signed equivalent of the unsigned type chosen for size_t.
#if __STDC_VERSION__-0 >= 201112l || defined(__swift__)
typedef __typeof__(_Generic((__swift_size_t)0,                                 \
                            unsigned long long int : (long long int)0,         \
                            unsigned long int : (long int)0,                   \
                            unsigned int : (int)0,                             \
                            unsigned short : (short)0,                         \
                            unsigned char : (signed char)0)) __swift_ssize_t;
#elif defined(__cplusplus)
#include <type_traits>
using __swift_ssize_t = std::make_signed<__swift_size_t>::type;
#else
#error "do not have __swift_ssize_t defined"
#endif

#endif // SWIFT_STDLIB_SHIMS_SWIFT_STDDEF_H
