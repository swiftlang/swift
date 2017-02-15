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
#if !defined(__APPLE__)
#include <stddef.h>
typedef size_t __swift_size_t;
#else
typedef __SIZE_TYPE__ __swift_size_t;
#endif

#endif // SWIFT_STDLIB_SHIMS_SWIFT_STDDEF_H
