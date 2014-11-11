//===--- SwiftStdint.h ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_SWIFT_STDINT_H
#define SWIFT_STDLIB_SHIMS_SWIFT_STDINT_H

// stdint.h is provided by Clang, but it dispatches to libc's stdint.h.  As a
// result, using stdint.h here would pull in Darwin module (which includes
// libc).  This creates a dependency cycle, so we can't use stdint.h in
// SwiftShims.

// Clang has been defining __INTxx_TYPE__ macros for a long time.
// __UINTxx_TYPE__ are defined only since Clang 3.5.

typedef __INT64_TYPE__ __swift_int64_t;
#ifdef __UINT64_TYPE__
typedef __UINT64_TYPE__ __swift_uint64_t;
#else
typedef unsigned __INT64_TYPE__ __swift_uint64_t;
#endif

typedef __INT32_TYPE__ __swift_int32_t;
#ifdef __UINT32_TYPE__
typedef __UINT32_TYPE__ __swift_uint32_t;
#else
typedef unsigned __INT32_TYPE__ __swift_uint32_t;
#endif

typedef __INT16_TYPE__ __swift_int16_t;
#ifdef __UINT16_TYPE__
typedef __UINT16_TYPE__ __swift_uint16_t;
#else
typedef unsigned __INT16_TYPE__ __swift_uint16_t;
#endif

typedef __INT8_TYPE__ __swift_int8_t;
#ifdef __UINT8_TYPE__
typedef __UINT8_TYPE__ __swift_uint8_t;
#else
typedef unsigned __INT8_TYPE__ __swift_uint8_t;
#endif

#define __swift_join3(a,b,c) a ## b ## c

#define __swift_intn_t(n) __swift_join3(__swift_int, n, _t)
#define __swift_uintn_t(n) __swift_join3(__swift_uint, n, _t)

typedef __swift_intn_t(__INTPTR_WIDTH__) __swift_intptr_t;
typedef __swift_uintn_t(__INTPTR_WIDTH__) __swift_uintptr_t;

#endif // SWIFT_STDLIB_SHIMS_SWIFT_STDINT_H

