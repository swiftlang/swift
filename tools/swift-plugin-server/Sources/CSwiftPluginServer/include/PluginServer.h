//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PLUGINSERVER_PLUGINSERVER_H
#define SWIFT_PLUGINSERVER_PLUGINSERVER_H

// NOTE: Partially ported from SwiftShim's SwiftStdint.h. We cannot include
// that header here because it belongs to the runtime, but we need the same
// logic for interoperability with Swift code in the compiler itself.
// stdint.h is provided by Clang, but it dispatches to libc's stdint.h.  As a
// result, using stdint.h here would pull in Darwin module (which includes
// libc). This creates a dependency cycle, so we can't use stdint.h in
// SwiftShims.
// On Linux, the story is different. We get the error message
// "/usr/include/x86_64-linux-gnu/sys/types.h:146:10: error: 'stddef.h' file not
// found"
// This is a known Clang/Ubuntu bug.

// Clang has been defining __INTxx_TYPE__ macros for a long time.
// __UINTxx_TYPE__ are defined only since Clang 3.5.
#if defined(_MSC_VER) && !defined(__clang__)
typedef __int64 __swiftc_int64_t;
typedef unsigned __int64 __swiftc_uint64_t;
typedef int __swiftc_int32_t;
typedef unsigned int __swiftc_uint32_t;
#elif !defined(__APPLE__) && !defined(__linux__) && !defined(__OpenBSD__) && !defined(__wasi__)
#include <stdint.h>
typedef int64_t __swiftc_int64_t;
typedef uint64_t __swiftc_uint64_t;
typedef int32_t __swiftc_int32_t;
typedef uint32_t __swiftc_uint32_t;
typedef intptr_t __swiftc_intptr_t;
typedef uintptr_t __swiftc_uintptr_t;
#else
typedef __INT64_TYPE__ __swiftc_int64_t;
#ifdef __UINT64_TYPE__
typedef __UINT64_TYPE__ __swiftc_uint64_t;
#else
typedef unsigned __INT64_TYPE__ __swiftc_uint64_t;
#endif

typedef __INT32_TYPE__ __swiftc_int32_t;
#ifdef __UINT32_TYPE__
typedef __UINT32_TYPE__ __swiftc_uint32_t;
#else
typedef unsigned __INT32_TYPE__ __swiftc_uint32_t;
#endif
#endif

#define __swiftc_join3(a,b,c) a ## b ## c

#define __swiftc_intn_t(n) __swiftc_join3(__swiftc_int, n, _t)
#define __swiftc_uintn_t(n) __swiftc_join3(__swiftc_uint, n, _t)

#if defined(_MSC_VER) && !defined(__clang__)
#if defined(_WIN64)
typedef __swiftc_int64_t SwiftInt;
typedef __swiftc_uint64_t SwiftUInt;
#elif defined(_WIN32)
typedef __swiftc_int32_t SwiftInt;
typedef __swiftc_uint32_t SwiftUInt;
#else
#error unknown windows pointer width
#endif
#else
typedef __swiftc_intn_t(__INTPTR_WIDTH__) SwiftInt;
typedef __swiftc_uintn_t(__INTPTR_WIDTH__) SwiftUInt;
#endif

#ifdef __cplusplus
extern "C" {
#endif

//===----------------------------------------------------------------------===//
// Inter-process communication.
//===----------------------------------------------------------------------===//

/// Create an IPC communication handle.
const void *PluginServer_createConnection(const char **errorMessage);

/// Destroy an IPC communication handle created by
/// 'PluginServer_createConnection'.
void PluginServer_destroyConnection(const void *connHandle);

/// Read bytes from the IPC communication handle.
SwiftInt PluginServer_read(const void *connHandle, void *data, SwiftUInt nbyte);

/// Write bytes to the IPC communication handle.
SwiftInt PluginServer_write(const void *connHandle, const void *data, SwiftUInt nbyte);

//===----------------------------------------------------------------------===//
// Dynamic link
//===----------------------------------------------------------------------===//

/// Load a dynamic link library, and return the handle.
void *PluginServer_load(const char *filename, const char **errorMessage);

/// Resolve a type metadata by a pair of the module name and the type name.
/// 'libraryHint' is a
const void *PluginServer_lookupMacroTypeMetadataByExternalName(
    const char *moduleName, const char *typeName, void *libraryHint,
    const char **errorMessage);

#ifdef __cplusplus
}
#endif

#endif /* SWIFT_PLUGINSERVER_PLUGINSERVER_H */
