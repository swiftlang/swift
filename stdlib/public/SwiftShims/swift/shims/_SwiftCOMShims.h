//===--- _SwiftCOMShims.h - Swift COM ABI structures ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// C-compatible declarations for compiler-emitted COM metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_COM_SHIMS_H
#define SWIFT_STDLIB_COM_SHIMS_H

#include "SwiftStdint.h"

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

/// The header of a compiler-emitted COM interface map.
///
/// The entries immediately follow this header.
typedef struct _SwiftCOMInterfaceMapHeader {
  __swift_uint32_t count;
  __swift_uint32_t reserved;
} _SwiftCOMInterfaceMapHeader;

/// An entry in a compiler-emitted COM interface map.
///
/// `descriptor` is relative to the address of this field. Its low bit
/// indicates that the resolved address contains an indirect reference to the
/// protocol descriptor.
///
/// `index` identifies the physical interface address point in the native
/// object's prefix. Projection zero is closest to the Swift object.
typedef struct _SwiftCOMInterfaceMapEntry {
  __swift_int32_t descriptor;
  __swift_uint32_t index;
} _SwiftCOMInterfaceMapEntry;

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif // SWIFT_STDLIB_COM_SHIMS_H
