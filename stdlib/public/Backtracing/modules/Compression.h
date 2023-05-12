//===--- Compression.h - C decls for compression libraries ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Includes and definitions to allow us to use the compression libraries
// (zlib, zstd and liblzma) in the backtracing module.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BACKTRACING_COMPRESSION_H
#define SWIFT_BACKTRACING_COMPRESSION_H

#include "zlib.h"
#include "zstd.h"
#include "lzma.h"

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

// The Swift importer can't cope with complex macros; it will do inline
// functions, however.
static inline lzma_stream lzma_stream_init() { return LZMA_STREAM_INIT; }

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif // SWIFT_BACKTRACING_COMPRESSION_H
