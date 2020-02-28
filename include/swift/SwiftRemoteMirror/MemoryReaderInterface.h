//===--- MemoryReaderInterface.h - Public reader interface ------*- C++ -*-===//
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
///
/// \file
/// This header declares the MemoryReader interface struct, which is a
/// a collection of function pointers to provide reading memory from external
/// processes.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_MEMORYREADERINTERFACE_H
#define SWIFT_REFLECTION_MEMORYREADERINTERFACE_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// They would think the type 'addr_t' is defined in the standard library
// because it has the same name format with the types in <cstdint>. In
// addition, the definition conflicts in Cygwin which defines it differently
// in the system library, so we use 'swift_addr_t'.
typedef uint64_t swift_addr_t;

/// The following callbacks provide "reader_context", which is an opaque context
/// provided in swift_reflection_createReflectionContext.

/// Free memory returned from readBytes. May be NULL if memory never needs to be freed.
typedef void (*FreeBytesFunction)(void *reader_context, const void *bytes, void *context);

/// Get the size in bytes of the target's pointer type.
typedef uint8_t (*PointerSizeFunction)(void *reader_context);

/// Get the size in bytes of the target's size type.
typedef uint8_t (*SizeSizeFunction)(void *reader_context);

/// Read a sequence of bytes at an address in the target.
///
/// \param address the address in the target address space
/// \param size the number of bytes to read
/// \param outFreeContext on return, an arbitrary context pointer that the caller will
///                       pass to the free function
/// \returns A pointer to the requested memory, or NULL if the memory could not be read.
///          The caller must invoke the free function on the returned pointer once it's
///          done using the memory.
typedef const void *(*ReadBytesFunction)(void *reader_context, swift_addr_t address,
                                         uint64_t size,
                                         void **outFreeContext);

/// Get the string length at the given address.
///
/// This scan always occurs in a read-only data section. If the scan
/// would go beyond the section boundary, a length of 0 should be
/// returned.
///
/// \param address the address in the target address space
/// \returns The length of the string or 0 if the scan was unsuccessful.
typedef uint64_t (*GetStringLengthFunction)(void *reader_context,
                                            swift_addr_t address);

/// Get the address of a symbol in the target address space.
///
/// \returns true if the lookup was successful.
typedef swift_addr_t (*GetSymbolAddressFunction)(void *reader_context,
                                                 const char *name,
                                                 uint64_t name_length);

typedef enum {
  /// The query should ignore inBuffer, and treat outBuffer as uint8_t* which
  /// should be populated with the size of an ordinary pointer in the remote
  /// process, in bytes.
  DLQ_GetPointerSize,

  /// The query should ignore inBuffer, and treat outBuffer as uint8_t* which
  /// should be populated with the size of size_t in the remote process, in
  /// bytes.
  DLQ_GetSizeSize,

  /// The query should ignore inBuffer, and treat outBuffer as pointer-sized
  /// buffer (the size of a target pointer, not a swift_addr_t) which should be
  /// populated with the mask of pointer addressable bits.
  DLQ_GetPtrAuthMask,
} DataLayoutQueryType;

/// Data layout query function, which returns answers based on query types (from
/// the DataLayoutQueryType enum). The meaning of the input and output buffer
/// is defined by the type of the query. Unknown requests should be handled by
/// returning 0.
///
/// \returns 0 on error, non-zero on success.
typedef int (*QueryDataLayoutFunction)(void *reader_context,
                                       DataLayoutQueryType type, void *inBuffer,
                                       void *outBuffer);

#ifdef __cplusplus
} // extern "C"
#endif

#endif //SWIFT_REFLECTION_MEMORYREADERINTERFACE_H
