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

typedef uint8_t (*PointerSizeFunction)(void *reader_context);
typedef uint8_t (*SizeSizeFunction)(void *reader_context);
typedef int (*ReadBytesFunction)(void *reader_context, swift_addr_t address,
                                 void *dest, uint64_t size);
typedef uint64_t (*GetStringLengthFunction)(void *reader_context,
                                            swift_addr_t address);
typedef swift_addr_t (*GetSymbolAddressFunction)(void *reader_context,
                                                 const char *name,
                                                 uint64_t name_length);

typedef struct MemoryReaderImpl {
  /// An opaque context that the implementor can specify to
  /// be passed to each of the APIs below.
  void *reader_context;

  /// Get the size in bytes of the target's pointer type.
  PointerSizeFunction getPointerSize;

  /// Get the size in bytes of the target's size type.
  SizeSizeFunction getSizeSize;

  // FIXME: -Wdocumentation complains about \param and \returns on function pointers.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdocumentation"

  /// Read a sequence of bytes at an address in the target.
  ///
  /// \param address the address in the target address space
  /// \param dest the caller-owned buffer into which to store the string
  /// \param size the number of bytes to read
  /// \returns true if the read was successful
  ReadBytesFunction readBytes;

  /// Get the string length at the given address.
  ///
  /// This scan always occurs in a read-only data section. If the scan
  /// would go beyond the section boundary, a length of 0 should be
  /// returned.
  ///
  /// \param address the address in the target address space
  /// \returns The length of the string or 0 if the scan was unsuccessful.
  GetStringLengthFunction getStringLength;

  /// Get the address of a symbol in the target address space.
  ///
  /// \returns true if the lookup was successful.
  GetSymbolAddressFunction getSymbolAddress;

#pragma clang diagnostic pop

} MemoryReaderImpl;

#ifdef __cplusplus
} // extern "C"
#endif

#endif //SWIFT_REFLECTION_MEMORYREADERINTERFACE_H
