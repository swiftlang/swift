//===--- MemoryReaderInterface.h - Public reader interface ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

typedef uint64_t addr_t;

typedef struct MemoryReaderImpl {
  /// Get the size in bytes of the target's pointer type.
  uint8_t (*getPointerSize)();

  /// Get the size in bytes of the target's size type.
  uint8_t (*getSizeSize)();

  // FIXME: -Wdocumentation complains about \param and \returns on function pointers.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdocumentation"

  /// Read a sequence of bytes at an address in the target.
  ///
  /// \param address the address in the target address space
  /// \param dest the caller-owned buffer into which to store the string
  /// \param size the number of bytes to read
  /// \returns true if the read was successful
  bool (*readBytes)(addr_t address, uint8_t *dest, uint64_t size);

  /// Read an integer scalar from the target.
  ///
  /// \parameter address the address in the target address space
  /// \param value the local address to store the result of the read
  /// \param size the size in bytes of the integer type
  /// \returns true if the read was successful.
  bool (*readInteger)(addr_t address, uint64_t *value, uint8_t size);

  /// Get the string length at the given address.
  ///
  /// This scan always occurs in a read-only data section. If the scan
  /// would go beyond the section boundary, a length of 0 should be
  /// returned.
  ///
  /// \param address the address in the target address space
  /// \returns The length of the string or 0 if the scan was unsuccessful.
  uint64_t (*getStringLength)(addr_t address);

#pragma clang diagnostic pop

} MemoryReaderImpl;

#ifdef __cplusplus
} // extern "C"
#endif

#endif //SWIFT_REFLECTION_MEMORYREADERINTERFACE_H
