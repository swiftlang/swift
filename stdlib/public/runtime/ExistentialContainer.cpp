//===--- ExistentialContainer.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/ExistentialContainer.h"
#include "swift/Runtime/HeapObject.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                 OpaqueExistentialContainer Implementation
//===----------------------------------------------------------------------===//

template <>
bool OpaqueExistentialContainer::isValueInline() const {
  return Type->getValueWitnesses()->isValueInline();
}

template <>
const OpaqueValue *OpaqueExistentialContainer::projectValue() const {
  auto *vwt = Type->getValueWitnesses();

  if (vwt->isValueInline())
    return reinterpret_cast<const OpaqueValue *>(&Buffer);

  // Compute the byte offset of the object in the box.
  unsigned alignMask = vwt->getAlignmentMask();
  unsigned byteOffset = (sizeof(HeapObject) + alignMask) & ~alignMask;
  auto *bytePtr = reinterpret_cast<const char *>(
      *reinterpret_cast<HeapObject *const *const>(&Buffer));
  return reinterpret_cast<const OpaqueValue *>(bytePtr + byteOffset);
}

template <>
void OpaqueExistentialContainer::deinit() {
  auto *vwt = Type->getValueWitnesses();
  if (vwt->isValueInline()) {
    return;
  }

  unsigned alignMask = vwt->getAlignmentMask();
  unsigned size = vwt->size;
  swift_deallocObject(*reinterpret_cast<HeapObject **>(&Buffer), size,
                      alignMask);
}

#ifndef NDEBUG

// *NOTE* This routine performs unused memory reads on purpose to try to catch
// use-after-frees in conjunction with ASAN or Guard Malloc.
template <> SWIFT_USED void OpaqueExistentialContainer::verify() const {
  // We do not actually care about value. We just want to see if the
  // memory is valid or not. So convert to a uint8_t and try to
  // memcpy into firstByte. We use volatile to just ensure that this
  // does not get dead code eliminated.
  uint8_t firstByte;
  memcpy(&firstByte, projectValue(), 1);
  volatile uint8_t firstVolatileByte = firstByte;
  (void)firstVolatileByte;
}

/// Dump information about this specific container and its contents.
template <> SWIFT_USED void OpaqueExistentialContainer::dump() const {
  // Quickly verify to make sure we are well formed.
  verify();

  printf("TargetOpaqueExistentialContainer.\n");
  printf("Metadata Pointer: %p.\n", Type);
  printf("Value Pointer: %p.\n", projectValue());
  printf("Is Value Stored Inline: %s.\n", isValueInline() ? "true" : "false");
}

#endif
