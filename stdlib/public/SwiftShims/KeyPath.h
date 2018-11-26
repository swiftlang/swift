//===--- KeyPath.h - ABI constants for key path objects ---------*- C++ -*-===//
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
//
//  Constants used in the layout of key path objects.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_SHIMS_KEYPATH_H__
#define __SWIFT_SHIMS_KEYPATH_H__

#include "SwiftStdint.h"

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

// Bitfields for the key path buffer header.

static const __swift_uint32_t _SwiftKeyPathBufferHeader_SizeMask
  = 0x00FFFFFFU;
static const __swift_uint32_t _SwiftKeyPathBufferHeader_TrivialFlag
  = 0x80000000U;
static const __swift_uint32_t _SwiftKeyPathBufferHeader_HasReferencePrefixFlag
  = 0x40000000U;
static const __swift_uint32_t _SwiftKeyPathBufferHeader_ReservedMask
  = 0x3F000000U;
  
// Bitfields for a key path component header.

static const __swift_uint32_t _SwiftKeyPathComponentHeader_PayloadMask
  = 0x00FFFFFFU;

static const __swift_uint32_t _SwiftKeyPathComponentHeader_DiscriminatorMask
  = 0x7F000000U;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_DiscriminatorShift
  = 24;

static const __swift_uint32_t _SwiftKeyPathComponentHeader_StructTag
  = 1;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedTag
  = 2;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ClassTag
  = 3;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_OptionalTag
  = 4;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ExternalTag
  = 0;

static const __swift_uint32_t
_SwiftKeyPathComponentHeader_TrivialPropertyDescriptorMarker = 0U;

static const __swift_uint32_t _SwiftKeyPathComponentHeader_StoredOffsetPayloadMask
  = 0x007FFFFFU;

static const __swift_uint32_t _SwiftKeyPathComponentHeader_MaximumOffsetPayload
  = 0x007FFFFCU;
  
static const __swift_uint32_t _SwiftKeyPathComponentHeader_UnresolvedIndirectOffsetPayload
  = 0x007FFFFDU;
  
static const __swift_uint32_t _SwiftKeyPathComponentHeader_UnresolvedFieldOffsetPayload
  = 0x007FFFFEU;

static const __swift_uint32_t _SwiftKeyPathComponentHeader_OutOfLineOffsetPayload
  = 0x007FFFFFU;
  
static const __swift_uint32_t _SwiftKeyPathComponentHeader_StoredMutableFlag
  = 0x00800000U;

static const __swift_uint32_t _SwiftKeyPathComponentHeader_OptionalChainPayload
  = 0;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_OptionalWrapPayload
  = 1;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_OptionalForcePayload
  = 2;

static const __swift_uint32_t _SwiftKeyPathComponentHeader_EndOfReferencePrefixFlag
  = 0x80000000U;
  
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedMutatingFlag
  = 0x00800000U;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedSettableFlag
  = 0x00400000U;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedIDByStoredPropertyFlag
  = 0x00200000U;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedIDByVTableOffsetFlag
  = 0x00100000U;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedHasArgumentsFlag
  = 0x00080000U;
// Not ABI, used internally by key path runtime implementation
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedInstantiatedFromExternalWithArgumentsFlag
  = 0x00000010U;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedIDResolutionMask
  = 0x0000000FU;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedIDResolved
  = 0x00000000U;
static const __swift_uint32_t _SwiftKeyPathComponentHeader_ComputedIDUnresolvedIndirectPointer
  = 0x00000002U;

extern void *(swift_keyPathGenericWitnessTable[]);

static inline void *__swift_keyPathGenericWitnessTable_addr(void) {
  return swift_keyPathGenericWitnessTable;
}

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif // __SWIFT_SHIMS_KEYPATH_H__
