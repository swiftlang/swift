//===--- RuntimeValueWitness.h                                         ---===//
// Swift Language Bytecode Layouts Runtime Implementation
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
// Implementations of runtime determined value witness functions
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BYTECODE_LAYOUTS_H
#define SWIFT_BYTECODE_LAYOUTS_H

#include "swift/Runtime/Metadata.h"
#include <cstdint>

// Layouts
//
// enum class LayoutType: char {
//   // Scalars
//   I8 = 'c',
//   I16 = 's',
//   I32 = 'l',
//   I64 = 'L',
//   ErrorReference = 'r',
//   NativeStrongReference = 'N',
//   NativeUnownedReference = 'n',
//   NativeWeakReference = 'W',
//   UnknownUnownedReference = 'u',
//   UnknownWeakReference = 'w',
//   BlockReference = 'b',
//   BridgeReference = 'B',
//   ObjCReference = 'o',
//   ExistentialReference = 'x',
//
//   // Enums
//   SinglePayloadEnum = 'e',
//   MultiPayloadEnum = 'E',
// };
//
// VALUE := STRUCT | ENUM | SCALAR
// SCALAR := 'c'|'s'|'l'|'L'|'C'|'r'|'N'|'n'|'W'|'u'|'w'|'b'|'B'|'o'|'f'|'x'
// ALIGNED_GROUP:= 'a' UINT32 (ALIGNMENT,UINT32,VALUE)+
// ALIGNMENT := '0'|'1'|'2'|'3'|'?'
// ENUM := SINGLEENUM | MULTIENUM
//
// SIZE := uint32 (does network order this need to be specified here?)
//
// // e numEmptyPayloads lengthOfPayload payload
// SINGLEENUM := 'e' SIZE SIZE VALUE
//
// // E numEmptyPayloads numPayloads lengthOfEachPayload payloads
// MULTIENUM := 'E' SIZE SIZE SIZE+ VALUE+
//
// OFFSETS := int32+
//
// Examples:
// struct SomeStruct {
//  let a : int8
//  let b : int16
//  let c : int16
//  let d : SomeClass
// }
//
// '1c2s2s3N'
// byte aligned int8
// 2 byte aligned int16
// 2 byte aligned int16
// 4 byte aligned Native Pointer
//
// enum SampleEnum {
//    case Payload(s: SomeStruct)
//    case None
//    case ReallyNone
// }
// 'e(0x00000002)(0x00000008)1c2s2s3N'
// An enum with:
// - 2 empty cases
// - a payload stringlength of 8
// - a struct payload as in the previous example

namespace swift {
enum class LayoutType : char {
  // Scalars
  I8 = 'c',
  I16 = 's',
  I32 = 'l',
  I64 = 'L',
  ErrorReference = 'r',
  NativeStrongReference = 'N',
  NativeUnownedReference = 'n',
  NativeWeakReference = 'W',
  UnknownUnownedReference = 'u',
  UnknownWeakReference = 'w',
  BlockReference = 'b',
  BridgeReference = 'B',
  ObjCReference = 'o',
  // Enums
  // Single
  // e{emptycases}{payload}
  //
  // Multi
  // e{emptycases}{num_cases}{payload1}{payload2}{...}{payloadn}
  SinglePayloadEnum = 'e',
  MultiPayloadEnum = 'E',
  AlignedGroup = 'a',
  ArcheType = 'A',
  ResilientType = 'R',
};

SWIFT_RUNTIME_EXPORT
void swift_generic_destroy(void *address, uint8_t *layout, uint32_t layoutLen);
} // namespace swift

#endif // SWIFT_BYTECODE_LAYOUTS_H
