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
#include <vector>

namespace swift {

enum class RefCountingKind : uint8_t {
  End = 0x00,
  Error = 0x01,
  NativeStrong = 0x02,
  NativeUnowned = 0x03,
  NativeWeak = 0x04,
  Unknown = 0x05,
  UnknownUnowned = 0x06,
  UnknownWeak = 0x07,
  Bridge = 0x08,
  Block = 0x09,
  ObjC = 0x0a,
  NativeSwiftObjC = 0x0b,

  Metatype = 0x0c,
  Generic = 0x0d,
  Existential = 0x0e,
  Resilient = 0x0f,

  SinglePayloadEnumSimple = 0x10,
  SinglePayloadEnumFN = 0x11,
  SinglePayloadEnumFNResolved = 0x12,
  SinglePayloadEnumGeneric = 0x13,

  MultiPayloadEnumFN = 0x14,
  MultiPayloadEnumFNResolved = 0x15,
  MultiPayloadEnumGeneric = 0x16,

  // Skip = 0x80,
  // We may use the MSB as flag that a count follows,
  // so all following values are reserved
  // Reserved: 0x81 - 0xFF
};

struct LayoutStringReader1 {
  const uint8_t *layoutStr;

  template <typename T>
  inline T readBytes() {
    T returnVal;
    memcpy(&returnVal, layoutStr, sizeof(T));
    layoutStr += sizeof(T);
    return returnVal;
  }

  template <typename... T>
  inline void readBytes(T&... result) {
    uintptr_t additionalOffset = 0;
    ([&] {
        memcpy(&result, layoutStr + additionalOffset, sizeof(T));
        additionalOffset += sizeof(T);
    }(), ...);
    layoutStr += additionalOffset;
  }

  template<typename T, typename F>
  inline T modify(F &&f) {
    LayoutStringReader1 readerCopy = *this;
    T res = f(readerCopy);
    layoutStr = readerCopy.layoutStr;
    return res;
  }

  template<typename F>
  inline void modify(F &&f) {
    LayoutStringReader1 readerCopy = *this;
    f(readerCopy);
    layoutStr = readerCopy.layoutStr;
  }

  template <typename T>
  inline T peekBytes(size_t peekOffset = 0) const {
    T returnVal;
    memcpy(&returnVal, layoutStr + peekOffset, sizeof(T));
    return returnVal;
  }

  inline void skip(size_t n) { layoutStr += n; }

  inline uintptr_t getAbsolute() {
    return (uintptr_t) layoutStr;
  }
};

struct LayoutStringReader {
  const uint8_t *layoutStr;
  size_t offset;

  template <typename T>
  inline T readBytes() {
    T returnVal;
    memcpy(&returnVal, layoutStr + offset, sizeof(T));
    offset += sizeof(T);
    return returnVal;
  }

  template <typename... T>
  inline void readBytes(T&... result) {
    uintptr_t additionalOffset = 0;
    ([&] {
        memcpy(&result, layoutStr + offset + additionalOffset, sizeof(T));
        additionalOffset += sizeof(T);
    }(), ...);
    offset += additionalOffset;
  }

  template<typename T, typename F>
  inline T modify(F &&f) {
    LayoutStringReader readerCopy = *this;
    T res = f(readerCopy);
    offset = readerCopy.offset;
    return res;
  }

  template<typename F>
  inline void modify(F &&f) {
    LayoutStringReader readerCopy = *this;
    f(readerCopy);
    offset = readerCopy.offset;
  }

  template <typename T>
  inline T peekBytes(size_t peekOffset = 0) const {
    T returnVal;
    memcpy(&returnVal, layoutStr + offset + peekOffset, sizeof(T));
    return returnVal;
  }

  inline void skip(size_t n) { offset += n; }

  inline uintptr_t getAbsolute() {
    return (uintptr_t) layoutStr + offset;
  }
};

struct LayoutStringWriter {
  uint8_t *layoutStr;
  size_t offset;

  template <typename T>
  inline void writeBytes(T value) {
    memcpy(layoutStr + offset, &value, sizeof(T));
    offset += sizeof(T);
  }

  inline void skip(size_t n) { offset += n; }
};

SWIFT_RUNTIME_EXPORT
void swift_cvw_destroy(swift::OpaqueValue *address, const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *swift_cvw_assignWithCopy(swift::OpaqueValue *dest,
                                             swift::OpaqueValue *src,
                                             const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *swift_cvw_assignWithTake(swift::OpaqueValue *dest,
                                             swift::OpaqueValue *src,
                                             const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *swift_cvw_initWithCopy(swift::OpaqueValue *dest,
                                           swift::OpaqueValue *src,
                                           const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *swift_cvw_initWithTake(swift::OpaqueValue *dest,
                                           swift::OpaqueValue *src,
                                           const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *
swift_cvw_initializeBufferWithCopyOfBuffer(swift::ValueBuffer *dest,
                                           swift::ValueBuffer *src,
                                           const Metadata *metadata);

SWIFT_RUNTIME_EXPORT
void swift_cvw_destroyMultiPayloadEnumFN(swift::OpaqueValue *address,
                                         const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *
swift_cvw_assignWithCopyMultiPayloadEnumFN(swift::OpaqueValue *dest,
                                           swift::OpaqueValue *src,
                                           const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *
swift_cvw_assignWithTakeMultiPayloadEnumFN(swift::OpaqueValue *dest,
                                           swift::OpaqueValue *src,
                                           const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *
swift_cvw_initWithCopyMultiPayloadEnumFN(swift::OpaqueValue *dest,
                                         swift::OpaqueValue *src,
                                         const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *
swift_cvw_initWithTakeMultiPayloadEnumFN(swift::OpaqueValue *dest,
                                         swift::OpaqueValue *src,
                                         const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *
swift_cvw_initializeBufferWithCopyOfBufferMultiPayloadEnumFN(
    swift::ValueBuffer *dest, swift::ValueBuffer *src,
    const Metadata *metadata);

SWIFT_RUNTIME_EXPORT
unsigned swift_cvw_singletonEnum_getEnumTag(swift::OpaqueValue *address,
                                            const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_cvw_singletonEnum_destructiveInjectEnumTag(
    swift::OpaqueValue *address, unsigned tag, const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
unsigned swift_cvw_enumSimple_getEnumTag(swift::OpaqueValue *address,
                                         const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_cvw_enumSimple_destructiveInjectEnumTag(swift::OpaqueValue *address,
                                                   unsigned tag,
                                                   const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
unsigned swift_cvw_enumFn_getEnumTag(swift::OpaqueValue *address,
                                     const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
unsigned
swift_cvw_multiPayloadEnumGeneric_getEnumTag(swift::OpaqueValue *address,
                                             const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_cvw_multiPayloadEnumGeneric_destructiveInjectEnumTag(
    swift::OpaqueValue *address, unsigned tag, const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
unsigned
swift_cvw_singlePayloadEnumGeneric_getEnumTag(swift::OpaqueValue *address,
                                              const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_cvw_singlePayloadEnumGeneric_destructiveInjectEnumTag(
    swift::OpaqueValue *address, unsigned tag, const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_cvw_instantiateLayoutString(const uint8_t *layoutStr,
                                       Metadata *type);

void swift_cvw_resolve_resilientAccessors(uint8_t *layoutStr,
                                          size_t layoutStrOffset,
                                          const uint8_t *fieldLayoutStr,
                                          const Metadata *fieldType);

void swift_cvw_arrayDestroy(swift::OpaqueValue *addr, size_t count,
                            size_t stride, const Metadata *metadata);

void swift_cvw_arrayInitWithCopy(swift::OpaqueValue *dest,
                                 swift::OpaqueValue *src, size_t count,
                                 size_t stride, const Metadata *metadata);

extern "C" void swift_cvw_arrayAssignWithCopy(swift::OpaqueValue *dest,
                                              swift::OpaqueValue *src,
                                              size_t count, size_t stride,
                                              const Metadata *metadata);

// For backwards compatibility
SWIFT_RUNTIME_EXPORT
void swift_generic_destroy(swift::OpaqueValue *address,
                           const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *swift_generic_assignWithCopy(swift::OpaqueValue *dest,
                                                 swift::OpaqueValue *src,
                                                 const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *swift_generic_assignWithTake(swift::OpaqueValue *dest,
                                                 swift::OpaqueValue *src,
                                                 const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *swift_generic_initWithCopy(swift::OpaqueValue *dest,
                                               swift::OpaqueValue *src,
                                               const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *swift_generic_initWithTake(swift::OpaqueValue *dest,
                                               swift::OpaqueValue *src,
                                               const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
swift::OpaqueValue *
swift_generic_initializeBufferWithCopyOfBuffer(swift::ValueBuffer *dest,
                                               swift::ValueBuffer *src,
                                               const Metadata *metadata);

SWIFT_RUNTIME_EXPORT
unsigned swift_singletonEnum_getEnumTag(swift::OpaqueValue *address,
                                        const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_singletonEnum_destructiveInjectEnumTag(swift::OpaqueValue *address,
                                                  unsigned tag,
                                                  const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
unsigned swift_enumSimple_getEnumTag(swift::OpaqueValue *address,
                                     const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_enumSimple_destructiveInjectEnumTag(swift::OpaqueValue *address,
                                               unsigned tag,
                                               const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
unsigned swift_enumFn_getEnumTag(swift::OpaqueValue *address,
                                 const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
unsigned swift_multiPayloadEnumGeneric_getEnumTag(swift::OpaqueValue *address,
                                                  const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_multiPayloadEnumGeneric_destructiveInjectEnumTag(
    swift::OpaqueValue *address, unsigned tag, const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
unsigned swift_singlePayloadEnumGeneric_getEnumTag(swift::OpaqueValue *address,
                                                   const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_singlePayloadEnumGeneric_destructiveInjectEnumTag(
    swift::OpaqueValue *address, unsigned tag, const Metadata *metadata);
SWIFT_RUNTIME_EXPORT
void swift_generic_instantiateLayoutString(const uint8_t *layoutStr, Metadata *type);

constexpr size_t layoutStringHeaderSize = sizeof(uint64_t) + sizeof(size_t);

} // namespace swift

#endif // SWIFT_BYTECODE_LAYOUTS_H
