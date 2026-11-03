//===--- _SwiftValueWitnessTable.h - C++ value witnesses --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The stable value witness table ABI used by generated C++ bindings. Keep the
// fields and pointer-authentication discriminators in sync with
// swift/ABI/ValueWitness.def and swift/ABI/MetadataValues.h.
// Included after the generated prologue and _SwiftCxxInteroperability.h.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CXX_VALUE_WITNESS_TABLE_H
#define SWIFT_CXX_VALUE_WITNESS_TABLE_H

namespace swift SWIFT_PRIVATE_ATTR {
namespace _impl {
extern "C" {

#if __cplusplus > 201402L
#define SWIFT_NOEXCEPT_FUNCTION_PTR noexcept
#else
#define SWIFT_NOEXCEPT_FUNCTION_PTR
#endif

using ValueWitnessInitializeBufferWithCopyOfBufferTy = void *_Nonnull (
    *__ptrauth_swift_value_witness_function_pointer(55882))(
    void *_Nonnull, void *_Nonnull, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using ValueWitnessDestroyTy =
    void (*__ptrauth_swift_value_witness_function_pointer(1272))(
        void *_Nonnull, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using ValueWitnessInitializeWithCopyTy = void *_Nonnull (
    *__ptrauth_swift_value_witness_function_pointer(58298))(
    void *_Nonnull, void *_Nonnull, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using ValueWitnessAssignWithCopyTy = void *_Nonnull (
    *__ptrauth_swift_value_witness_function_pointer(34641))(
    void *_Nonnull, void *_Nonnull, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using ValueWitnessInitializeWithTakeTy = void *_Nonnull (
    *__ptrauth_swift_value_witness_function_pointer(18648))(
    void *_Nonnull, void *_Nonnull, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using ValueWitnessAssignWithTakeTy = void *_Nonnull (
    *__ptrauth_swift_value_witness_function_pointer(61402))(
    void *_Nonnull, void *_Nonnull, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using ValueWitnessGetEnumTagSinglePayloadTy = unsigned (
    *__ptrauth_swift_value_witness_function_pointer(24816))(
    const void *_Nonnull, unsigned, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using ValueWitnessStoreEnumTagSinglePayloadTy =
    void (*__ptrauth_swift_value_witness_function_pointer(41169))(
        void *_Nonnull, unsigned, unsigned,
        void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;

struct ValueWitnessTable {
  ValueWitnessInitializeBufferWithCopyOfBufferTy _Nonnull initializeBufferWithCopyOfBuffer;
  ValueWitnessDestroyTy _Nonnull destroy;
  ValueWitnessInitializeWithCopyTy _Nonnull initializeWithCopy;
  ValueWitnessAssignWithCopyTy _Nonnull assignWithCopy;
  ValueWitnessInitializeWithTakeTy _Nonnull initializeWithTake;
  ValueWitnessAssignWithTakeTy _Nonnull assignWithTake;
  ValueWitnessGetEnumTagSinglePayloadTy _Nonnull getEnumTagSinglePayload;
  ValueWitnessStoreEnumTagSinglePayloadTy _Nonnull storeEnumTagSinglePayload;
  size_t size;
  size_t stride;
  unsigned flags;
  unsigned extraInhabitantCount;

  constexpr size_t getAlignment() const { return (flags & 255) + 1; }
};

using EnumValueWitnessGetEnumTagTy =
    unsigned (*__ptrauth_swift_value_witness_function_pointer(41909))(
        const void *_Nonnull, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using EnumValueWitnessDestructiveProjectEnumDataTy =
    void (*__ptrauth_swift_value_witness_function_pointer(1053))(
        void *_Nonnull, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
using EnumValueWitnessDestructiveInjectEnumTagTy =
    void (*__ptrauth_swift_value_witness_function_pointer(45796))(
        void *_Nonnull, unsigned, void *_Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;

struct EnumValueWitnessTable {
  ValueWitnessTable vwTable;
  EnumValueWitnessGetEnumTagTy _Nonnull getEnumTag;
  EnumValueWitnessDestructiveProjectEnumDataTy _Nonnull destructiveProjectEnumData;
  EnumValueWitnessDestructiveInjectEnumTagTy _Nonnull destructiveInjectEnumTag;
};

#undef SWIFT_NOEXCEPT_FUNCTION_PTR

} // extern "C"
} // namespace _impl
} // namespace swift SWIFT_PRIVATE_ATTR

#endif // SWIFT_CXX_VALUE_WITNESS_TABLE_H
