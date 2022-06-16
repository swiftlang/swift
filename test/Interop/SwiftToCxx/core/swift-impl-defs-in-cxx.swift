// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Core -clang-header-expose-public-decls -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s < %t/core.h

// RUN: %check-interop-cxx-header-in-clang(%t/core.h)


// CHECK:      #ifndef SWIFT_PRINTED_CORE
// CHECK-NEXT: #define SWIFT_PRINTED_CORE
// CHECK-NEXT: namespace swift {
// CHECK-EMPTY:
// CHECK-NEXT: namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: #ifdef __cplusplus
// CHECK-NEXT: extern "C" {
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: // Swift type metadata response type.
// CHECK-NEXT: struct MetadataResponseTy {
// CHECK-NEXT:   void * _Null_unspecified _0;
// CHECK-NEXT:   uint{{.*}}_t _1;
// CHECK-NEXT: };
// CHECK-NEXT: // Swift type metadata request type.
// CHECK-NEXT: typedef uint{{.*}}_t MetadataRequestTy;
// CHECK-EMPTY:
// CHECK-NEXT: using ValueWitnessInitializeBufferWithCopyOfBufferTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(55882))(void * _Nonnull, void * _Nonnull, void * _Nonnull);
// CHECK-NEXT: using ValueWitnessDestroyTy = void(* __ptrauth_swift_value_witness_function_pointer(1272))(void * _Nonnull, void * _Nonnull);
// CHECK-NEXT: using ValueWitnessInitializeWithCopyTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(58298))(void * _Nonnull, void * _Nonnull, void * _Nonnull);
// CHECK-NEXT: using ValueWitnessAssignWithCopyTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(34641))(void * _Nonnull, void * _Nonnull, void * _Nonnull);
// CHECK-NEXT: using ValueWitnessInitializeWithTakeTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(18648))(void * _Nonnull, void * _Nonnull, void * _Nonnull);
// CHECK-NEXT: using ValueWitnessAssignWithTakeTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(61402))(void * _Nonnull, void * _Nonnull, void * _Nonnull);
// CHECK-NEXT: using ValueWitnessGetEnumTagSinglePayloadTy = unsigned(* __ptrauth_swift_value_witness_function_pointer(24816))(const void * _Nonnull, unsigned, void * _Nonnull);
// CHECK-NEXT: using ValueWitnessStoreEnumTagSinglePayloadTy = void(* __ptrauth_swift_value_witness_function_pointer(41169))(void * _Nonnull, unsigned, unsigned, void * _Nonnull);
// CHECK-EMPTY:
// CHECK-NEXT: struct ValueWitnessTable {
// CHECK-NEXT:  ValueWitnessInitializeBufferWithCopyOfBufferTy _Nonnull initializeBufferWithCopyOfBuffer;
// CHECK-NEXT:  ValueWitnessDestroyTy _Nonnull destroy;
// CHECK-NEXT:  ValueWitnessInitializeWithCopyTy _Nonnull initializeWithCopy;
// CHECK-NEXT:  ValueWitnessAssignWithCopyTy _Nonnull assignWithCopy;
// CHECK-NEXT:  ValueWitnessInitializeWithTakeTy _Nonnull initializeWithTake;
// CHECK-NEXT:  ValueWitnessAssignWithTakeTy _Nonnull assignWithTake;
// CHECK-NEXT:  ValueWitnessGetEnumTagSinglePayloadTy _Nonnull getEnumTagSinglePayload;
// CHECK-NEXT:  ValueWitnessStoreEnumTagSinglePayloadTy _Nonnull storeEnumTagSinglePayload;
// CHECK-NEXT:  size_t size;
// CHECK-NEXT:  size_t stride;
// CHECK-NEXT:  unsigned flags;
// CHECK-NEXT:  unsigned extraInhabitantCount;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: #ifdef __cplusplus
// CHECK-NEXT: }
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace _impl
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace swift
// CHECK-EMPTY:
// CHECK-NEXT: #endif
