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
// CHECK-NEXT: using ValueWitnessInitializeBufferWithCopyOfBufferTy = void * _Nonnull(*)(void * _Nonnull, void * _Nonnull, void * _Nonnull) __ptrauth_swift_value_witness_function_pointer(
// CHECK-NEXT: using ValueWitnessDestroyTy = void(*)(void * _Nonnull, void * _Nonnull) __ptrauth_swift_value_witness_function_pointer(
// CHECK-NEXT: using ValueWitnessInitializeWithCopyTy = void * _Nonnull(*)(void * _Nonnull, void * _Nonnull, void * _Nonnull) __ptrauth_swift_value_witness_function_pointer(
// CHECK-NEXT: using ValueWitnessAssignWithCopyTy = void * _Nonnull(*)(void * _Nonnull, void * _Nonnull, void * _Nonnull) __ptrauth_swift_value_witness_function_pointer(
// CHECK-NEXT: using ValueWitnessInitializeWithTakeTy = void * _Nonnull(*)(void * _Nonnull, void * _Nonnull, void * _Nonnull) __ptrauth_swift_value_witness_function_pointer(
// CHECK-NEXT: using ValueWitnessAssignWithTakeTy = void * _Nonnull(*)(void * _Nonnull, void * _Nonnull, void * _Nonnull) __ptrauth_swift_value_witness_function_pointer(
// CHECK-NEXT: using ValueWitnessGetEnumTagSinglePayloadTy = unsigned(*)(const void * _Nonnull, unsigned, void * _Nonnull) __ptrauth_swift_value_witness_function_pointer(
// CHECK-NEXT: using ValueWitnessStoreEnumTagSinglePayloadTy = void(*)(void * _Nonnull, unsigned, unsigned, void * _Nonnull) __ptrauth_swift_value_witness_function_pointer(
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
// CHECK-NEXT: /// Container for an opaque Swift value, like resilient struct.
// CHECK-NEXT: class OpaqueStorage {
// CHECK-NEXT: public:
// CHECK-NEXT:   inline OpaqueStorage() : storage(nullptr) { }
// CHECK-NEXT:   inline OpaqueStorage(ValueWitnessTable * _Nonnull vwTable) : storage(new char[vwTable->size]) { }
// CHECK-NEXT:   inline OpaqueStorage(OpaqueStorage&& other) : storage(other.storage) { other.storage = nullptr; }
// CHECK-NEXT:   inline OpaqueStorage(const OpaqueStorage&) = delete;
// CHECK-NEXT:   inline ~OpaqueStorage() { if (storage) { delete[] storage; } }
// CHECK-NEXT:   void operator =(OpaqueStorage&& other) { auto temp = storage; storage = other.storage; other.storage = temp; }
// CHECK-NEXT:   void operator =(const OpaqueStorage&) = delete;
// CHECK-NEXT:   inline char * _Nonnull getOpaquePointer() { return static_cast<char * _Nonnull>(storage); }
// CHECK-NEXT:   inline const char * _Nonnull getOpaquePointer() const { return static_cast<char * _Nonnull>(storage); }
// CHECK-NEXT: private:
// CHECK-NEXT:   char * _Nullable storage;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace _impl
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace swift
// CHECK-EMPTY:
// CHECK-NEXT: #endif
