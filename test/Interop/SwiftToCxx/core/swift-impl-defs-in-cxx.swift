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
// CHECK-NEXT: #if __cplusplus > 201402L
// CHECK-NEXT: #  define SWIFT_NOEXCEPT_FUNCTION_PTR noexcept
// CHECK-NEXT: #else
// CHECK-NEXT: #  define SWIFT_NOEXCEPT_FUNCTION_PTR
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: using ValueWitnessInitializeBufferWithCopyOfBufferTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(55882))(void * _Nonnull, void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using ValueWitnessDestroyTy = void(* __ptrauth_swift_value_witness_function_pointer(1272))(void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using ValueWitnessInitializeWithCopyTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(58298))(void * _Nonnull, void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using ValueWitnessAssignWithCopyTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(34641))(void * _Nonnull, void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using ValueWitnessInitializeWithTakeTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(18648))(void * _Nonnull, void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using ValueWitnessAssignWithTakeTy = void * _Nonnull(* __ptrauth_swift_value_witness_function_pointer(61402))(void * _Nonnull, void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using ValueWitnessGetEnumTagSinglePayloadTy = unsigned(* __ptrauth_swift_value_witness_function_pointer(24816))(const void * _Nonnull, unsigned, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using ValueWitnessStoreEnumTagSinglePayloadTy = void(* __ptrauth_swift_value_witness_function_pointer(41169))(void * _Nonnull, unsigned, unsigned, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
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
// CHECK-NEXT: using EnumValueWitnessGetEnumTagTy = int(* __ptrauth_swift_value_witness_function_pointer(41909))(const void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using EnumValueWitnessDestructiveProjectEnumDataTy = void(* __ptrauth_swift_value_witness_function_pointer(1053))(void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-NEXT: using EnumValueWitnessDestructiveInjectEnumTagTy = void(* __ptrauth_swift_value_witness_function_pointer(45796))(void * _Nonnull, unsigned, void * _Nonnull) SWIFT_NOEXCEPT_FUNCTION_PTR;
// CHECK-EMPTY:
// CHECK-NEXT: struct EnumValueWitnessTable {
// CHECK-NEXT:   ValueWitnessTable vwTable;
// CHECK-NEXT:   EnumValueWitnessGetEnumTagTy _Nonnull getEnumTag;
// CHECK-NEXT:   EnumValueWitnessDestructiveProjectEnumDataTy _Nonnull destructiveProjectEnumData;
// CHECK-NEXT:   EnumValueWitnessDestructiveInjectEnumTagTy _Nonnull destructiveInjectEnumTag;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: #undef SWIFT_NOEXCEPT_FUNCTION_PTR
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT: // type metadata address for Bool.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $sSbN;
// CHECK-NEXT: // type metadata address for Int8.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss4Int8VN;
// CHECK-NEXT: // type metadata address for UInt8.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss5UInt8VN;
// CHECK-NEXT: // type metadata address for Int16.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss5Int16VN;
// CHECK-NEXT: // type metadata address for UInt16.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss6UInt16VN;
// CHECK-NEXT: // type metadata address for Int32.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss5Int32VN;
// CHECK-NEXT: // type metadata address for UInt32.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss6UInt32VN;
// CHECK-NEXT: // type metadata address for Int64.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss5Int64VN;
// CHECK-NEXT: // type metadata address for UInt64.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss6UInt64VN;
// CHECK-NEXT: // type metadata address for Float.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $sSfN;
// CHECK-NEXT: // type metadata address for Double.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $sSdN;
// CHECK-NEXT: // type metadata address for OpaquePointer.
// CHECK-NEXT: SWIFT_IMPORT_STDLIB_SYMBOL extern size_t $ss13OpaquePointerVN;
// CHECK-EMPTY:
// CHECK-NEXT: #ifdef __cplusplus
// CHECK-NEXT: }
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: inline void * _Nonnull opaqueAlloc(size_t size, size_t align) noexcept {
// CHECK-NEXT: #if defined(_WIN32)
// CHECK-NEXT:   void *r = _aligned_malloc(size, align);
// CHECK-NEXT: #else
// CHECK-NEXT:   if (align < sizeof(void *)) align = sizeof(void *);
// CHECK-NEXT:   void *r = nullptr;
// CHECK-NEXT:   int res = posix_memalign(&r, align, size);
// CHECK-NEXT:   (void)res;
// CHECK-NEXT: #endif
// CHECK-NEXT:   return r;
// CHECK-NEXT: }
// CHECK-NEXT: inline void opaqueFree(void * _Nonnull p) noexcept {
// CHECK-NEXT: #if defined(_WIN32)
// CHECK-NEXT:   _aligned_free(p);
// CHECK-NEXT: #else
// CHECK-NEXT:   free(p);
// CHECK-NEXT: #endif
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: /// Container for an opaque Swift value, like resilient struct.
// CHECK-NEXT: class OpaqueStorage {
// CHECK-NEXT: public:
// CHECK-NEXT:   inline OpaqueStorage() noexcept : storage(nullptr) { }
// CHECK-NEXT:   inline OpaqueStorage(ValueWitnessTable * _Nonnull vwTable) noexcept : storage(reinterpret_cast<char *>(opaqueAlloc(vwTable->size, (vwTable->flags &255) + 1))) { }
// CHECK-NEXT:   inline OpaqueStorage(OpaqueStorage&& other) noexcept : storage(other.storage) { other.storage = nullptr; }
// CHECK-NEXT:   inline OpaqueStorage(const OpaqueStorage&) noexcept = delete;
// CHECK-NEXT:   inline ~OpaqueStorage() noexcept { if (storage) { opaqueFree(static_cast<char * _Nonnull>(storage)); } }
// CHECK-NEXT:   void operator =(OpaqueStorage&& other) noexcept { auto temp = storage; storage = other.storage; other.storage = temp; }
// CHECK-NEXT:   void operator =(const OpaqueStorage&) noexcept = delete;
// CHECK-NEXT:   inline char * _Nonnull getOpaquePointer() noexcept { return static_cast<char * _Nonnull>(storage); }
// CHECK-NEXT:   inline const char * _Nonnull getOpaquePointer() const noexcept { return static_cast<char * _Nonnull>(storage); }
// CHECK-NEXT: private:
// CHECK-NEXT:   char * _Nullable storage;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: /// Naive exception class that should be thrown
// CHECK-NEXT: class NaiveException {
// CHECK-NEXT: public:
// CHECK-NEXT: inline NaiveException(const char * _Nonnull msg) noexcept : msg_(msg) { }
// CHECK-NEXT: inline NaiveException(NaiveException&& other) noexcept : msg_(other.msg_) { other.msg_ = nullptr; }
// CHECK-NEXT: inline ~NaiveException() noexcept { }
// CHECK-NEXT: void operator =(NaiveException&& other) noexcept { auto temp = msg_; msg_ = other.msg_; other.msg_ = temp; }
// CHECK-NEXT: void operator =(const NaiveException&) noexcept = delete;
// CHECK-NEXT: inline const char * _Nonnull getMessage() const noexcept { return(msg_); }
// CHECK-NEXT: private:
// CHECK-NEXT: const char * _Nonnull msg_;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace _impl
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT: #if __cplusplus > 201402L
// CHECK-NEXT: template<class T>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext = false;
// CHECK-EMPTY:
// CHECK-NEXT: template<class T> inline void * _Nonnull getTypeMetadata();
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<bool> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<bool>() {
// CHECK-NEXT:   return &_impl::$sSbN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<int8_t> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<int8_t>() {
// CHECK-NEXT:   return &_impl::$ss4Int8VN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<uint8_t> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<uint8_t>() {
// CHECK-NEXT:   return &_impl::$ss5UInt8VN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<int16_t> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<int16_t>() {
// CHECK-NEXT:   return &_impl::$ss5Int16VN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<uint16_t> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<uint16_t>() {
// CHECK-NEXT:   return &_impl::$ss6UInt16VN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<int32_t> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<int32_t>() {
// CHECK-NEXT:   return &_impl::$ss5Int32VN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<uint32_t> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<uint32_t>() {
// CHECK-NEXT:   return &_impl::$ss6UInt32VN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<int64_t> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<int64_t>() {
// CHECK-NEXT:   return &_impl::$ss5Int64VN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<uint64_t> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<uint64_t>() {
// CHECK-NEXT:   return &_impl::$ss6UInt64VN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<float> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<float>() {
// CHECK-NEXT:   return &_impl::$sSfN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<double> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<double>() {
// CHECK-NEXT:   return &_impl::$sSdN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<void *> = true;
// CHECK-EMPTY:
// CHECK-NEXT: template<>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata<void *>() {
// CHECK-NEXT:   return &_impl::$ss13OpaquePointerVN;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: #endif
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace swift
// CHECK-EMPTY:
// CHECK-NEXT: #endif
