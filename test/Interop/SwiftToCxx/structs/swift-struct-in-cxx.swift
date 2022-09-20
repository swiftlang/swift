// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -Wno-unused-private-field -Wno-unused-function)

// CHECK: namespace Structs {
// CHECK: namespace _impl {

// CHECK: namespace Structs {

// CHECK:      namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_StructWithIntField;
// CHECK-EMPTY:
// CHECK-NEXT: // Type metadata accessor for StructWithIntField
// CHECK-NEXT: SWIFT_EXTERN swift::_impl::MetadataResponseTy $s7Structs18StructWithIntFieldVMa(swift::_impl::MetadataRequestTy) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT: }

// CHECK:      class StructWithIntField final {
// CHECK-NEXT: public:
// CHECK-NEXT:   inline ~StructWithIntField() {
// CHECK:        }
// CHECK-NEXT:   inline StructWithIntField(const StructWithIntField &other) {
// CHECK:        }
// CHECK-NEXT:   inline StructWithIntField(StructWithIntField &&) = default;
// CHECK-NEXT: private:
// CHECK-NEXT:   inline StructWithIntField() {}
// CHECK-NEXT:   static inline StructWithIntField _make() { return StructWithIntField(); }
// CHECK-NEXT:   inline const char * _Nonnull _getOpaquePointer() const { return _storage; }
// CHECK-NEXT:   inline char * _Nonnull _getOpaquePointer() { return _storage; }
// CHECK-EMPTY:
// CHECK-NEXT:   alignas(8) char _storage[8];
// CHECK-NEXT:   friend class _impl::_impl_StructWithIntField;
// CHECK-NEXT: };

// CHECK:      namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_StructWithIntField {
// CHECK-NEXT: public:
// CHECK-NEXT: static inline char *  _Nonnull getOpaquePointer(StructWithIntField &object) { return object._getOpaquePointer(); }
// CHECK-NEXT: static inline const char * _Nonnull getOpaquePointer(const StructWithIntField &object) { return object._getOpaquePointer(); }
// CHECK-NEXT: template<class T>
// CHECK-NEXT: static inline StructWithIntField returnNewValue(T callable) {
// CHECK-NEXT:   auto result = StructWithIntField::_make();
// CHECK-NEXT:   callable(result._getOpaquePointer());
// CHECK-NEXT:   return result;
// CHECK-NEXT:  }
// CHECK-NEXT: static inline void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK-NEXT:   auto metadata = _impl::$s7Structs18StructWithIntFieldVMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: }
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: } // end namespace
// CHECK-EMPTY:
// CHECK-NEXT: namespace swift {
// CHECK-NEXT: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wc++17-extensions"
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isUsableInGenericContext<Structs::StructWithIntField> = true;
// CHECK-NEXT: template<>
// CHECK-NEXT: struct TypeMetadataTrait<Structs::StructWithIntField>
// CHECK-NEXT: inline void * _Nonnull getTypeMetadata() {
// CHECK-NEXT:   return Structs::_impl::$s7Structs18StructWithIntFieldVMa(0)._0;
// CHECK-NEXT: }
// CHECK-NEXT: };
// CHECK-NEXT: namespace _impl{
// CHECK-NEXT: template<>
// CHECK-NEXT: static inline const constexpr bool isValueType<Structs::StructWithIntField> = true;
// CHECK-NEXT: template<>
// CHECK-NEXT: struct implClassFor<Structs::StructWithIntField> { using type = Structs::_impl::_impl_StructWithIntField; };
// CHECK-NEXT: } // namespace
// CHECK-NEXT: #pragma clang diagnostic pop
// CHECK-NEXT: } // namespace swift
// CHECK-EMPTY:
// CHECK-NEXT: namespace Structs {

public struct StructWithIntField {
  let field: Int64
}

// Special name gets renamed in C++.
// CHECK: class register_ final {
// CHECK: alignas(8) char _storage[16];
// CHECK-NEXT:   friend class
// CHECK-NEXT: };
public struct register {
  let field1: Int64
  let field2: Int64
}

// CHECK: } // namespace Structs
