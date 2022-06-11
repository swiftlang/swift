// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -Wno-unused-private-field)

// CHECK: namespace Structs {
// CHECK: namespace _impl {

// CHECK: namespace Structs {

// CHECK:      namespace _impl {
// CHECK-EMPTY:
// CHECK-NEXT: class _impl_StructWithIntField;
// CHECK-EMPTY:
// CHECK-NEXT: }

// CHECK:      class StructWithIntField final {
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
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: }
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
