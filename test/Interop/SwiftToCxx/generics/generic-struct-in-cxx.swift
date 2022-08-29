// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Generics -clang-header-expose-public-decls -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h)

// Check that an instantiation compiles too.
// RUN: echo "constexpr int x = sizeof(Generics::GenericPair<int, int>);" >> %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h)

// FIXME: evolution on.

public struct GenericPair<T, T2> {
    let x: T
    let y: T2
}

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class _impl_GenericPair;
// CHECK-EMPTY:
// CHECK-NEXT: // Type metadata accessor for GenericPair
// CHECK-NEXT: SWIFT_EXTERN swift::_impl::MetadataResponseTy $s8Generics11GenericPairVMa(swift::_impl::MetadataRequestTy, void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class GenericPair final {
// CHECK-NEXT: public:
// CHECK-NEXT:   inline ~GenericPair() {
// CHECK-NEXT:     auto metadata = _impl::$s8Generics11GenericPairVMa(0, swift::getTypeMetadata<T_0_0>(), swift::getTypeMetadata<T_0_1>());

// CHECK: swift::_impl::OpaqueStorage _storage;
// CHECK-NEXT: friend class _impl::_impl_GenericPair<T_0_0, T_0_1>;
// CHECK-NEXT: }

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class _impl_GenericPair {
// CHECK-NEXT: public:
// CHECK-NEXT:   static inline char * _Nonnull getOpaquePointer(GenericPair<T_0_0, T_0_1> &object) { return object._getOpaquePointer(); }
// CHECK-NEXT:   static inline const char * _Nonnull getOpaquePointer(const GenericPair<T_0_0, T_0_1> &object) { return object._getOpaquePointer(); }
// CHECK-NEXT: template<class T>
// CHECK-NEXT: static inline GenericPair<T_0_0, T_0_1> returnNewValue(T callable) {
// CHECK-NEXT:   auto result = GenericPair<T_0_0, T_0_1>::_make();
// CHECK-NEXT:   callable(result._getOpaquePointer());
// CHECK-NEXT:   return result;
// CHECK-NEXT: }
