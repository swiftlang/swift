// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Generics -clang-header-expose-public-decls -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h)

// Check that an instantiation compiles too.
// RUN: echo "constexpr int x = sizeof(Generics::GenericPair<int, int>);" >> %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h)

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-library-evolution -typecheck -module-name Generics -clang-header-expose-public-decls -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h)

public struct GenericPair<T, T2> {
    let x: T
    let y: T2
}

public func makeGenericPair<T, T1>(_ x: T, _ y: T1) -> GenericPair<T, T1> {
    return GenericPair<T, T1>(x: x, y: y);
}

public func takeGenericPair<T, T1>(_ x: GenericPair<T, T1>) {
    print(x)
}

public func passThroughGenericPair<T1, T>(_ x: GenericPair<T1, T>, _ y: T)  -> GenericPair<T1, T> {
    return GenericPair<T1, T>(x: x.x, y: y)
}

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class _impl_GenericPair;
// CHECK-EMPTY:
// CHECK-NEXT: static_assert(2 <= 3, "unsupported generic requirement list for metadata func");
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

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class GenericPair;
// CHECK-EMPTY:
// CHECK-NEXT: template<class T, class T1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T> && swift::isUsableInGenericContext<T1>
// CHECK-NEXT: inline GenericPair<T, T1> makeGenericPair(const T & x, const T1 & y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_GenericPair<T, T1>::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s8Generics15makeGenericPairyAA0cD0Vyxq_Gx_q_tr0_lF(result, swift::_impl::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::getTypeMetadata<T>(), swift::getTypeMetadata<T1>());
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: template<class T1, class T>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T1> && swift::isUsableInGenericContext<T>
// CHECK-NEXT: inline GenericPair<T1, T> passThroughGenericPair(const GenericPair<T1, T>& x, const T & y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_GenericPair<T1, T>::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s8Generics22passThroughGenericPairyAA0dE0Vyxq_GAE_q_tr0_lF(result, _impl::_impl_GenericPair<T1, T>::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::getTypeMetadata<T1>(), swift::getTypeMetadata<T>());
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: template<class T, class T1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T> && swift::isUsableInGenericContext<T1>
// CHECK-NEXT: inline void takeGenericPair(const GenericPair<T, T1>& x) noexcept {
// CHECK-NEXT:  return _impl::$s8Generics15takeGenericPairyyAA0cD0Vyxq_Gr0_lF(_impl::_impl_GenericPair<T, T1>::getOpaquePointer(x), swift::getTypeMetadata<T>(), swift::getTypeMetadata<T1>());
// CHECK-NEXT:}
