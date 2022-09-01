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

@frozen
public struct GenericPair<T, T2> {
    var x: T
    var y: T2
}

public func makeGenericPair<T, T1>(_ x: T, _ y: T1) -> GenericPair<T, T1> {
    return GenericPair<T, T1>(x: x, y: y);
}

public func makeConcretePair(_ x: UInt32, _ y: UInt32) -> GenericPair<UInt32, UInt32> {
    return GenericPair<UInt32, UInt32>(x: x, y: y)
}

public func takeGenericPair<T, T1>(_ x: GenericPair<T, T1>) {
    print(x)
}

public func takeConcretePair(_ x: GenericPair<UInt32, UInt32>) {
    print("CONCRETE pair of UInt32: ", x.x, x.y, ";")
}

public func passThroughGenericPair<T1, T>(_ x: GenericPair<T1, T>, _ y: T)  -> GenericPair<T1, T> {
    return GenericPair<T1, T>(x: x.x, y: y)
}

public typealias ConcreteUint32Pair = GenericPair<UInt32, UInt32>

public func passThroughConcretePair(_ x: ConcreteUint32Pair, y: UInt32) -> ConcreteUint32Pair {
    return ConcreteUint32Pair(x: x.x, y: y)
}

public func inoutGenericPair<T1, T>(_ x: inout GenericPair<T1, T>, _ y: T1) {
    x.x = y
}

public func inoutConcretePair(_ x: UInt32, _ y: inout GenericPair<UInt32, UInt32>) {
    y.x = x
}

// CHECK: SWIFT_EXTERN void $s8Generics17inoutConcretePairyys6UInt32V_AA07GenericD0VyA2DGztF(uint32_t x, char * _Nonnull y) SWIFT_NOEXCEPT SWIFT_CALL; // inoutConcretePair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics16inoutGenericPairyyAA0cD0Vyxq_Gz_xtr0_lF(void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // inoutGenericPair(_:_:)
// CHECK-NEXT: // Stub struct to be used to pass/return values to/from Swift functions.
// CHECK-NEXT: struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V {
// CHECK-NEXT:   uint64_t _1;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: static inline void swift_interop_returnDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(char * _Nonnull result, struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V value) __attribute__((always_inline)) {
// CHECK-NEXT:   memcpy(result + 0, &value._1, 8);
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: static inline struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V swift_interop_passDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:   struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V result;
// CHECK-NEXT:   memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: SWIFT_EXTERN struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V $s8Generics16makeConcretePairyAA07GenericD0Vys6UInt32VAFGAF_AFtF(uint32_t x, uint32_t y) SWIFT_NOEXCEPT SWIFT_CALL; // makeConcretePair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics15makeGenericPairyAA0cD0Vyxq_Gx_q_tr0_lF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // makeGenericPair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V $s8Generics23passThroughConcretePair_1yAA07GenericE0Vys6UInt32VAGGAH_AGtF(struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V x, uint32_t y) SWIFT_NOEXCEPT SWIFT_CALL; // passThroughConcretePair(_:y:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics22passThroughGenericPairyAA0dE0Vyxq_GAE_q_tr0_lF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // passThroughGenericPair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics16takeConcretePairyyAA07GenericD0Vys6UInt32VAFGF(struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V x) SWIFT_NOEXCEPT SWIFT_CALL; // takeConcretePair(_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics15takeGenericPairyyAA0cD0Vyxq_Gr0_lF(const void * _Nonnull x, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // takeGenericPair(_:)

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
// CHECK-NEXT: inline void inoutConcretePair(uint32_t x, GenericPair<uint32_t, uint32_t>& y) noexcept {
// CHECK-NEXT:   return _impl::$s8Generics17inoutConcretePairyys6UInt32V_AA07GenericD0VyA2DGztF(x, _impl::_impl_GenericPair<uint32_t, uint32_t>::getOpaquePointer(y));
// CHECK-NEXT: }

// CHECK: template<class T1, class T>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T1> && swift::isUsableInGenericContext<T>
// CHECK-NEXT: inline void inoutGenericPair(GenericPair<T1, T>& x, const T1 & y) noexcept {
// CHECK-NEXT:   return _impl::$s8Generics16inoutGenericPairyyAA0cD0Vyxq_Gz_xtr0_lF(_impl::_impl_GenericPair<T1, T>::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::getTypeMetadata<T1>(), swift::getTypeMetadata<T>());
// CHECK-NEXT: }

// CHECK: template<class T, class T1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T> && swift::isUsableInGenericContext<T1>
// CHECK-NEXT: inline GenericPair<T, T1> makeGenericPair(const T & x, const T1 & y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_GenericPair<T, T1>::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s8Generics15makeGenericPairyAA0cD0Vyxq_Gx_q_tr0_lF(result, swift::_impl::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::getTypeMetadata<T>(), swift::getTypeMetadata<T1>());
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: inline GenericPair<uint32_t, uint32_t> passThroughConcretePair(const GenericPair<uint32_t, uint32_t>& x, uint32_t y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_GenericPair<uint32_t, uint32_t>::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:     _impl::swift_interop_returnDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(result, _impl::$s8Generics23passThroughConcretePair_1yAA07GenericE0Vys6UInt32VAGGAH_AGtF(_impl::swift_interop_passDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(_impl::_impl_GenericPair<uint32_t, uint32_t>::getOpaquePointer(x)), y));
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: template<class T1, class T>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T1> && swift::isUsableInGenericContext<T>
// CHECK-NEXT: inline GenericPair<T1, T> passThroughGenericPair(const GenericPair<T1, T>& x, const T & y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_GenericPair<T1, T>::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s8Generics22passThroughGenericPairyAA0dE0Vyxq_GAE_q_tr0_lF(result, _impl::_impl_GenericPair<T1, T>::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::getTypeMetadata<T1>(), swift::getTypeMetadata<T>());
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: inline void takeConcretePair(const GenericPair<uint32_t, uint32_t>& x) noexcept {
// CHECK-NEXT:  return _impl::$s8Generics16takeConcretePairyyAA07GenericD0Vys6UInt32VAFGF(_impl::swift_interop_passDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(_impl::_impl_GenericPair<uint32_t, uint32_t>::getOpaquePointer(x)));
// CHECK-NEXT: }

// CHECK: template<class T, class T1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T> && swift::isUsableInGenericContext<T1>
// CHECK-NEXT: inline void takeGenericPair(const GenericPair<T, T1>& x) noexcept {
// CHECK-NEXT:  return _impl::$s8Generics15takeGenericPairyyAA0cD0Vyxq_Gr0_lF(_impl::_impl_GenericPair<T, T1>::getOpaquePointer(x), swift::getTypeMetadata<T>(), swift::getTypeMetadata<T1>());
// CHECK-NEXT:}
