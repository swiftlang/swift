// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/generic-struct-in-cxx.swift  -D KNOWN_LAYOUT -D INDIRECT_KNOWN_LAYOUT -module-name Generics -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-interop-cxx-header-in-clang(%t/generics.h -Wno-reserved-identifier -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/generic-struct-in-cxx.swift -D KNOWN_LAYOUT -D INDIRECT_KNOWN_LAYOUT -enable-library-evolution -module-name Generics -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-interop-cxx-header-in-clang(%t/generics.h -Wno-reserved-identifier -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// CHECK: SWIFT_EXTERN void $s8Generics11GenericPairV1yq_vg(SWIFT_INDIRECT_RESULT void * _Nonnull, void * _Nonnull , SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // _
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV1yq_vs(const void * _Nonnull newValue, void * _Nonnull , SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // _
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairVyACyxq_Gx_Siq_tcfC(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, ptrdiff_t i, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // init(_:_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV6methodyyF(void * _Nonnull , SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // method()
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV14mutatingMethodyyACyq_xGF(const void * _Nonnull other, void * _Nonnull , SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // mutatingMethod(_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV13genericMethodyqd__qd___q_tlF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull , SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // genericMethod(_:_:)


// CHECK: SWIFT_EXTERN void $s8Generics17inoutConcretePairyys6UInt16V_AA07GenericD0VyA2DGztF(uint16_t x, void * _Nonnull y) SWIFT_NOEXCEPT SWIFT_CALL; // inoutConcretePair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics16inoutGenericPairyyAA0cD0Vyxq_Gz_xtr0_lF(void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // inoutGenericPair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics16makeConcretePairyAA07GenericD0Vys6UInt16VAFGAF_AFtF(SWIFT_INDIRECT_RESULT void * _Nonnull, uint16_t x, uint16_t y) SWIFT_NOEXCEPT SWIFT_CALL; // makeConcretePair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics15makeGenericPairyAA0cD0Vyxq_Gx_q_tr0_lF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // makeGenericPair(_:_:)
