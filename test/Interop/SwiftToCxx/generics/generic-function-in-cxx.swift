// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-generic-interop-cxx-header-in-clang(%t/functions.h)


public func genericPrintFunctionTwoArg<T>(_ x: T, _ y: Int) {
    print("X:", x)
    print("Y:", y)
}

public func genericPrintFunction<T>(_ x: T) {
    print("\(T.self) value=\(x)")
}

public func genericPrintFunctionMultiGeneric<T1, T2>(_ x: Int, _ t1: T1, _ t1p: T1, _ y: Int, _ t2: T2) {
    print("\(T1.self) value 1=\(t1)")
    print("\(T1.self) value 2=\(t1p)")
    print("\(T2.self) value 1=\(t2)")
    print("other values=\(x),\(y)")
}

// CHECK: SWIFT_EXTERN void $s9Functions20genericPrintFunctionyyxlF(const void * _Nonnull x, void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // genericPrintFunction(_:)
// CHECK-NEXT: SWIFT_EXTERN void $s9Functions32genericPrintFunctionMultiGenericyySi_xxSiq_tr0_lF(ptrdiff_t x, const void * _Nonnull t1, const void * _Nonnull t1p, ptrdiff_t y, const void * _Nonnull t2, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // genericPrintFunctionMultiGeneric(_:_:_:_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s9Functions26genericPrintFunctionTwoArgyyx_SitlF(const void * _Nonnull x, ptrdiff_t y, void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // genericPrintFunctionTwoArg(_:_:)

// CHECK:      template<class T>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T>
// CHECK-NEXT: inline void genericPrintFunction(const T & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions20genericPrintFunctionyyxlF(reinterpret_cast<const void *>(&x), swift::getTypeMetadata<T>());
// CHECK-NEXT: }


// CHECK:      template<class T1, class T2>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T1> && swift::isUsableInGenericContext<T2>
// CHECK-NEXT: inline void genericPrintFunctionMultiGeneric(swift::Int x, const T1 & t1, const T1 & t1p, swift::Int y, const T2 & t2) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions32genericPrintFunctionMultiGenericyySi_xxSiq_tr0_lF(x, reinterpret_cast<const void *>(&t1), reinterpret_cast<const void *>(&t1p), y, reinterpret_cast<const void *>(&t2), swift::getTypeMetadata<T1>(), swift::getTypeMetadata<T2>());
// CHECK-NEXT: }


// CHECK:      template<class T>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T>
// CHECK-NEXT: inline void genericPrintFunctionTwoArg(const T & x, swift::Int y) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions26genericPrintFunctionTwoArgyyx_SitlF(reinterpret_cast<const void *>(&x), y, swift::getTypeMetadata<T>());
// CHECK-NEXT: }
