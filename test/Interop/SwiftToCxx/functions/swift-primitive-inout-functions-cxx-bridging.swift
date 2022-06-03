// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK: SWIFT_EXTERN void $s9Functions8inOutIntyySizF(ptrdiff_t * x) SWIFT_NOEXCEPT SWIFT_CALL; // inOutInt(_:)
// CHECK: SWIFT_EXTERN void $s9Functions11inOutTwoIntyySiz_SiztF(ptrdiff_t * x, ptrdiff_t * y) SWIFT_NOEXCEPT SWIFT_CALL; // inOutTwoInt(_:_:)
// CHECK: SWIFT_EXTERN void $s9Functions13inOutTwoParamyySbz_SdztF(bool * x, double * y) SWIFT_NOEXCEPT SWIFT_CALL; // inOutTwoParam(_:_:)

// CHECK:      inline void inOutInt(swift::Int & x) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions8inOutIntyySizF(&x);
// CHECK-NEXT: }

// CHECK:      inline void inOutTwoInt(swift::Int & x, swift::Int & y) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions11inOutTwoIntyySiz_SiztF(&x, &y);
// CHECK-NEXT: }

// CHECK:      inline void inOutTwoParam(bool & x, double & y) noexcept {
// CHECK-NEXT:   return _impl::$s9Functions13inOutTwoParamyySbz_SdztF(&x, &y);
// CHECK-NEXT: }

public func inOutInt(_ x: inout Int) { x = Int() }

public func inOutTwoInt(_ x: inout Int, _ y: inout Int) {
    x += y
    y -= 2 * x
}

public func inOutTwoParam(_ x: inout Bool, _ y: inout Double) {
    y = 3.14
    x = !x
}
