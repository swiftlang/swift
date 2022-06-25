// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)

public enum Large {
    case first(Int64, Int64, Int64, Int64, Int64, Int64)
    case second
}

public func makeLarge(_ x: Int) -> Large {
    return x >= 0 ? .first(0, 1, 2, 3, 4, 5) : .second
}

public func printLarge(_ en: Large) {
    switch en {
    case let .first(a, b, c, d, e, f):
        let x = (a, b, c, d, e, f)
        print("Large.first\(x)")
    case .second:
        print("Large.second")
    }
}

public func passThroughLarge(_ en: Large) -> Large {
    return en
}

public func inoutLarge(_ en: inout Large, _ x: Int) {
    if x >= 0 {
        en = .first(-1, -2, -3, -4, -5, -6)
    } else {
        en = .second
    }
}

// CHECK: SWIFT_EXTERN void $s5Enums10inoutLargeyyAA0C0Oz_SitF(void * _Nonnull en, ptrdiff_t x) SWIFT_NOEXCEPT SWIFT_CALL; // inoutLarge(_:_:)
// CHECK: SWIFT_EXTERN void $s5Enums9makeLargeyAA0C0OSiF(SWIFT_INDIRECT_RESULT void * _Nonnull, ptrdiff_t x) SWIFT_NOEXCEPT SWIFT_CALL; // makeLarge(_:)
// CHECK: SWIFT_EXTERN void $s5Enums16passThroughLargeyAA0D0OADF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull en) SWIFT_NOEXCEPT SWIFT_CALL; // passThroughLarge(_:)
// CHECK: SWIFT_EXTERN void $s5Enums10printLargeyyAA0C0OF(const void * _Nonnull en) SWIFT_NOEXCEPT SWIFT_CALL; // printLarge(_:)
// CHECK: class Large final {

// CHECK:      inline void inoutLarge(Large& en, swift::Int x) noexcept {
// CHECK-NEXT:   return _impl::$s5Enums10inoutLargeyyAA0C0Oz_SitF(_impl::_impl_Large::getOpaquePointer(en), x);
// CHECK-NEXT: }

// CHECK:      inline Large makeLarge(swift::Int x) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_Large::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s5Enums9makeLargeyAA0C0OSiF(result, x);
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK:      inline Large passThroughLarge(const Large& en) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_Large::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s5Enums16passThroughLargeyAA0D0OADF(result, _impl::_impl_Large::getOpaquePointer(en));
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK:      inline void printLarge(const Large& en) noexcept {
// CHECK-NEXT:   return _impl::$s5Enums10printLargeyyAA0C0OF(_impl::_impl_Large::getOpaquePointer(en));
// CHECK-NEXT: }
