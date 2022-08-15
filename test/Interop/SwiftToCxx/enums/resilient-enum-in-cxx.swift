// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -enable-library-evolution -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h
// RUN: %FileCheck --check-prefixes=CHECK,OLD_CASE %s < %t/enums.h

// RUN: %target-swift-frontend %s -enable-library-evolution -D NEW_CASE -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums_new_case.h
// RUN: %FileCheck --check-prefixes=CHECK,NEW_CASE %s < %t/enums_new_case.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)
// RUN: %check-interop-cxx-header-in-clang(%t/enums_new_case.h -Wno-unused-private-field -Wno-unused-function)

public enum Foo {
    case a(Double)
#if NEW_CASE
    case b(Int)
#endif
}

public func makeFoo(_ x: Int) -> Foo {
#if NEW_CASE
    if x >= 0 {
        return .b(x)
    } else {
        return .a(Double(x))
    }
#else
    return .a(Double(x))
#endif
}

public func printFoo(_ x: Foo) {
    print(x)
}

// CHECK:         // Tags for resilient enum Foo
// CHECK-NEXT:    extern "C" {
// CHECK-NEXT:    extern int $s5Enums3FooO1ayACSdcACmFWC;
// NEW_CASE-NEXT: extern int $s5Enums3FooO1byACSicACmFWC;
// CHECK-NEXT:    }
// CHECK-EMPTY:
// CHECK-NEXT:    } // namespace _impl
// CHECK-EMPTY:
// CHECK-NEXT:    class Foo final {
// CHECK-NEXT:    public:
// CHECK:         inline operator cases() const {
// CHECK-NEXT:      auto tag = _getEnumTag();
// CHECK-NEXT:      if (tag == _impl::$s5Enums3FooO1ayACSdcACmFWC) return cases::a;
// NEW_CASE-NEXT:   if (tag == _impl::$s5Enums3FooO1byACSicACmFWC) return cases::b;
// CHECK-NEXT:      abort();
// CHECK-NEXT:    }
// CHECK-NEXT:    inline bool inResilientUnknownCase() const {
// CHECK-NEXT:      auto tag = _getEnumTag();
// CHECK-NEXT:      return
// OLD_CASE-NEXT:     tag != _impl::$s5Enums3FooO1ayACSdcACmFWC;
// NEW_CASE-NEXT:     tag != _impl::$s5Enums3FooO1ayACSdcACmFWC &&
// NEW_CASE-NEXT:     tag != _impl::$s5Enums3FooO1byACSicACmFWC;
// CHECK-NEXT:    }
// CHECK-NEXT:    inline bool isA() const {
// CHECK-NEXT:      return _getEnumTag() == _impl::$s5Enums3FooO1ayACSdcACmFWC;
// CHECK-NEXT:    }
// NEW_CASE:      inline bool isB() const {
// NEW_CASE-NEXT:   return _getEnumTag() == _impl::$s5Enums3FooO1byACSicACmFWC;
// NEW_CASE-NEXT: }
