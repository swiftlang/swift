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
// CHECK-NEXT:    extern unsigned $s5Enums3FooO1ayACSdcACmFWC;
// NEW_CASE-NEXT: extern unsigned $s5Enums3FooO1byACSicACmFWC;
// CHECK-NEXT:    }
// CHECK-EMPTY:
// CHECK-NEXT:    } // namespace _impl
// CHECK-EMPTY:
// CHECK-NEXT:    class Foo final {
// CHECK-NEXT:    public:
// CHECK:         enum class cases {
// CHECK-NEXT:      a,
// NEW_CASE-NEXT:   b,
// CHECK-NEXT:      unknownDefault
// CHECK-NEXT:    }
// CHECK:         static struct {  // impl struct for case unknownDefault
// CHECK-NEXT:      constexpr operator cases() const {
// CHECK-NEXT:        return cases::unknownDefault;
// CHECK-NEXT:      }
// CHECK-NEXT:    } unknownDefault;
// CHECK-NEXT:    inline bool isUnknownDefault() const {
// CHECK-NEXT:      return *this == Foo::unknownDefault;
// CHECK-NEXT:    }
// CHECK:         inline operator cases() const {
// CHECK-NEXT:      auto tag = _getEnumTag();
// CHECK-NEXT:      if (tag == _impl::$s5Enums3FooO1ayACSdcACmFWC) return cases::a;
// NEW_CASE-NEXT:   if (tag == _impl::$s5Enums3FooO1byACSicACmFWC) return cases::b;
// CHECK-NEXT:      return cases::unknownDefault;
// CHECK-NEXT:    }
