// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/name-collision.h
// RUN: %FileCheck %s < %t/name-collision.h

public class C0 {
  public static var description: String { "hello" }
  public var description: String { "world" }
}

public class C1 {
  public var feat: Int { 123 }
  public var Feat: Int { 123 }
}

public class C2 {
    public var item: Int { 42 }

    public func getItem() -> Int {
        return item + 1
    }
}

public class C3 {
    public func getValue() -> Int {
        return 0
    }

    public var value: Int { 42 }
}

public class C4 {
    public var item: Int { 42 }

    public func getItem() -> String {
        return "hello"
    }
}

public struct S0 {
    public var x: Int
    public var count: Int { x }

    public func getCount(_ offset: Int) -> Int {
        return count + offset
    }
}

// CHECK: class SWIFT_SYMBOL("s:4main2C0C") C0 :
// CHECK-NOT: getDescription()
// CHECK: static SWIFT_INLINE_THUNK swift::String getDescription()
// CHECK: skip emitting accessor method for 'description'. 'getDescription' already declared.
// CHECK-NOT: getDescription()
// CHECK:};

// CHECK: class SWIFT_SYMBOL("s:4main2C1C") C1 :
// CHECK-NOT: getFeat()
// CHECK:   SWIFT_INLINE_THUNK swift::Int getFeat()
// CHECK:   skip emitting accessor method for 'Feat'. 'getFeat' already declared.
// CHECK-NOT: getFeat()
// CHECK:};

// CHECK: class SWIFT_SYMBOL("s:4main2C2C") C2 :
// CHECK:   SWIFT_INLINE_THUNK swift::Int getItem()
// CHECK-NOT: getItem()
// CHECK:};

// CHECK: class SWIFT_SYMBOL("s:4main2C3C") C3 :
// CHECK:   SWIFT_INLINE_THUNK swift::Int getValue()
// CHECK-NOT: getValue()
// CHECK:};

// CHECK: class SWIFT_SYMBOL("s:4main2C4C") C4 :
// CHECK:   SWIFT_INLINE_THUNK swift::Int getItem()
// CHECK-NOT: getItem()
// CHECK:};

// CHECK: class SWIFT_SYMBOL("s:4main2S0V") S0 final {
// CHECK:   SWIFT_INLINE_THUNK swift::Int getCount() const
// CHECK:   SWIFT_INLINE_THUNK swift::Int getCount(swift::Int offset) const
// CHECK:};

// Out-of-line definitions section.
// CHECK: SWIFT_INLINE_THUNK swift::String C0::getDescription() {
// CHECK: skip emitting accessor method for 'description'. 'getDescription' already declared.
// CHECK-NOT: getDescription()
// CHECK-NOT: getFeat()
// CHECK: SWIFT_INLINE_THUNK swift::Int C1::getFeat() {
// CHECK: skip emitting accessor method for 'Feat'. 'getFeat' already declared.
// CHECK-NOT: getFeat()
