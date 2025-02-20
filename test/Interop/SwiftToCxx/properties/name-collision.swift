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

// CHECK: namespace main SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("main") {
// CHECK-NOT: getDescription()
// CHECK: SWIFT_INLINE_THUNK swift::String C0::getDescription() {
// CHECK: skip emitting accessor method for 'description'. 'getDescription' already declared.
// CHECK-NOT: getDescription()
// CHECK-NOT: getFeat()
// CHECK: SWIFT_INLINE_THUNK swift::Int C1::getFeat() {
// CHECK: skip emitting accessor method for 'Feat'. 'getFeat' already declared.
// CHECK-NOT: getFeat()
