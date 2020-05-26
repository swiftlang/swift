// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -module-name Test -emit-module-interface-path %t/Test.swiftinterface %s
// RUN: %FileCheck %s < %t/Test.swiftinterface --check-prefix CHECK --check-prefix NONRESILIENT
// RUN: %target-swift-frontend -compile-module-from-interface %t/Test.swiftinterface -o %t/Test.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -emit-module-interface-path - %t/Test.swiftmodule -module-name Test | %FileCheck %s --check-prefix CHECK --check-prefix NONRESILIENT

// RUN: %target-swift-frontend -typecheck -module-name TestResilient -emit-module-interface-path %t/TestResilient.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface --check-prefix CHECK --check-prefix RESILIENT

// RUN: %target-swift-frontend -compile-module-from-interface %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -emit-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s --check-prefix CHECK --check-prefix RESILIENT

// CHECK: @frozen public struct HasLazyVarsFixedLayout {
// CHECK-NEXT: public var foo: Swift.Int {
// CHECK-NEXT:   mutating get
// CHECK-NEXT:   set
// CHECK-NEXT: }
// CHECK: private var $__lazy_storage_$_foo: Swift.Int?
// CHECK-NOT: private var bar
// CHECK: private var $__lazy_storage_$_bar: Swift.Int?
// CHECK-NEXT: }
@frozen
public struct HasLazyVarsFixedLayout {
  public lazy var foo: Int = 0
  private lazy var bar: Int = 0
}

// CHECK: public struct HasLazyVars {
// CHECK-NEXT: public var foo: Swift.Int {
// CHECK-NEXT:   mutating get
// CHECK-NEXT:   set
// CHECK-NEXT: }
// NONRESILIENT: private var $__lazy_storage_$_foo: Swift.Int?
// CHECK-NOT: private var bar
// NONRESILIENT: private var $__lazy_storage_$_bar: Swift.Int?
// CHECK-NEXT: }
public struct HasLazyVars {
  public lazy var foo: Int = 0
  private lazy var bar: Int = 0
}
