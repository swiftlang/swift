// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -language-mode 6 -typecheck -module-name Test -emit-module-interface-path %t/Test.swiftinterface %s
// RUN: %FileCheck %s < %t/Test.swiftinterface --check-prefix CHECK --check-prefix NONRESILIENT
// RUN: %target-swift-frontend -language-mode 6 -compile-module-from-interface %t/Test.swiftinterface -o %t/Test.swiftmodule
// RUN: %target-swift-frontend -language-mode 6 -emit-module -o /dev/null -merge-modules -emit-module-interface-path - %t/Test.swiftmodule -module-name Test | %FileCheck %s --check-prefix CHECK --check-prefix NONRESILIENT

// RUN: %target-swift-frontend -language-mode 6 -typecheck -module-name TestResilient -emit-module-interface-path %t/TestResilient.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface

// RUN: %target-swift-frontend -language-mode 6 -compile-module-from-interface %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule
// RUN: %target-swift-frontend -language-mode 6 -emit-module -o /dev/null -merge-modules -emit-module-interface-path - %t/TestResilient.swiftmodule -module-name TestResilient | %FileCheck %s

// REQUIRE: concurrency

// CHECK: public struct HasMainActorLazyVars {
// CHECK-NEXT: @_Concurrency::MainActor public var foo: Swift::Int {
// CHECK-NEXT:   mutating get
// CHECK-NEXT:   set
// CHECK-NEXT: }
// NONRESILIENT: @_Concurrency::MainActor @_hasInitialValue private var $__lazy_storage_$_foo: Swift::Int?
// CHECK-NOT: private var bar
// NONRESILIENT: @_Concurrency::MainActor @_hasInitialValue private var $__lazy_storage_$_bar: Swift::Int?
// CHECK-NEXT: }
public struct HasMainActorLazyVars {
  @MainActor public lazy var foo: Int = 0
  @MainActor private lazy var bar: Int = 0
}


// CHECK: public struct PreconcurrencyHasMainActorLazyVars {
// CHECK-NEXT: {{(@preconcurrency @_Concurrency::MainActor|@_Concurrency::MainActor @preconcurrency)}} public var foo: Swift::Int {
// CHECK-NEXT:   mutating get
// CHECK-NEXT:   set
// CHECK-NEXT: }
// NONRESILIENT: @_Concurrency::MainActor @preconcurrency @_hasInitialValue private var $__lazy_storage_$_foo: Swift::Int?
// CHECK-NOT: private var bar
// NONRESILIENT: @_Concurrency::MainActor @preconcurrency @_hasInitialValue private var $__lazy_storage_$_bar: Swift::Int?
// CHECK-NEXT: }
public struct PreconcurrencyHasMainActorLazyVars {
  @preconcurrency @MainActor public lazy var foo: Int = 0
  @preconcurrency @MainActor private lazy var bar: Int = 0
}


// CHECK: @_Concurrency::MainActor public struct MainActorHasLazyVars {
// CHECK-NEXT: @_Concurrency::MainActor public var foo: Swift::Int {
// CHECK-NEXT:   mutating get
// CHECK-NEXT:   set
// CHECK-NEXT: }
// NONRESILIENT: @_Concurrency::MainActor @_hasInitialValue private var $__lazy_storage_$_foo: Swift::Int?
// CHECK-NOT: private var bar
// NONRESILIENT: @_Concurrency::MainActor @_hasInitialValue private var $__lazy_storage_$_bar: Swift::Int?
// CHECK-NEXT: }
@MainActor
public struct MainActorHasLazyVars {
  public lazy var foo: Int = 0
  private lazy var bar: Int = 0
}


// CHECK: {{(@preconcurrency @_Concurrency::MainActor|@_Concurrency::MainActor @preconcurrency)}} public struct PreconcurrencyMainActorHasLazyVars {
// CHECK-NEXT: {{(@preconcurrency @_Concurrency::MainActor|@_Concurrency::MainActor @preconcurrency)}} public var foo: Swift::Int {
// CHECK-NEXT:   mutating get
// CHECK-NEXT:   set
// CHECK-NEXT: }
// NONRESILIENT: @_Concurrency::MainActor @preconcurrency @_hasInitialValue private var $__lazy_storage_$_foo: Swift::Int?
// CHECK-NOT: private var bar
// NONRESILIENT: @_Concurrency::MainActor @preconcurrency @_hasInitialValue private var $__lazy_storage_$_bar: Swift::Int?
// CHECK-NEXT: }
@preconcurrency @MainActor
public struct PreconcurrencyMainActorHasLazyVars {
  public lazy var foo: Int = 0
  private lazy var bar: Int = 0
}

