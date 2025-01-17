// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -enable-library-evolution -parse-as-library -emit-module-interface-path - -module-name MyFile -swift-version 6 | %FileCheck %s
// RUN: %target-swift-frontend %s -typecheck -enable-library-evolution -parse-as-library -emit-module-interface-path - -module-name MyFile -swift-version 6 -module-interface-preserve-types-as-written | %FileCheck -check-prefix=CHECK-REPR %s

// The force printing type reprs option is only available in asserts builds.
// REQUIRES: asserts

// This test validates that when we produce interface files we produce the
// correct interface file for sending when printing normally or with type reprs
// enabled.

public class NonSendableKlass {}

// CHECK: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: public func test() -> sending MyFile.NonSendableKlass
// CHECK-NEXT: #else
// CHECK-NEXT: public func test() -> MyFile.NonSendableKlass
// CHECK-NEXT: #endif

// CHECK-REPR: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-REPR-NEXT: public func test() -> sending NonSendableKlass
// CHECK-REPR-NEXT: #else
// CHECK-REPR-NEXT: public func test() -> NonSendableKlass
// CHECK-REPR-NEXT: #endif
public func test() -> sending NonSendableKlass { NonSendableKlass() }

// CHECK: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: public func test2(_ x: sending MyFile.NonSendableKlass)
// CHECK-NEXT: #else
// CHECK-NEXT: public func test2(_ x: __owned MyFile.NonSendableKlass)
// CHECK-NEXT: #endif

// CHECK-REPR: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-REPR-NEXT: public func test2(_ x: sending NonSendableKlass)
// CHECK-REPR-NEXT: #else
// CHECK-REPR-NEXT: public func test2(_ x: __owned NonSendableKlass)
// CHECK-REPR-NEXT: #endif
public func test2(_ x: sending NonSendableKlass) {}

// CHECK: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-NEXT: @_Concurrency.MainActor public var closure: () -> sending MyFile.NonSendableKlass
// CHECK-NEXT: #else
// CHECK-NEXT: @_Concurrency.MainActor public var closure: () -> MyFile.NonSendableKlass
// CHECK-NEXT: #endif

// CHECK-REPR: #if compiler(>=5.3) && $SendingArgsAndResults
// CHECK-REPR-NEXT: @_Concurrency.MainActor public var closure: () -> sending NonSendableKlass
// CHECK-REPR-NEXT: #else
// CHECK-REPR-NEXT: @_Concurrency.MainActor public var closure: () -> NonSendableKlass
// CHECK-REPR-NEXT: #endif
@MainActor public var closure: () -> sending NonSendableKlass = { NonSendableKlass() }
