// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -enable-library-evolution -parse-as-library -emit-module-interface-path - -module-name MyFile -swift-version 6 | %FileCheck %s
// RUN: %target-swift-frontend %s -typecheck -enable-library-evolution -parse-as-library -emit-module-interface-path - -module-name MyFile -swift-version 6 -module-interface-preserve-types-as-written | %FileCheck -check-prefix=CHECK-REPR %s

// The force printing type reprs option is only available in asserts builds.
// REQUIRES: asserts

// This test validates that when we produce interface files we produce the
// correct interface file for sending when printing normally or with type reprs
// enabled.

public class NonSendableKlass {}

// CHECK: public func test() -> sending MyFile.NonSendableKlass

// CHECK-REPR: public func test() -> sending NonSendableKlass
public func test() -> sending NonSendableKlass { NonSendableKlass() }

// CHECK: public func test2(_ x: sending MyFile.NonSendableKlass)

// CHECK-REPR: public func test2(_ x: sending NonSendableKlass)
public func test2(_ x: sending NonSendableKlass) {}

// CHECK: @_Concurrency.MainActor public var closure: () -> sending MyFile.NonSendableKlass

// CHECK-REPR: @_Concurrency.MainActor public var closure: () -> sending NonSendableKlass
@MainActor public var closure: () -> sending NonSendableKlass = { NonSendableKlass() }
