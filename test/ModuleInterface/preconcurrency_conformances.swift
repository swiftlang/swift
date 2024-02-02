// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/A.swiftmodule \
// RUN:   -emit-module-interface-path %t/A.swiftinterface

// Build the client and check the interface
// RUN: %target-swift-frontend -emit-module %t/src/Client.swift \
// RUN:   -module-name Client -I %t -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/Client.swiftmodule \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -enable-experimental-feature DynamicActorIsolation \
// RUN:   -disable-availability-checking \
// RUN:   -verify

// RUN: %FileCheck %s < %t/Client.swiftinterface

// RUN: %target-swift-emit-module-interface(%t/Client.swiftinterface) -I %t %t/src/Client.swift -module-name Client \
// RUN:   -disable-availability-checking -enable-experimental-feature DynamicActorIsolation -verify

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t -module-name Client \
// RUN:   -disable-availability-checking -enable-experimental-feature DynamicActorIsolation -verify

// REQUIRES: asserts
// REQUIRES: concurrency

//--- A.swift
public protocol P {
  func test() -> Int
}

public protocol Q {
  var x: Int { get }
}

public protocol WithAssoc {
  associatedtype T
  func test() -> T
}

//--- Client.swift
import A

// CHECK: #if {{.*}} $DynamicActorIsolation
// CHECK-NEXT: @_Concurrency.MainActor public struct GlobalActorTest : @preconcurrency A.P

@MainActor
public struct GlobalActorTest : @preconcurrency P {
  public func test() -> Int { 0 }
}

@MainActor
public class ExtTest {
}

// CHECK: #if {{.*}} $DynamicActorIsolation
// CHECK-NEXT: extension Client.ExtTest : @preconcurrency A.P
extension ExtTest : @preconcurrency P {
  public func test() -> Int { 1 }
}

// CHECK: #if {{.*}} && $DynamicActorIsolation
// CHECK-NEXT: public actor ActorTest : @preconcurrency A.P
public actor ActorTest : @preconcurrency P {
  public func test() -> Int { 2 }
}

public actor ActorExtTest {
}

// CHECK: #if {{.*}} $DynamicActorIsolation
// CHECK-NEXT: extension Client.ActorExtTest : @preconcurrency A.Q
extension ActorExtTest : @preconcurrency Q {
  public var x: Int { 42 }
}

public struct TestConditional<T> {}

// CHECK: #if {{.*}} $DynamicActorIsolation
// CHECK-NEXT: extension Client.TestConditional : @preconcurrency A.WithAssoc where T == Swift.Int {
// CHECK-NEXT:  @_Concurrency.MainActor public func test() -> T
// CHECK-NEXT: }
extension TestConditional : @preconcurrency WithAssoc where T == Int {
  @MainActor public func test() -> T { 42 } // Ok
}

// CHECK: #if {{.*}} $DynamicActorIsolation
// CHECK-NEXT: extension Client.GlobalActorTest : Swift.Sendable {}
// CHECK-NEXT: #endif
