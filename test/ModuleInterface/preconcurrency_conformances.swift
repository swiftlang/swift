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
// RUN:   -enable-upcoming-feature DynamicActorIsolation \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -verify

// RUN: %FileCheck %s < %t/Client.swiftinterface

// RUN: %target-swift-emit-module-interface(%t/Client.swiftinterface) -I %t %t/src/Client.swift -module-name Client \
// RUN:   -target %target-swift-5.1-abi-triple -enable-upcoming-feature DynamicActorIsolation -verify

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t -module-name Client \
// RUN:   -target %target-swift-5.1-abi-triple -enable-upcoming-feature DynamicActorIsolation -verify

// REQUIRES: concurrency
// REQUIRES: swift_feature_DynamicActorIsolation

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

// CHECK-NOT: #if {{.*}} $DynamicActorIsolation
// CHECK: @_Concurrency.MainActor public struct GlobalActorTest : @preconcurrency A.P

@MainActor
public struct GlobalActorTest : @preconcurrency P {
  public func test() -> Int { 0 }
}

@MainActor
public class ExtTest {
}

// CHECK-NOT: #if {{.*}} $DynamicActorIsolation
// CHECK: extension Client.ExtTest : @preconcurrency A.P
extension ExtTest : @preconcurrency P {
  public func test() -> Int { 1 }
}

// CHECK-NOT: #if {{.*}} && $DynamicActorIsolation
// CHECK: public actor ActorTest : @preconcurrency A.P
public actor ActorTest : @preconcurrency P {
  public func test() -> Int { 2 }
}

public actor ActorExtTest {
}

// CHECK-NOT: #if {{.*}} $DynamicActorIsolation
// CHECK: extension Client.ActorExtTest : @preconcurrency A.Q
extension ActorExtTest : @preconcurrency Q {
  public var x: Int { 42 }
}

public struct TestConditional<T> {}

// CHECK-NOT: #if {{.*}} $DynamicActorIsolation
// CHECK: extension Client.TestConditional : @preconcurrency A.WithAssoc where T == Swift.Int {
// CHECK-NEXT:  @_Concurrency.MainActor public func test() -> T
// CHECK-NEXT: }
extension TestConditional : @preconcurrency WithAssoc where T == Int {
  @MainActor public func test() -> T { 42 } // Ok
}

// CHECK-NOT: #if {{.*}} $DynamicActorIsolation
// CHECK: extension Client.GlobalActorTest : Swift.Sendable {}
