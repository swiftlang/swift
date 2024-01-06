// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library
// RUN: %target-swift-frontend -emit-module %t/src/PublicModule.swift \
// RUN:   -module-name PublicModule -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/PublicModule.swiftmodule \
// RUN:   -emit-module-interface-path %t/PublicModule.swiftinterface

// Build the client and check the interface
// RUN: %target-swift-frontend -emit-module %t/src/Client.swift \
// RUN:   -module-name Client -I %t -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/Client.swiftmodule \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -enable-experimental-feature PreconcurrencyConformances

// RUN: %FileCheck %s < %t/Client.swiftinterface

// RUN: %target-swift-emit-module-interface(%t/Client.swiftinterface) -I %t %s -module-name Client \
// RUN:   -enable-experimental-feature PreconcurrencyConformances

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t -module-name Client \
// RUN:   -enable-experimental-feature PreconcurrencyConformances

// REQUIRES: asserts
// REQUIRES: concurrency

//--- PublicModule.swift
public protocol P {
  func test() -> Int
}

//--- Client.swift
import PublicModule

// CHECK: #if compiler(>=5.3) && $PreconcurrencyConformances
// CHECK-NEXT: @_Concurrency.MainActor public struct GlobalActorTest : @preconcurrency PublicModule.P
@MainActor
public struct GlobalActorTest : @preconcurrency P {
  public func test() -> Int { 0 }
}

@MainActor
public class ExtTest {
}

// CHECK: #if compiler(>=5.3) && $PreconcurrencyConformances
// CHECK-NEXT: extension Client.ExtTest : @preconcurrency PublicModule.P
extension ExtTest : @preconcurrency P {
  public func test() -> Int { 1 }
}

// CHECK: #if compiler(>=5.3) && $PreconcurrencyConformances
// CHECK-NEXT: extension Client.GlobalActorTest : Swift.Sendable {}
// CHECK-NEXT: #endif

// TODO: 'actor' cannot be tested until @preconcurrency conformances are implemented.
