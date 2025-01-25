// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN: -target %target-swift-5.1-abi-triple -verify \
// RUN: -module-name A -swift-version 6 \
// RUN: -emit-module-path %t/A.swiftmodule

// Build the client
// RUN: %target-swift-frontend -emit-module %t/src/Client.swift \
// RUN: -target %target-swift-5.1-abi-triple -verify \
// RUN: -module-name Client -I %t -swift-version 6 \
// RUN: -emit-module-path %t/Client.swiftmodule

// REQUIRES: concurrency

//--- A.swift
@MainActor
public protocol P {}

@frozen
public struct ImplicitlySendable {
  nonisolated public var prop: Bool = true

  nonisolated public init() {}
}

public struct S: P {
  nonisolated public var x: Int = 0
  public var y: Int = 1

  nonisolated public init() {}
}

//--- Client.swift
import A

actor A {
  func test() {
    var s = S()
    s.x += 0 // okay
    // expected-error@+1 {{main actor-isolated property 'y' can not be mutated on a nonisolated actor instance}}
    s.y += 1
    var sendable = ImplicitlySendable()
    sendable.prop = false // okay
  }
}
