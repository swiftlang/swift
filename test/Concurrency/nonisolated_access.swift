// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN: -disable-availability-checking \
// RUN: -module-name A -swift-version 6 \
// RUN: -emit-module-path %t/A.swiftmodule

// Build the client
// RUN: %target-swift-frontend -emit-module %t/src/Client.swift \
// RUN: -disable-availability-checking \
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

  nonisolated public init() {}
}

//--- Client.swift
import A

actor A {
  func test() {
    var s = S()
    s.x += 0 // okay
    var sendable = ImplicitlySendable()
    sendable.prop = false // okay
  }
}
