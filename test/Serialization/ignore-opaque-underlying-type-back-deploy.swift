/// Variant of ignore-opaque-underlying-type because of the macOS host need.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: asserts
// REQUIRES: OS=macosx

/// Resilient scenario, we ignore underlying type of non-inlinable functions.
/// Build libraries.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface -verify

/// Build clients, with and without safety.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -enable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck %s

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck %s

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-access-control 2>&1 \
// RUN:   | %FileCheck %s

/// Build against the swiftinterface.
// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck %s

/// Non-resilient scenario, all underlying types are loaded.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule -verify
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck --check-prefix=NON-RESILIENT %s
// NON-RESILIENT-NOT: Ignoring underlying information

//--- Lib.swift
public protocol V {}

public struct EV : V {
    public init () {}
}

@available(SwiftStdlib 5.1, *)
public extension V {
// CHECK: Loading underlying information for opaque type of 'backdeployedOpaqueFunc()'
  @backDeployed(before: SwiftStdlib 5.1) // expected-warning 4 {{'@backDeployed' cannot be applied to instance method 'backdeployedOpaqueFunc()' because it has a 'some' return type}}
  func backdeployedOpaqueFunc() -> some V { EV() }
}

//--- Client.swift
import Lib

if #available(SwiftStdlib 5.1, *) {
  let v = EV()
  let _ = v.backdeployedOpaqueFunc()
}
