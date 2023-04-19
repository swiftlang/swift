// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: asserts

//--- Lib.swift

// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

// RUN: cat %t/Lib.swiftinterface | %FileCheck %t/Lib.swift

public protocol PublicProtocol : AnyObject {}
// CHECK: public protocol PublicProtocol : AnyObject

protocol InternalProtocol: PublicProtocol {}
// CHECK-NOT: InternalProtocol

public class IndirectConformant {
    public init() {}
}
extension IndirectConformant: InternalProtocol {}
// CHECK: extension Lib.IndirectConformant : Lib.PublicProtocol {}

//--- Client.swift

/// Works without safety.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t

/// Works with safety.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -enable-deserialization-safety \
// RUN:   -Xllvm -debug-only=Serialization 2>&1 \
// RUN:   | %FileCheck %t/Client.swift

/// Works with swiftinterface.
// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t

import Lib

func requireConformanceToPublicProtocol(_ a: PublicProtocol) {}
requireConformanceToPublicProtocol(IndirectConformant())

/// Deserialization safety should keep the original chain. We're mostly
/// documenting the current safety implementation details here, if we can get
/// without deserializing 'InternalProtocol' it would be even better.
// CHECK: Deserialized: 'IndirectConformant'
// CHECK: Deserialized: 'PublicProtocol'
// CHECK: Deserialized: 'InternalProtocol'
