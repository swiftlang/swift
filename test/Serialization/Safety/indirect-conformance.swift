// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: asserts

//--- Lib.swift

// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

// RUN: cat %t/Lib.swiftinterface | %FileCheck %t/Lib.swift

public protocol PublicProtocol {}
// CHECK: public protocol PublicProtocol

protocol InternalProtocol: PublicProtocol {}
// CHECK-NOT: InternalProtocol

public class IndirectConformant {
    public init() {}
}

extension IndirectConformant: InternalProtocol {}
// CHECK: extension Lib.IndirectConformant : Lib.PublicProtocol {}

extension String: InternalProtocol {}
// CHECK: extension Swift.String : Lib.PublicProtocol {}

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
requireConformanceToPublicProtocol("string")

// CHECK: Deserialized: 'IndirectConformant'
// CHECK: Deserialized: 'PublicProtocol'
// CHECK-NOT: Deserialized: 'InternalProtocol'
